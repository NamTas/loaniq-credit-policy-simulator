## ============================================================
## Step 1: Synthetic SME loan dataset + SQL database setup
## ============================================================
## No public dataset combines application decisions + rejection reasons +
## post-approval default performance for SME lending, so we SIMULATE a
## realistic dataset with genuine statistical relationships baked in
## (higher DTI -> more rejections, lower credit score -> more defaults,
## etc.) so every downstream test/model produces meaningful, defensible
## results. State clearly in your README that this is a simulated dataset
## built to mirror realistic SME lending patterns -- this is a completely
## legitimate and common approach for a portfolio project.

library(tidyverse)
library(DBI)
library(RSQLite)

dir.create("data", showWarnings = FALSE)
dir.create("models", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

set.seed(42)
n <- 10000

sectors    <- c("Trading", "Manufacturing", "Retail", "Services", "Agriculture", "Textiles")
locations  <- c("Dhaka", "Chattogram", "Sylhet", "Khulna", "Rajshahi", "Barishal")
sector_w   <- c(0.28, 0.20, 0.22, 0.16, 0.08, 0.06)

apps <- tibble(
  Application_ID     = sprintf("APP%05d", 1:n),
  Applicant_ID       = sprintf("SME%05d", 1:n),
  Business_Sector    = sample(sectors, n, replace = TRUE, prob = sector_w),
  Business_Location  = sample(locations, n, replace = TRUE),
  Business_Age       = pmax(0, round(rgamma(n, shape = 2, scale = 3), 1)),
  Number_of_Employees = pmax(1, round(rlnorm(n, meanlog = 2, sdlog = 0.8))),
  Annual_Revenue     = pmax(200000, round(rlnorm(n, meanlog = 14.5, sdlog = 0.7))),
  Application_Date   = sample(seq(as.Date("2024-01-01"), as.Date("2026-08-31"), by = "day"), n, replace = TRUE)
)

apps <- apps %>%
  mutate(
    Monthly_Income    = round(Annual_Revenue / 12 * runif(n, 0.15, 0.35)),
    Monthly_Expenses  = round(Monthly_Income * runif(n, 0.4, 0.85)),
    Cash_Flow         = Monthly_Income - Monthly_Expenses,
    Loan_Amount       = round(pmin(Annual_Revenue * runif(n, 0.15, 0.6), 15000000), -3),
    Loan_Tenure       = sample(c(12, 24, 36, 48, 60), n, replace = TRUE),
    Existing_Debt     = round(pmax(0, rlnorm(n, meanlog = 12, sdlog = 1.1))),
    Collateral_Value  = round(Loan_Amount * runif(n, 0.6, 1.6)),
    Debt_to_Income    = round((Existing_Debt / 12 + Loan_Amount / Loan_Tenure) / pmax(Monthly_Income, 1), 2),
    DSCR              = round(pmax(0.3, Cash_Flow / pmax(Loan_Amount / Loan_Tenure, 1)), 2),
    Credit_History    = round(pmax(0, Business_Age * runif(n, 0.5, 1)), 1),
    Previous_Default  = rbinom(n, 1, plogis(-2 + 1.5 * (Credit_History < 1))),
    Previous_Loan_Count = rpois(n, lambda = pmax(0.2, Credit_History / 2)),
    Previous_Late_Payment = rpois(n, lambda = ifelse(Previous_Default == 1, 2.5, 0.4)),
    ## Credit score built FROM the above (so it's internally consistent,
    ## not an arbitrary random number) -- 300-850 scale like real bureaus.
    Credit_Score = round(pmin(850, pmax(300,
      650 - 60 * Previous_Default - 15 * Previous_Late_Payment +
      10 * pmin(Credit_History, 10) - 40 * pmin(Debt_to_Income, 2) +
      rnorm(n, 0, 40)
    )))
  )

## ---- Approval decision: driven by a realistic logistic combination -------
apps <- apps %>%
  mutate(
    approval_logit = -3 +
      0.010 * (Credit_Score - 600) +
      0.9   * DSCR -
      1.4   * pmin(Debt_to_Income, 3) +
      0.10  * pmin(Business_Age, 10) -
      1.6   * Previous_Default -
      0.25  * Previous_Late_Payment +
      0.35  * (Collateral_Value > Loan_Amount),
    approval_prob = plogis(approval_logit),
    Application_Status = ifelse(runif(n) < approval_prob, "Approved", "Rejected")
  )

## ---- Rejection reason: pick whichever factor was "worst" for that app ----
apps <- apps %>%
  rowwise() %>%
  mutate(
    Rejection_Reason = if (Application_Status == "Approved") NA_character_ else {
      scores <- c(
        "Poor credit history"      = -(Credit_Score - 600) / 100,
        "High debt-to-income"      = Debt_to_Income,
        "Insufficient collateral"  = ifelse(Collateral_Value < Loan_Amount, 2, 0),
        "Insufficient cash flow"   = -DSCR,
        "Previous default record" = Previous_Default * 3,
        "Incomplete documentation" = runif(1, 0, 1.2)   # residual/random category
      )
      names(scores)[which.max(scores)]
    }
  ) %>%
  ungroup() %>%
  mutate(Processing_Time = round(pmax(1, rgamma(n, shape = 3, scale = 2.5)), 1))

## ---- Post-approval performance (Model B target) ---------------------------
## Deliberately uses a PARTLY DIFFERENT set of drivers than approval, so
## approval quality and repayment quality aren't just the same signal
## twice -- "approval != repayment performance" is a real and important
## distinction to demonstrate in this project.
apps <- apps %>%
  mutate(
    default_logit = ifelse(Application_Status == "Approved",
      -2.6 -
        0.007 * (Credit_Score - 600) +
        0.5   * pmin(Debt_to_Income, 3) -
        0.5   * DSCR +
        0.3   * Previous_Late_Payment -
        0.05  * pmin(Business_Age, 10) +
        0.4   * (Number_of_Employees < 3),
      NA
    ),
    default_prob = plogis(default_logit),
    Default_Flag = ifelse(Application_Status == "Approved",
                           ifelse(runif(n) < default_prob, 1, 0), NA),
    Days_Past_Due = ifelse(Application_Status == "Approved",
                            ifelse(Default_Flag == 1, round(rgamma(n, shape = 2, scale = 40)), 0),
                            NA),
    Repayment_Status = case_when(
      Application_Status != "Approved" ~ NA_character_,
      Default_Flag == 1 ~ "Default",
      Days_Past_Due > 0 ~ "Late",
      TRUE ~ "Current"
    )
  ) %>%
  select(-approval_logit, -approval_prob, -default_logit, -default_prob)

cat(sprintf("Approval rate: %.1f%%\n", mean(apps$Application_Status == "Approved") * 100))
cat(sprintf("Default rate among approved: %.1f%%\n",
            mean(apps$Default_Flag[apps$Application_Status == "Approved"], na.rm = TRUE) * 100))

## ---- Save flat file for the R scripts --------------------------------------
saveRDS(apps, "data/sme_loans.rds")
write_csv(apps, "data/sme_loans.csv")

## ---- Build a NORMALIZED SQL database (this is what makes the SQL section
## of your portfolio credible -- a single flat CSV loaded into one table is
## not what "SQL skills" means to a hiring manager; several related tables
## joined together is) ---------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), "sme_loans.db")

applicants <- apps %>%
  select(Applicant_ID, Business_Sector, Business_Location, Business_Age,
         Number_of_Employees, Annual_Revenue)

financials <- apps %>%
  select(Application_ID, Applicant_ID, Loan_Amount, Loan_Tenure, Existing_Debt,
         Collateral_Value, Debt_to_Income, DSCR, Cash_Flow,
         Monthly_Income, Monthly_Expenses)

credit_history <- apps %>%
  select(Application_ID, Applicant_ID, Credit_History, Previous_Default,
         Previous_Loan_Count, Previous_Late_Payment, Credit_Score)

applications <- apps %>%
  select(Application_ID, Applicant_ID, Application_Date, Application_Status,
         Rejection_Reason, Processing_Time)

loan_outcomes <- apps %>%
  filter(Application_Status == "Approved") %>%
  select(Application_ID, Default_Flag, Days_Past_Due, Repayment_Status)

dbWriteTable(con, "applicants", applicants, overwrite = TRUE)
dbWriteTable(con, "financials", financials, overwrite = TRUE)
dbWriteTable(con, "credit_history", credit_history, overwrite = TRUE)
dbWriteTable(con, "applications", applications, overwrite = TRUE)
dbWriteTable(con, "loan_outcomes", loan_outcomes, overwrite = TRUE)

## Sanity-check one join-based query
check <- dbGetQuery(con, "
  SELECT a.Business_Sector,
         COUNT(*) AS total_applications,
         ROUND(AVG(CASE WHEN ap.Application_Status='Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
  FROM applications ap
  JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
  GROUP BY a.Business_Sector
  ORDER BY approval_rate_pct DESC
")
print(check)

dbDisconnect(con)
cat("\nStep 1 complete. sme_loans.db (5 normalized tables) + data/sme_loans.rds saved.\n")
