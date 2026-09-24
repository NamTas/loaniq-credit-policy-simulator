## ============================================================
## Step 2: Module 1 (Application Analytics) + Module 2 (Rejection Analysis)
## ============================================================

library(tidyverse)

apps <- readRDS("data/sme_loans.rds")
dir.create("plots", showWarnings = FALSE)

## ---- MODULE 1: Application-level descriptive analytics ---------------------
overview <- tibble(
  Total_Applications = nrow(apps),
  Approved           = sum(apps$Application_Status == "Approved"),
  Rejected           = sum(apps$Application_Status == "Rejected"),
  Approval_Rate_Pct  = round(mean(apps$Application_Status == "Approved") * 100, 1),
  Avg_Loan_Amount    = round(mean(apps$Loan_Amount)),
  Median_Loan_Amount = round(median(apps$Loan_Amount))
)
print(overview)
write_csv(overview, "data/overview_kpis.csv")

by_sector <- apps %>%
  group_by(Business_Sector) %>%
  summarise(
    Applications = n(),
    Approval_Pct = round(mean(Application_Status == "Approved") * 100, 1),
    Avg_Loan_Amount = round(mean(Loan_Amount))
  ) %>%
  arrange(desc(Approval_Pct))
print(by_sector)
write_csv(by_sector, "data/by_sector.csv")

by_business_age <- apps %>%
  mutate(age_band = cut(Business_Age, breaks = c(-1, 1, 3, 5, Inf),
                         labels = c("<1yr", "1-3yr", "3-5yr", "5yr+"))) %>%
  group_by(age_band) %>%
  summarise(Applications = n(), Approval_Pct = round(mean(Application_Status == "Approved") * 100, 1))
print(by_business_age)

by_credit_history <- apps %>%
  mutate(credit_band = cut(Credit_Score, breaks = c(0, 580, 670, 740, 850),
                            labels = c("Poor", "Fair", "Good", "Excellent"))) %>%
  group_by(credit_band) %>%
  summarise(Applications = n(), Approval_Pct = round(mean(Application_Status == "Approved") * 100, 1))
print(by_credit_history)

p1 <- ggplot(by_sector, aes(x = reorder(Business_Sector, Approval_Pct), y = Approval_Pct)) +
  geom_col(fill = "#2c3e50") + coord_flip() +
  labs(title = "Approval rate by sector", x = NULL, y = "Approval %") + theme_minimal()
ggsave("plots/approval_by_sector.png", p1, width = 6, height = 4)

## ---- MODULE 2: Rejection analysis -------------------------------------------
rejections <- apps %>% filter(Application_Status == "Rejected")

rejection_reasons <- rejections %>%
  count(Rejection_Reason, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 1))
print(rejection_reasons)
write_csv(rejection_reasons, "data/rejection_reasons.csv")

rejection_by_sector <- rejections %>%
  count(Business_Sector, Rejection_Reason) %>%
  group_by(Business_Sector) %>%
  mutate(pct_within_sector = round(n / sum(n) * 100, 1)) %>%
  ungroup()
print(rejection_by_sector)

## Directional relationships (Module 2's "X up -> rejection probability up")
dti_relationship <- apps %>%
  mutate(dti_band = cut(Debt_to_Income, breaks = c(-Inf, 0.3, 0.5, 0.7, Inf),
                         labels = c("<0.3", "0.3-0.5", "0.5-0.7", "0.7+"))) %>%
  group_by(dti_band) %>%
  summarise(Applications = n(), Rejection_Pct = round(mean(Application_Status == "Rejected") * 100, 1))
cat("\nDebt-to-Income vs Rejection Rate:\n"); print(dti_relationship)

age_relationship <- apps %>%
  mutate(age_band = cut(Business_Age, breaks = c(-1, 1, 3, 5, Inf),
                         labels = c("<1yr", "1-3yr", "3-5yr", "5yr+"))) %>%
  group_by(age_band) %>%
  summarise(Applications = n(), Approval_Pct = round(mean(Application_Status == "Approved") * 100, 1))
cat("\nBusiness Age vs Approval Rate:\n"); print(age_relationship)

p2 <- ggplot(rejection_reasons, aes(x = reorder(Rejection_Reason, Pct), y = Pct)) +
  geom_col(fill = "#c0392b") + coord_flip() +
  labs(title = "Rejection reasons", x = NULL, y = "% of rejections") + theme_minimal()
ggsave("plots/rejection_reasons.png", p2, width = 6, height = 4)

cat("\nStep 2 complete. Descriptive + rejection analysis saved to /data and /plots.\n")
