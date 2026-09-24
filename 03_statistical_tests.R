## ============================================================
## Step 3: Module 3 -- Statistical Analysis
## ============================================================
## This is what makes the project more than "trained XGBoost, got 0.85
## AUC" -- it's the section that shows genuine statistical literacy,
## which the JD explicitly asks for.

library(tidyverse)

apps <- readRDS("data/sme_loans.rds")

results <- list()

## ---- 1. Chi-square: Is approval status associated with sector? -----------
tbl_sector <- table(apps$Business_Sector, apps$Application_Status)
chisq_sector <- chisq.test(tbl_sector)
cat("=== Chi-square: Approval ~ Sector ===\n")
cat("H0: Approval is independent of sector.\n")
cat(sprintf("Chi-sq = %.2f, df = %d, p-value = %.4g\n",
            chisq_sector$statistic, chisq_sector$parameter, chisq_sector$p.value))
cat(if (chisq_sector$p.value < 0.05)
      "-> Reject H0: approval status IS associated with sector.\n"
    else "-> Fail to reject H0: no significant association found.\n")
results$chisq_sector <- chisq_sector

## ---- 2. Chi-square: Is approval status associated with business location -
tbl_location <- table(apps$Business_Location, apps$Application_Status)
chisq_location <- chisq.test(tbl_location)
cat("\n=== Chi-square: Approval ~ Location ===\n")
cat(sprintf("Chi-sq = %.2f, df = %d, p-value = %.4g\n",
            chisq_location$statistic, chisq_location$parameter, chisq_location$p.value))
results$chisq_location <- chisq_location

## ---- 3. Mann-Whitney U tests: Approved vs Rejected on key numeric vars ----
## (Mann-Whitney rather than a t-test since loan amount / revenue / DTI are
##  right-skewed, not normally distributed -- this is the statistically
##  correct choice and worth stating explicitly in your writeup.)
mw_test <- function(var_name) {
  approved <- apps[[var_name]][apps$Application_Status == "Approved"]
  rejected <- apps[[var_name]][apps$Application_Status == "Rejected"]
  test <- wilcox.test(approved, rejected)
  tibble(
    Variable = var_name,
    Median_Approved = round(median(approved), 2),
    Median_Rejected = round(median(rejected), 2),
    W_statistic = test$statistic,
    p_value = test$p.value,
    Significant = test$p.value < 0.05
  )
}

mw_results <- map_dfr(
  c("Loan_Amount", "Annual_Revenue", "Business_Age", "Debt_to_Income", "DSCR", "Credit_Score"),
  mw_test
)
cat("\n=== Mann-Whitney U tests: Approved vs Rejected ===\n")
print(mw_results)
write_csv(mw_results, "data/mann_whitney_results.csv")
results$mann_whitney <- mw_results

## ---- 4. Correlation matrix among key numeric variables --------------------
num_vars <- apps %>%
  select(Annual_Revenue, Loan_Amount, Existing_Debt, Cash_Flow, Credit_Score,
         Debt_to_Income, DSCR, Business_Age)

corr_matrix <- round(cor(num_vars, use = "complete.obs"), 2)
cat("\n=== Correlation matrix ===\n")
print(corr_matrix)
write_csv(as.data.frame(corr_matrix) %>% rownames_to_column("Variable"),
          "data/correlation_matrix.csv")

corr_long <- as.data.frame(corr_matrix) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

p_corr <- ggplot(corr_long, aes(Var1, Var2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = Correlation), size = 3) +
  scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#2c3e50", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation matrix -- key financial variables", x = NULL, y = NULL)
ggsave("plots/correlation_matrix.png", p_corr, width = 7, height = 6)

## ---- Save everything for the Shiny "Model Monitoring"/stats tab ----------
saveRDS(list(
  chisq_sector = chisq_sector,
  chisq_location = chisq_location,
  mann_whitney = mw_results,
  corr_matrix = corr_matrix
), "models/statistical_tests.rds")

cat("\nStep 3 complete. Statistical test results saved to /models and /data.\n")
