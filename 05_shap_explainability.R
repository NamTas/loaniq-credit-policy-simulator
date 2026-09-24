## ============================================================
## Step 5: SHAP Explainability (native TreeSHAP -- fast, exact)
## ============================================================
## Uses xgboost's built-in predcontrib=TRUE, which computes exact SHAP
## values for the WHOLE test set in one matrix operation -- no slow
## permutation loop needed (see project 1's note on this).

library(tidyverse)
library(xgboost)

rm_obj <- readRDS("models/credit_risk_models.rds")

compute_shap <- function(model, X_test, label) {
  dtest <- xgb.DMatrix(as.matrix(X_test))
  contrib <- predict(model, dtest, predcontrib = TRUE)
  colnames(contrib) <- c(colnames(X_test), "BIAS")

  global_imp <- as.data.frame(contrib) %>%
    select(-BIAS) %>%
    summarise(across(everything(), ~ mean(abs(.x)))) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "mean_abs_shap") %>%
    arrange(desc(mean_abs_shap))

  cat(sprintf("\nTop 8 SHAP drivers -- %s:\n", label))
  print(head(global_imp, 8))

  list(shap_matrix = contrib, global_importance = global_imp)
}

## ---- Model A (Approval) SHAP ------------------------------------------------
shapA <- compute_shap(
  rm_obj$xgbA_model,
  rm_obj$testA %>% select(-target),
  "Model A (Approval)"
)

## ---- Model B (Default risk) SHAP --------------------------------------------
shapB <- compute_shap(
  rm_obj$xgbB_model,
  rm_obj$testB %>% select(-target),
  "Model B (Default Risk)"
)

## Sample of applicant IDs for the dropdown in the Shiny app.
set.seed(42)
sampleA_idx <- sample(seq_len(nrow(rm_obj$testA)), min(500, nrow(rm_obj$testA)))
sampleB_idx <- sample(seq_len(nrow(rm_obj$testB)), min(500, nrow(rm_obj$testB)))

saveRDS(list(
  shapA = shapA, shapB = shapB,
  sampleA_idx = sampleA_idx, sampleB_idx = sampleB_idx,
  testA = rm_obj$testA, testB = rm_obj$testB
), "models/shap_results.rds")

cat("\nStep 5 complete. SHAP results saved to models/shap_results.rds\n")
