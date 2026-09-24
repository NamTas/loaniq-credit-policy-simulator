## ============================================================
## Step 4: Module 4 -- Credit Risk Models
## Model A: Application approval (Target = Application_Status)
## Model B: Post-approval default risk (Target = Default_Flag, approved-only)
## Kept SEPARATE deliberately: approval quality != repayment performance.
## ============================================================

library(tidyverse)
library(xgboost)
library(pROC)
library(PRROC)

apps <- readRDS("data/sme_loans.rds")

feature_cols <- c("Business_Age", "Number_of_Employees", "Annual_Revenue",
                   "Loan_Amount", "Loan_Tenure", "Existing_Debt", "Collateral_Value",
                   "Debt_to_Income", "DSCR", "Cash_Flow", "Credit_History",
                   "Previous_Default", "Previous_Loan_Count", "Previous_Late_Payment",
                   "Credit_Score")

## ---- Shared evaluation helper ----------------------------------------------
evaluate_model <- function(pred_prob, actual, threshold = 0.5) {
  pred_class <- ifelse(pred_prob >= threshold, 1, 0)
  cm <- table(Predicted = pred_class, Actual = actual)
  tp <- sum(pred_class == 1 & actual == 1); fp <- sum(pred_class == 1 & actual == 0)
  fn <- sum(pred_class == 0 & actual == 1); tn <- sum(pred_class == 0 & actual == 0)
  precision <- tp / (tp + fp); recall <- tp / (tp + fn)
  f1 <- 2 * precision * recall / (precision + recall)
  roc_obj <- roc(actual, pred_prob, quiet = TRUE)
  pr_obj <- pr.curve(scores.class0 = pred_prob[actual == 1],
                      scores.class1 = pred_prob[actual == 0], curve = FALSE)
  list(
    confusion_matrix = cm,
    metrics = tibble(
      Accuracy = round((tp + tn) / length(actual), 3),
      Precision = round(precision, 3),
      Recall = round(recall, 3),
      F1 = round(f1, 3),
      ROC_AUC = round(as.numeric(auc(roc_obj)), 3),
      PR_AUC = round(pr_obj$auc.integral, 3)
    ),
    roc_obj = roc_obj
  )
}

train_xgb <- function(X_train, y_train, X_test) {
  dtrain <- xgb.DMatrix(as.matrix(X_train), label = y_train)
  dtest  <- xgb.DMatrix(as.matrix(X_test))
  model <- xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "auc",
                  max_depth = 4, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8),
    data = dtrain, nrounds = 300, verbose = 0
  )
  list(model = model, pred = predict(model, dtest))
}

## ============================================================
## MODEL A: Approval prediction
## ============================================================
dataA <- apps %>%
  mutate(target = ifelse(Application_Status == "Approved", 1, 0)) %>%
  select(all_of(feature_cols), target)

set.seed(42)
idx <- sample(nrow(dataA), 0.7 * nrow(dataA))
trainA <- dataA[idx, ]; testA <- dataA[-idx, ]

logitA <- glm(target ~ ., data = trainA, family = "binomial")
predA_logit <- predict(logitA, testA, type = "response")

xgbA <- train_xgb(trainA %>% select(-target), trainA$target, testA %>% select(-target))

evalA_logit <- evaluate_model(predA_logit, testA$target)
evalA_xgb   <- evaluate_model(xgbA$pred, testA$target)

cat("=== MODEL A: Approval Prediction ===\n")
cat("-- Logistic Regression --\n"); print(evalA_logit$metrics)
cat("-- XGBoost --\n"); print(evalA_xgb$metrics)

## ============================================================
## MODEL B: Post-approval default prediction (approved applications only)
## ============================================================
dataB <- apps %>%
  filter(Application_Status == "Approved") %>%
  mutate(target = Default_Flag) %>%
  select(all_of(feature_cols), target)

set.seed(42)
idxB <- sample(nrow(dataB), 0.7 * nrow(dataB))
trainB <- dataB[idxB, ]; testB <- dataB[-idxB, ]

logitB <- glm(target ~ ., data = trainB, family = "binomial")
predB_logit <- predict(logitB, testB, type = "response")

xgbB <- train_xgb(trainB %>% select(-target), trainB$target, testB %>% select(-target))

evalB_logit <- evaluate_model(predB_logit, testB$target)
evalB_xgb   <- evaluate_model(xgbB$pred, testB$target)

cat("\n=== MODEL B: Post-Approval Default Prediction ===\n")
cat("-- Logistic Regression --\n"); print(evalB_logit$metrics)
cat("-- XGBoost --\n"); print(evalB_xgb$metrics)

## ---- Save everything the Shiny app / SHAP script needs --------------------
saveRDS(list(
  feature_cols = feature_cols,
  trainA = trainA, testA = testA, logitA = logitA, xgbA_model = xgbA$model,
  predA_logit = predA_logit, predA_xgb = xgbA$pred,
  evalA_logit = evalA_logit, evalA_xgb = evalA_xgb,
  trainB = trainB, testB = testB, logitB = logitB, xgbB_model = xgbB$model,
  predB_logit = predB_logit, predB_xgb = xgbB$pred,
  evalB_logit = evalB_logit, evalB_xgb = evalB_xgb
), "models/credit_risk_models.rds")

cat("\nStep 4 complete. Both models trained and saved to models/credit_risk_models.rds\n")
