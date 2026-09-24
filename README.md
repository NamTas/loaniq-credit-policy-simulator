# SME Credit Risk Analytics & Policy Simulation Platform (LoanIQ)

An end-to-end SME credit decisioning analytics platform featuring descriptive analytics, rejection reason modeling, statistical hypothesis testing, dual credit risk models (approval vs. default probability), SHAP-based model explainability, and a live scenario simulation dashboard built with **R Shiny**.

---

## 📌 Project Overview

This platform simulates an enterprise-level credit risk workflow for Small and Medium Enterprise (SME) lending:

* **Executive Dashboard:** Application volume flow, approval trends, and status breakdowns.
* **Predictive Credit Scoring:** Dual-model architecture (**Model A:** Approval likelihood, **Model B:** Default risk) comparing Logistic Regression and XGBoost.
* **Explainable AI (XAI):** Native TreeSHAP integration providing both global risk driver insights and individual applicant waterfall breakdowns.
* **Interactive Policy Simulator:** Live what-if analysis tool allowing credit policy managers to adjust underwriting parameters (min business age, DTI, DSCR, credit score, collateral) and immediately visualize approval rate and portfolio risk mix shifts.

> **Data Note:** To reflect realistic SME lending scenarios (combining application metrics, rejection flags, and post-disbursement default outcomes), this project utilizes a statistically structured synthetic dataset generated via `01_data_generation.R` (10,000 application records).

---

## 🛠️ Tech Stack & Architecture

* **Language:** R
* **Dashboard Framework:** Shiny, bslib (Executive Dark Theme)
* **Machine Learning:** `xgboost`, `pROC`, `PRROC`, `pROC`
* **Explainability:** SHAP (TreeSHAP)
* **Data & Storage:** `tidyverse`, `DBI`, `RSQLite` (Normalized 5-table SQLite DB)

---

## 🚀 Project Pipeline & Execution Order

To run the project locally, execute the scripts sequentially to populate the `data/` and `models/` directories required by the Shiny app:

| Order | Script | Function |
| :--- | :--- | :--- |
| **01** | `01_data_generation.R` | Generates the synthetic dataset and builds `sme_loans.db` (SQLite) |
| **02** | `02_descriptive_rejection_analysis.R` | Computes application KPIs and rejection driver metrics |
| **03** | `03_statistical_tests.R` | Runs Chi-Square, Mann-Whitney U tests, and correlation matrices |
| **04** | `04_credit_risk_models.R` | Trains and evaluates Model A (Approval) and Model B (Default Risk) |
| **05** | `05_shap_explainability.R` | Computes native TreeSHAP values for local/global explainability |
| **App**| `app.R` | Launches the interactive 7-tab Shiny Dashboard |

---

## 💻 How to Run Locally

1. **Clone the repository:**
   ```bash
   git clone [https://github.com/YOUR_USERNAME/loaniq-credit-policy-simulator.git](https://github.com/NamTas/loaniq-credit-policy-simulator.git)
   cd loaniq-credit-policy-simulator