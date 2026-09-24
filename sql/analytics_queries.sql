-- ============================================================
-- SME Loan Policy Simulator -- Analytical SQL Queries
-- Run these against sme_loans.db (e.g. in DBeaver, DB Browser for SQLite,
-- or straight from R via DBI::dbGetQuery(con, "...")).
-- Tables: applicants, financials, credit_history, applications, loan_outcomes
-- ============================================================

-- 1. Overall approval rate
SELECT
  ROUND(AVG(CASE WHEN Application_Status = 'Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
FROM applications;

-- 2. Applications and approval rate by sector
SELECT
  a.Business_Sector,
  COUNT(*) AS total_applications,
  SUM(CASE WHEN ap.Application_Status = 'Approved' THEN 1 ELSE 0 END) AS approved,
  ROUND(AVG(CASE WHEN ap.Application_Status = 'Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
FROM applications ap
JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
GROUP BY a.Business_Sector
ORDER BY approval_rate_pct DESC;

-- 3. Approval rate by business location
SELECT
  a.Business_Location,
  COUNT(*) AS total_applications,
  ROUND(AVG(CASE WHEN ap.Application_Status = 'Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
FROM applications ap
JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
GROUP BY a.Business_Location
ORDER BY approval_rate_pct DESC;

-- 4. Monthly application volume trend
SELECT
  strftime('%Y-%m', Application_Date) AS month,
  COUNT(*) AS applications
FROM applications
GROUP BY month
ORDER BY month;

-- 5. Monthly approval rate trend
SELECT
  strftime('%Y-%m', Application_Date) AS month,
  ROUND(AVG(CASE WHEN Application_Status = 'Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
FROM applications
GROUP BY month
ORDER BY month;

-- 6. Rejection reason breakdown
SELECT
  Rejection_Reason,
  COUNT(*) AS n_rejections,
  ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM applications WHERE Application_Status = 'Rejected'), 1) AS pct
FROM applications
WHERE Application_Status = 'Rejected'
GROUP BY Rejection_Reason
ORDER BY n_rejections DESC;

-- 7. Rejection reason by sector (which sectors fail on which grounds)
SELECT
  a.Business_Sector,
  ap.Rejection_Reason,
  COUNT(*) AS n
FROM applications ap
JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
WHERE ap.Application_Status = 'Rejected'
GROUP BY a.Business_Sector, ap.Rejection_Reason
ORDER BY a.Business_Sector, n DESC;

-- 8. Average loan amount: approved vs rejected
SELECT
  ap.Application_Status,
  ROUND(AVG(f.Loan_Amount), 0) AS avg_loan_amount,
  ROUND(AVG(f.Debt_to_Income), 2) AS avg_dti,
  ROUND(AVG(f.DSCR), 2) AS avg_dscr
FROM applications ap
JOIN financials f ON f.Application_ID = ap.Application_ID
GROUP BY ap.Application_Status;

-- 9. Credit score distribution by approval status
SELECT
  ap.Application_Status,
  CASE
    WHEN c.Credit_Score < 580 THEN 'Poor (<580)'
    WHEN c.Credit_Score < 670 THEN 'Fair (580-669)'
    WHEN c.Credit_Score < 740 THEN 'Good (670-739)'
    ELSE 'Excellent (740+)'
  END AS credit_band,
  COUNT(*) AS n
FROM applications ap
JOIN credit_history c ON c.Application_ID = ap.Application_ID
GROUP BY ap.Application_Status, credit_band
ORDER BY ap.Application_Status, credit_band;

-- 10. Default rate by sector (post-approval performance)
SELECT
  a.Business_Sector,
  COUNT(*) AS approved_loans,
  SUM(o.Default_Flag) AS defaults,
  ROUND(AVG(o.Default_Flag) * 100, 1) AS default_rate_pct
FROM loan_outcomes o
JOIN applications ap ON ap.Application_ID = o.Application_ID
JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
GROUP BY a.Business_Sector
ORDER BY default_rate_pct DESC;

-- 11. Business age vs approval rate (bucketed)
SELECT
  CASE
    WHEN a.Business_Age < 1 THEN '<1 year'
    WHEN a.Business_Age < 3 THEN '1-3 years'
    WHEN a.Business_Age < 5 THEN '3-5 years'
    ELSE '5+ years'
  END AS business_age_band,
  COUNT(*) AS total_applications,
  ROUND(AVG(CASE WHEN ap.Application_Status = 'Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
FROM applications ap
JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
GROUP BY business_age_band
ORDER BY business_age_band;

-- 12. Processing time (TAT) by sector
SELECT
  a.Business_Sector,
  ROUND(AVG(ap.Processing_Time), 1) AS avg_processing_days
FROM applications ap
JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
GROUP BY a.Business_Sector
ORDER BY avg_processing_days DESC;

-- 13. Applicants with a previous default -- how does that affect this application?
SELECT
  c.Previous_Default,
  COUNT(*) AS total_applications,
  ROUND(AVG(CASE WHEN ap.Application_Status = 'Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
FROM applications ap
JOIN credit_history c ON c.Application_ID = ap.Application_ID
GROUP BY c.Previous_Default;

-- 14. Collateral coverage ratio vs approval
SELECT
  CASE
    WHEN f.Collateral_Value >= f.Loan_Amount THEN 'Fully collateralized'
    ELSE 'Under-collateralized'
  END AS collateral_status,
  COUNT(*) AS total_applications,
  ROUND(AVG(CASE WHEN ap.Application_Status = 'Approved' THEN 1.0 ELSE 0 END) * 100, 1) AS approval_rate_pct
FROM applications ap
JOIN financials f ON f.Application_ID = ap.Application_ID
GROUP BY collateral_status;

-- 15. Top 10 largest approved loans currently past due
SELECT
  ap.Application_ID,
  a.Business_Sector,
  f.Loan_Amount,
  o.Days_Past_Due
FROM loan_outcomes o
JOIN applications ap ON ap.Application_ID = o.Application_ID
JOIN applicants a ON a.Applicant_ID = ap.Applicant_ID
JOIN financials f ON f.Application_ID = ap.Application_ID
WHERE o.Days_Past_Due > 0
ORDER BY f.Loan_Amount DESC
LIMIT 10;

-- 16. Portfolio concentration -- % of total loan volume by sector
SELECT
  a.Business_Sector,
  ROUND(SUM(f.Loan_Amount) * 100.0 / (SELECT SUM(Loan_Amount) FROM financials), 1) AS pct_of_portfolio
FROM financials f
JOIN applicants a ON a.Applicant_ID = f.Applicant_ID
GROUP BY a.Business_Sector
ORDER BY pct_of_portfolio DESC;
