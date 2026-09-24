## ============================================================
## LoanIQ: Approval & Risk Simulator Dashboard
## High-Tech Executive Dark Theme
## ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(xgboost)
library(pROC)
library(DT)

# Load Datasets & Models
apps    <- readRDS("data/sme_loans.rds")
rm_obj  <- readRDS("models/credit_risk_models.rds")
shap_r  <- readRDS("models/shap_results.rds")
stats_r <- readRDS("models/statistical_tests.rds")

## ---- Score EVERY application with Model B (default risk) ----
feature_cols <- rm_obj$feature_cols
X_all <- apps %>% select(all_of(feature_cols)) %>% as.matrix()
apps$risk_score <- predict(rm_obj$xgbB_model, xgb.DMatrix(X_all))
apps <- apps %>%
  mutate(risk_band = case_when(
    risk_score < 0.10 ~ "Low",
    risk_score < 0.25 ~ "Medium",
    TRUE ~ "High"
  ))

## Baseline Policy Definition
current_policy <- list(min_age = 3, max_dti = 0.5, min_score = 650, min_dscr = 1.2, min_collateral_ratio = 1.0)

simulate_policy <- function(data, min_age, max_dti, min_score, min_dscr, min_collateral_ratio) {
  eligible <- data %>%
    filter(Business_Age >= min_age,
           Debt_to_Income <= max_dti,
           Credit_Score >= min_score,
           DSCR >= min_dscr,
           Collateral_Value / Loan_Amount >= min_collateral_ratio)
  tibble(
    Eligible_Applications = nrow(eligible),
    Approval_Rate_Pct = round(nrow(eligible) / nrow(data) * 100, 1),
    Avg_Loan_Amount = if (nrow(eligible) > 0) round(mean(eligible$Loan_Amount)) else NA,
    Low_Risk_Pct = if (nrow(eligible) > 0) round(mean(eligible$risk_band == "Low") * 100, 1) else NA,
    Medium_Risk_Pct = if (nrow(eligible) > 0) round(mean(eligible$risk_band == "Medium") * 100, 1) else NA,
    High_Risk_Pct = if (nrow(eligible) > 0) round(mean(eligible$risk_band == "High") * 100, 1) else NA
  )
}

baseline_result <- simulate_policy(apps, current_policy$min_age, current_policy$max_dti,
                                   current_policy$min_score, current_policy$min_dscr,
                                   current_policy$min_collateral_ratio)

## ---- Custom Dark Theme GGPlot Helper ----
theme_dark_pro <- function() {
  theme_minimal(base_family = "sans") %+replace%
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = "#1F2937", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#94A3B8", size = 9),
      axis.title = element_text(color = "#CBD5E1", size = 10, face = "bold"),
      legend.text = element_text(color = "#FFFFFF", size = 10, face = "bold"),
      legend.title = element_text(color = "#38BDF8", size = 10, face = "bold"),
      plot.title = element_text(color = "#38BDF8", size = 11, face = "bold", hjust = 0),
      strip.text = element_text(color = "#F8FAFC", size = 10, face = "bold")
    )
}

## Custom Eye-Catchy KPI Card Generator (Updated Spacing & Alignment)
kpi_card <- function(title, value, icon_name, accent_color, gradient_bg) {
  div(
    class = "custom-kpi-card",
    style = sprintf("background: %s; border-left: 5px solid %s;", gradient_bg, accent_color),
    div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%; gap: 15px;",
      div(
        style = "flex-grow: 1; overflow: hidden;",
        div(class = "kpi-title", title),
        div(class = "kpi-value", value)
      ),
      div(
        class = "kpi-icon",
        style = sprintf("color: %s; background: rgba(255, 255, 255, 0.08); padding: 10px; border-radius: 50%%; display: flex; align-items: center; justify-content: center; width: 42px; height: 42px; flex-shrink: 0; margin-left: auto;", accent_color),
        icon(icon_name)
      )
    )
  )
}

## ============================================================
## UI DESIGN
## ============================================================
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 8px; flex-shrink: 0;",
    span("LoanIQ", style = "font-weight: 900; font-size: 1.4rem; letter-spacing: 0.8px; color: #38BDF8;"),
    span("| Approval & Risk Simulator", style = "font-weight: 500; font-size: 0.85rem; color: #94A3B8; white-space: nowrap;")
  ),
  theme = bs_theme(
    version = 5,
    bg = "#0B0F19",
    fg = "#F8FAFC",
    primary = "#38BDF8"
  ),
  
  ## ---- Custom CSS Styling ----
  header = tags$head(
    tags$style(HTML("
      body { background-color: #0B0F19 !important; color: #F8FAFC !important; font-family: 'Inter', sans-serif; overflow-x: hidden; }
      
      /* Header & Navigation Styling (No Scrollbars, Clean Bigger Font) */
      .navbar { background-color: #0F172A !important; border-bottom: 1px solid #1E293B !important; padding: 0.6rem 1.2rem !important; overflow: hidden !important; }
      .navbar-nav { flex-direction: row !important; gap: 6px !important; margin-left: 15px !important; flex-wrap: nowrap !important; overflow: hidden !important; }
      .nav-link { color: #94A3B8 !important; font-weight: 600 !important; font-size: 0.88rem !important; white-space: nowrap !important; padding: 7px 12px !important; border-radius: 6px; transition: all 0.2s ease; }
      .nav-link:hover { color: #38BDF8 !important; background: rgba(56, 189, 248, 0.08) !important; }
      .nav-link.active { color: #38BDF8 !important; background: rgba(56, 189, 248, 0.18) !important; font-weight: 700 !important; border-bottom: none !important; }
      
      /* Completely Hide Scrollbars in Navbar */
      .navbar::-webkit-scrollbar, .navbar-nav::-webkit-scrollbar { display: none !important; width: 0 !important; height: 0 !important; }
      
      /* Card Styling */
      .card { background-color: #111827 !important; border: 1px solid #1F2937 !important; border-radius: 12px !important; box-shadow: 0 4px 20px rgba(0, 0, 0, 0.35) !important; margin-bottom: 15px; }
      .card-header { background-color: #111827 !important; border-bottom: 1px solid #1F2937 !important; color: #38BDF8 !important; font-weight: 700 !important; font-size: 0.85rem !important; text-transform: uppercase; letter-spacing: 0.5px; }
      
      /* Eye-Catchy KPI Box Styling */
      .custom-kpi-card {
        border-radius: 12px;
        padding: 14px 18px;
        box-shadow: 0 6px 18px rgba(0, 0, 0, 0.4);
        height: 88px;
        overflow: hidden !important;
        display: flex;
        align-items: center;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      .custom-kpi-card:hover { transform: translateY(-3px); box-shadow: 0 10px 25px rgba(0, 0, 0, 0.5); }
      .kpi-title { font-size: 0.72rem; text-transform: uppercase; font-weight: 700; color: #94A3B8; letter-spacing: 0.5px; white-space: nowrap; }
      .kpi-value { font-size: 1.55rem; font-weight: 800; color: #FFFFFF; margin-top: 2px; line-height: 1.1; }
      .kpi-icon i { font-size: 1.3rem; }
      
      /* Plot Canvas Transparent Background Fix */
      .shiny-plot-output { background-color: transparent !important; }
      
      /* Inputs & Tables */
      .form-control, .selectize-input, .selectize-dropdown { background-color: #0F172A !important; color: #F8FAFC !important; border: 1px solid #334155 !important; border-radius: 8px !important; }
      .selectize-input { background-color: #0F172A !important; color: #F8FAFC !important; }
      .table { color: #F8FAFC !important; border-color: #1F2937 !important; }
      .table th { background-color: #0F172A !important; color: #38BDF8 !important; border-bottom: 2px solid #1F2937 !important; font-size: 0.85rem; }
      .table td { border-color: #1F2937 !important; font-size: 0.85rem; }
      
      /* Sliders */
      .irs--shiny .irs-bar { background: #38BDF8 !important; border-top: 1px solid #38BDF8 !important; border-bottom: 1px solid #38BDF8 !important; }
      .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #38BDF8 !important; color: #0B0F19 !important; font-weight: bold; }
    "))
  ),
  
  ## ---- TAB 1: Executive Overview ----
  nav_panel("Executive Overview",
            layout_columns(col_widths = c(3, 3, 3, 3),
                           kpi_card("Total Applications", format(nrow(apps), big.mark = ","), "file-invoice", "#38BDF8", "linear-gradient(135deg, #0F172A 0%, #1E293B 100%)"),
                           kpi_card("Approval Rate", sprintf("%.1f%%", mean(apps$Application_Status == "Approved") * 100), "check-circle", "#34D399", "linear-gradient(135deg, #064E3B 0%, #0F172A 100%)"),
                           kpi_card("Avg Loan Amount", paste0("$", format(round(mean(apps$Loan_Amount)), big.mark = ",")), "dollar-sign", "#FBBF24", "linear-gradient(135deg, #451A03 0%, #0F172A 100%)"),
                           kpi_card("High-Risk Share", sprintf("%.1f%%", mean(apps$risk_band == "High") * 100), "triangle-exclamation", "#F87171", "linear-gradient(135deg, #4C0519 0%, #0F172A 100%)")
            ),
            div(style = "margin-top: 10px;"),
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Monthly Application Volume Flow"), plotOutput("monthly_volume", height = "250px")),
                           card(card_header("Monthly Approval Rate Trend (%)"), plotOutput("monthly_approval", height = "250px"))
            ),
            card(card_header("Application Status Breakdown"), plotOutput("status_split", height = "240px"))
  ),
  
  ## ---- TAB 2: Application Analytics ----
  nav_panel("Application Analytics",
            layout_columns(col_widths = c(3, 9),
                           card(
                             card_header("Global Filters"),
                             selectInput("f_sector", "Business Sector", choices = c("All", unique(apps$Business_Sector))),
                             selectInput("f_location", "Business Location", choices = c("All", unique(apps$Business_Location)))
                           ),
                           layout_columns(col_widths = c(6, 6),
                                          card(card_header("Approval Rate by Sector"), plotOutput("approval_sector", height = "250px")),
                                          card(card_header("Approval Rate by Business Age"), plotOutput("approval_age", height = "250px"))
                           )
            ),
            card(card_header("Loan Amount Distribution (Filtered Pool)"), plotOutput("loan_amount_dist", height = "240px"))
  ),
  
  ## ---- TAB 3: Rejection Analytics ----
  nav_panel("Rejection Analytics",
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Top Application Rejection Drivers"), plotOutput("rejection_reasons_plot", height = "260px")),
                           card(card_header("Rejection Rate by DTI Band"), plotOutput("rejection_dti", height = "260px"))
            ),
            card(card_header("Rejection Reason Share by Business Sector"), plotOutput("rejection_by_sector", height = "240px"))
  ),
  
  ## ---- TAB 4: Credit Risk ----
  nav_panel("Credit Risk",
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Portfolio Risk Band Distribution"), plotOutput("risk_band_dist", height = "260px")),
                           card(card_header("Predicted Default Probability Density"), plotOutput("risk_score_dist", height = "260px"))
            ),
            card(card_header("Model A vs Model B Performance Matrix"), tableOutput("model_quick_compare"))
  ),
  
  ## ---- TAB 5: Policy Simulator ----
  nav_panel("Policy Simulator",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header("Simulated Policy Controls"),
                             sliderInput("sim_age", "Min Business Age (Years)", min = 0, max = 10, value = 3, step = 0.5),
                             sliderInput("sim_dti", "Max Debt-to-Income (DTI)", min = 0.1, max = 1.5, value = 0.5, step = 0.05),
                             sliderInput("sim_score", "Min Credit Score", min = 300, max = 850, value = 650, step = 10),
                             sliderInput("sim_dscr", "Min DSCR Ratio", min = 0.5, max = 3, value = 1.2, step = 0.1),
                             sliderInput("sim_collateral", "Min Collateral Coverage", min = 0.5, max = 2, value = 1.0, step = 0.1)
                           ),
                           card(
                             card_header("Baseline vs Simulated Impact"),
                             tableOutput("policy_comparison"),
                             div(style = "margin-top: 15px;"),
                             plotOutput("policy_risk_bands", height = "220px")
                           )
            )
  ),
  
  ## ---- TAB 6: Applicant Explanation ----
  nav_panel("Applicant Explanation",
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header("Select Applicant ID"),
                             selectInput("applicant_row", "Applicant (Test Index)", choices = shap_r$sampleB_idx)
                           ),
                           card(
                             card_header("SHAP Local Feature Contribution Waterfall"),
                             plotOutput("shap_waterfall", height = "360px")
                           )
            ),
            card(card_header("Global Default Risk Drivers (SHAP Importance)"), plotOutput("global_shap_plot", height = "250px"))
  ),
  
  ## ---- TAB 7: Model Monitoring ----
  nav_panel("Model Monitoring",
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Model A (Approval) Metrics"), tableOutput("metricsA")),
                           card(card_header("Model B (Default) Metrics"), tableOutput("metricsB"))
            ),
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Model B ROC Curve Analysis"), plotOutput("roc_B", height = "250px")),
                           card(card_header("Statistical Hypothesis Test Summary"), tableOutput("stats_summary"))
            )
  )
)

## ============================================================
## SERVER LOGIC
## ============================================================
server <- function(input, output, session) {
  
  ## ---- Tab 1 ----
  output$monthly_volume <- renderPlot({
    apps %>% mutate(month = floor_date(Application_Date, "month")) %>%
      count(month) %>%
      ggplot(aes(month, n)) + 
      geom_area(fill = "#38BDF8", alpha = 0.2) +
      geom_line(color = "#38BDF8", linewidth = 1.2) +
      labs(x = NULL, y = "Applications") + theme_dark_pro()
  }, bg = "transparent")
  
  output$monthly_approval <- renderPlot({
    apps %>% mutate(month = floor_date(Application_Date, "month")) %>%
      group_by(month) %>% summarise(rate = mean(Application_Status == "Approved") * 100) %>%
      ggplot(aes(month, rate)) + 
      geom_area(fill = "#34D399", alpha = 0.2) +
      geom_line(color = "#34D399", linewidth = 1.2) +
      labs(x = NULL, y = "Approval %") + theme_dark_pro()
  }, bg = "transparent")
  
  output$status_split <- renderPlot({
    apps %>% count(Application_Status) %>%
      ggplot(aes(x = "", y = n, fill = Application_Status)) +
      geom_col(width = 1, color = "#0B0F19", linewidth = 1) + coord_polar("y") + 
      scale_fill_manual(values = c("Approved" = "#34D399", "Rejected" = "#F87171")) +
      labs(fill = "Application Status") +
      theme_dark_pro() + 
      theme(
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.grid.major = element_blank(),
        legend.text = element_text(color = "#FFFFFF", size = 11, face = "bold"),
        legend.title = element_text(color = "#38BDF8", size = 11, face = "bold")
      )
  }, bg = "transparent")
  
  ## ---- Tab 2 ----
  filtered <- reactive({
    d <- apps
    if (input$f_sector != "All") d <- d %>% filter(Business_Sector == input$f_sector)
    if (input$f_location != "All") d <- d %>% filter(Business_Location == input$f_location)
    d
  })
  output$approval_sector <- renderPlot({
    filtered() %>% group_by(Business_Sector) %>%
      summarise(rate = mean(Application_Status == "Approved") * 100) %>%
      ggplot(aes(reorder(Business_Sector, rate), rate)) + 
      geom_col(fill = "#38BDF8", width = 0.6) + coord_flip() + 
      labs(x = NULL, y = "Approval %") + theme_dark_pro()
  }, bg = "transparent")
  
  output$approval_age <- renderPlot({
    filtered() %>% mutate(band = cut(Business_Age, c(-1, 1, 3, 5, Inf), labels = c("<1", "1-3", "3-5", "5+"))) %>%
      group_by(band) %>% summarise(rate = mean(Application_Status == "Approved") * 100) %>%
      ggplot(aes(band, rate)) + 
      geom_col(fill = "#818CF8", width = 0.5) + 
      labs(x = "Business Age (Yrs)", y = "Approval %") + theme_dark_pro()
  }, bg = "transparent")
  
  output$loan_amount_dist <- renderPlot({
    ggplot(filtered(), aes(Loan_Amount, fill = Application_Status)) +
      geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("Approved" = "#34D399", "Rejected" = "#F87171")) +
      labs(x = "Loan Amount ($)", y = "Count") + theme_dark_pro()
  }, bg = "transparent")
  
  ## ---- Tab 3 ----
  output$rejection_reasons_plot <- renderPlot({
    apps %>% filter(Application_Status == "Rejected") %>% count(Rejection_Reason) %>%
      ggplot(aes(reorder(Rejection_Reason, n), n)) + 
      geom_col(fill = "#F87171", width = 0.6) + coord_flip() + 
      labs(x = NULL, y = "Count") + theme_dark_pro()
  }, bg = "transparent")
  
  output$rejection_dti <- renderPlot({
    apps %>% mutate(band = cut(Debt_to_Income, c(-Inf, 0.3, 0.5, 0.7, Inf), labels = c("<0.3", "0.3-0.5", "0.5-0.7", "0.7+"))) %>%
      group_by(band) %>% summarise(rate = mean(Application_Status == "Rejected") * 100) %>%
      ggplot(aes(band, rate)) + 
      geom_col(fill = "#FB923C", width = 0.5) + 
      labs(x = "DTI Ratio Band", y = "Rejection %") + theme_dark_pro()
  }, bg = "transparent")
  
  output$rejection_by_sector <- renderPlot({
    apps %>% filter(Application_Status == "Rejected") %>%
      count(Business_Sector, Rejection_Reason) %>%
      ggplot(aes(Business_Sector, n, fill = Rejection_Reason)) +
      geom_col(position = "fill") + coord_flip() + 
      scale_fill_brewer(palette = "Set3") +
      labs(x = NULL, y = "Share") + theme_dark_pro()
  }, bg = "transparent")
  
  ## ---- Tab 4 ----
  output$risk_band_dist <- renderPlot({
    apps %>% count(risk_band) %>%
      ggplot(aes(factor(risk_band, levels = c("Low","Medium","High")), n, fill = risk_band)) +
      geom_col(width = 0.5) + labs(x = NULL, y = "Applications") +
      scale_fill_manual(values = c(Low = "#34D399", Medium = "#FBBF24", High = "#F87171")) +
      theme_dark_pro() + theme(legend.position = "none")
  }, bg = "transparent")
  
  output$risk_score_dist <- renderPlot({
    ggplot(apps, aes(risk_score)) + 
      geom_histogram(bins = 40, fill = "#38BDF8", alpha = 0.8) +
      labs(x = "Predicted Default Probability", y = "Count") + theme_dark_pro()
  }, bg = "transparent")
  
  output$model_quick_compare <- renderTable({
    bind_rows(
      rm_obj$evalA_logit$metrics %>% mutate(Model = "Model A -- Logistic"),
      rm_obj$evalA_xgb$metrics %>% mutate(Model = "Model A -- XGBoost"),
      rm_obj$evalB_logit$metrics %>% mutate(Model = "Model B -- Logistic"),
      rm_obj$evalB_xgb$metrics %>% mutate(Model = "Model B -- XGBoost")
    ) %>% relocate(Model)
  })
  
  ## ---- Tab 5: Policy Simulator ----
  sim_result <- reactive({
    simulate_policy(apps, input$sim_age, input$sim_dti, input$sim_score,
                    input$sim_dscr, input$sim_collateral)
  })
  
  output$policy_comparison <- renderTable({
    bind_rows(
      baseline_result %>% mutate(Policy = "Current Baseline"),
      sim_result() %>% mutate(Policy = "Simulated Policy")
    ) %>% relocate(Policy)
  })
  
  output$policy_risk_bands <- renderPlot({
    eligible_sim <- apps %>%
      filter(Business_Age >= input$sim_age, Debt_to_Income <= input$sim_dti,
             Credit_Score >= input$sim_score, DSCR >= input$sim_dscr,
             Collateral_Value / Loan_Amount >= input$sim_collateral)
    if (nrow(eligible_sim) == 0) return(NULL)
    eligible_sim %>% count(risk_band) %>%
      ggplot(aes(factor(risk_band, levels = c("Low","Medium","High")), n, fill = risk_band)) +
      geom_col(width = 0.5) + labs(title = "Simulated Eligible Pool Risk Profile", x = NULL, y = "Count") +
      scale_fill_manual(values = c(Low = "#34D399", Medium = "#FBBF24", High = "#F87171")) +
      theme_dark_pro() + theme(legend.position = "none")
  }, bg = "transparent")
  
  ## ---- Tab 6 ----
  output$shap_waterfall <- renderPlot({
    row_i <- as.integer(input$applicant_row)
    contrib <- shap_r$shapB$shap_matrix[row_i, ]
    feats <- setdiff(names(contrib), "BIAS")
    top <- contrib[feats][order(abs(contrib[feats]), decreasing = TRUE)][1:10]
    plot_df <- tibble(feature = names(top), shap = as.numeric(top)) %>% arrange(shap)
    ggplot(plot_df, aes(reorder(feature, shap), shap, fill = shap > 0)) +
      geom_col(width = 0.6) + coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#F87171", "FALSE" = "#38BDF8"), guide = "none") +
      labs(x = NULL, y = "SHAP Contribution to Default Risk Score",
           title = paste("Risk Explanation for Test Row #", row_i)) +
      theme_dark_pro()
  }, bg = "transparent")
  
  output$global_shap_plot <- renderPlot({
    shap_r$shapB$global_importance %>% slice_max(mean_abs_shap, n = 10) %>%
      ggplot(aes(reorder(feature, mean_abs_shap), mean_abs_shap)) +
      geom_col(fill = "#818CF8", width = 0.6) + coord_flip() +
      labs(x = NULL, y = "Mean |SHAP Value|") + theme_dark_pro()
  }, bg = "transparent")
  
  ## ---- Tab 7 ----
  output$metricsA <- renderTable({
    bind_rows(rm_obj$evalA_logit$metrics %>% mutate(Model = "Logistic Regression"),
              rm_obj$evalA_xgb$metrics %>% mutate(Model = "XGBoost Classifier")) %>% relocate(Model)
  })
  
  output$metricsB <- renderTable({
    bind_rows(rm_obj$evalB_logit$metrics %>% mutate(Model = "Logistic Regression"),
              rm_obj$evalB_xgb$metrics %>% mutate(Model = "XGBoost Classifier")) %>% relocate(Model)
  })
  
  output$roc_B <- renderPlot({
    plot(rm_obj$evalB_logit$roc_obj, col = "#38BDF8", lwd = 2, main = "", col.axis = "#94A3B8")
    plot(rm_obj$evalB_xgb$roc_obj, col = "#FB923C", lwd = 2, add = TRUE)
    legend("bottomright", legend = c("Logistic (AUC ~0.62)", "XGBoost (AUC ~0.44)"),
           col = c("#38BDF8", "#FB923C"), lwd = 2, bty = "n", text.col = "#F8FAFC")
  }, bg = "transparent")
  
  output$stats_summary <- renderTable({
    tibble(
      Test = c("Chi-sq: Approval ~ Sector", "Chi-sq: Approval ~ Location"),
      Statistic = c(round(stats_r$chisq_sector$statistic, 2), round(stats_r$chisq_location$statistic, 2)),
      p_value = c(signif(stats_r$chisq_sector$p.value, 3), signif(stats_r$chisq_location$p.value, 3))
    )
  })
}

shinyApp(ui, server)