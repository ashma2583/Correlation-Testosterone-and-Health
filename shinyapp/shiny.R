# ============================================================
# Title: Integrative Health Predictors of Testosterone in Adult Males
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Integrative Health Predictors of Testosterone in Adult Males"),
  p("Explore how cardiovascular, metabolic, and renal indicators relate to testosterone levels among adult males in the NHANES dataset."),
  
  sidebarLayout(
    sidebarPanel(
      # ------------------------------------------------------------
      # FILTER OPTIONS
      # ------------------------------------------------------------
      h4("Filter Options"),
      selectInput("diabetes", "Filter by Diabetes Status:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("phys_active", "Filter by Physical Activity:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      
      hr(),
      
      # ------------------------------------------------------------
      # PLOT OPTIONS
      # ------------------------------------------------------------
      h4("Plot Options"),
      selectInput("xvar", "Select X Variable:",
                  choices = c("BMI", "Pulse", "BPDiaAve", "BPSysAve", "UrineFlow1", "TotChol", "Age")),
      
      hr(),
      
      # ------------------------------------------------------------
      # MODEL OPTIONS
      # ------------------------------------------------------------
      h4("Model Options"),
      
      h5("Select Health Metrics (Predictors of Interest):"),
      checkboxGroupInput("predictors", NULL,
                         choices = c("Systolic BP (BPSysAve)" = "BPSysAve",
                                     "Diastolic BP (BPDiaAve)" = "BPDiaAve",
                                     "Resting Pulse (Pulse)" = "Pulse",
                                     "Total Cholesterol (TotChol)" = "TotChol",
                                     "Urine Flow Rate (UrineFlow1)" = "UrineFlow1"),
                         selected = NULL),
      
      h5("Select Controls (Confounders):"),
      checkboxGroupInput("controls", NULL,
                         choices = c("Age (Default to ON)" = "Age",
                                     "BMI (Default to ON)" = "BMI",
                                     "Diabetes Status" = "Diabetes",
                                     "Physical Activity" = "PhysActive"),
                         selected = c("Age", "BMI")),
      
      actionButton("run_model", "Run Regression", class = "btn-primary")
    ),
    
    # ------------------------------------------------------------
    # MAIN PANEL
    # ------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("scatterPlot"),
                 verbatimTextOutput("corrText")
        ),
        tabPanel("Regression Summary",
                 verbatimTextOutput("modelSummary")
        ),
        tabPanel("About",
                 h4("About This App"),
                 p("This interactive web app explores how cardiovascular, metabolic, and renal health indicators 
                    relate to testosterone levels among adult males in the NHANES dataset. It combines regression 
                    modeling and visualization to help users identify which everyday health metrics are most strongly 
                    associated with hormonal balance."),
                 
                 h5("How to Use"),
                 tags$ul(
                   tags$li("Use the 'Filter Options' section to select diabetes and physical activity subgroups."),
                   tags$li("Under 'Plot Options', choose which variable to visualize on the x-axis."),
                   tags$li("Under 'Model Options', add or remove predictors and confounders."),
                   tags$li("Click 'Run Regression' to generate results in the 'Regression Summary' tab.")
                 ),
                 
                 h5("Interactivity & Interpretation"),
                 p("The plot displays a fitted regression line with a 95% confidence interval. 
                   Below the plot, the Pearson correlation coefficient (r) and p-value quantify the 
                   direction, strength, and significance of the relationship."),
                 
                 h5("Technical Notes"),
                 tags$ul(
                   tags$li("Linear regression models were estimated using R’s 'lm()' function."),
                   tags$li("VIF values < 1.5 confirmed no multicollinearity."),
                   tags$li("Cook’s Distance < 0.1 for all observations indicated model stability."),
                   tags$li("No log-transform was applied, as the linear model best captured the data."),
                   tags$li("'Weight' and 'HealthGen' variables were removed for clarity and consistency.")
                 ),
                 
                 h5("Project Context"),
                 p("This project investigates how lifestyle and physiological factors jointly influence 
                    testosterone levels. Results suggest that higher BMI and resting pulse are associated 
                    with lower testosterone, while stronger renal health (urine flow) predicts higher levels. 
                    The model emphasizes interconnected health systems rather than isolated variables.")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output) {
  
  # Reactive dataset filter
  filtered_data <- reactive({
    data <- df  # assumes 'df' is loaded (cleaned NHANES subset)
    
    if (input$diabetes != "All") {
      data <- data %>% filter(Diabetes == input$diabetes)
    }
    if (input$phys_active != "All") {
      data <- data %>% filter(PhysActive == input$phys_active)
    }
    data
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes_string(x = input$xvar, y = "Testosterone")) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      geom_smooth(method = "lm", color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
      labs(
        x = input$xvar,
        y = "Testosterone (ng/dL)",
        title = paste("Testosterone vs", input$xvar),
        subtitle = "Fitted regression line with 95% confidence interval"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Pearson correlation + p-value (precise)
  output$corrText <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 2) {
      test <- cor.test(data[[input$xvar]], data$Testosterone, use = "complete.obs")
      r_val <- signif(test$estimate, 4)
      p_val <- signif(test$p.value, 4)
      
      cat("Pearson correlation between", input$xvar, "and Testosterone:\n")
      cat("r =", format(r_val, digits = 4), ", p =", format(p_val, scientific = TRUE), "\n")
      
      if (p_val < 0.05) {
        cat("→ Statistically significant (p < 0.05)\n")
      } else {
        cat("→ Not statistically significant\n")
      }
      
      abs_r <- abs(as.numeric(r_val))
      if (abs_r < 0.3) {
        cat("Interpretation: Weak relationship.\n")
      } else if (abs_r < 0.5) {
        cat("Interpretation: Moderate relationship.\n")
      } else {
        cat("Interpretation: Strong relationship.\n")
      }
    } else {
      cat("Not enough data points to calculate correlation.")
    }
  })
  
  # Regression model
  model_result <- eventReactive(input$run_model, {
    data <- filtered_data()
    all_vars <- c(input$predictors, input$controls)
    if (length(all_vars) == 0) return(NULL)
    
    formula_str <- paste("Testosterone ~", paste(all_vars, collapse = " + "))
    lm(as.formula(formula_str), data = data)
  })
  
  # Display regression output
  output$modelSummary <- renderPrint({
    model <- model_result()
    if (!is.null(model)) summary(model)
  })
}

# ------------------------------------------------------------
# RUN APP
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)
