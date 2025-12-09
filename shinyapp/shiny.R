# ============================================================
# Title: The Cardiometabolic Profile of Low Testosterone in Adult Males
# Dataset: cleaned_health_data (from NHANES)
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)

# ------------------------------------------------------------
# DATA PREPARATION
# ------------------------------------------------------------
cleaned_health_data <- NHANES |>
  filter(Gender == "male", !is.na(Testosterone)) |>
  select(Testosterone, 
         Age, BPSysAve, BPDiaAve, Pulse, TotChol, 
         BMI, Weight, UrineFlow1, 
         Diabetes, PhysActive, HealthGen) |>
  distinct() |>
  drop_na() |>
  mutate(
    Diabetes = factor(Diabetes, levels = c("No", "Yes")),
    PhysActive = factor(PhysActive, levels = c("No", "Yes")),
    HealthGen = factor(HealthGen, 
                       levels = c("Poor", "Fair", "Good", "Vgood", "Excellent"))
  )

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("The Cardiometabolic Profile of Low Testosterone in Adult Males"),
  p("Explore how physiological and lifestyle factors—such as BMI, Pulse, Blood Pressure, and Physical Activity—
    relate to measured testosterone levels among adult males in NHANES."),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Options"),
      selectInput("diabetes", "Filter by Diabetes Status:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("phys_active", "Filter by Physical Activity:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      
      selectInput("xvar", "Select X Variable:",
                  choices = c("Age", "BMI", "Weight", "Pulse", 
                              "BPDiaAve", "BPSysAve", "UrineFlow1", "TotChol"),
                  selected = "BMI"),
      
      checkboxInput("logT", "Log-transform Testosterone", value = FALSE),
      hr(),
      
      h4("Model Options"),
      checkboxGroupInput("controls", "Add Control Variables:",
                         choices = c("Age", "BMI", "Weight", "Pulse", 
                                     "PhysActive", "Diabetes", 
                                     "TotChol", "BPDiaAve", "BPSysAve", 
                                     "UrineFlow1", "HealthGen"),
                         selected = c("BMI", "Pulse")),
      
      actionButton("run_model", "Run Regression", class = "btn-primary"),
      hr(),
      helpText("Data source: NHANES (U.S. National Health and Nutrition Examination Survey)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("scatterPlot"),
                 verbatimTextOutput("corrText")),
        tabPanel("Regression Summary",
                 h4("Current Model Formula:"),
                 verbatimTextOutput("formulaText"),
                 h4("Regression Output:"),
                 verbatimTextOutput("modelSummary")),
        tabPanel("About",
                 h4("About This App"),
                 p("This Shiny app visualizes the relationships between testosterone levels and key
                   physiological and lifestyle indicators from the NHANES dataset. It allows users
                   to subset the data interactively and view regression outputs in real time."),
                 p("The purpose is to highlight how cardiovascular, metabolic, and behavioral factors
                   together shape male hormonal health."))
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output) {
  
  # --- Reactive data filtering
  filtered_data <- reactive({
    data <- cleaned_health_data
    
    if (input$diabetes != "All") {
      data <- data %>% filter(Diabetes == input$diabetes)
    }
    if (input$phys_active != "All") {
      data <- data %>% filter(PhysActive == input$phys_active)
    }
    data
  })
  
  # --- Scatterplot
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    y_var <- if (input$logT) log(data$Testosterone) else data$Testosterone
    
    ggplot(data, aes_string(x = input$xvar, y = NULL)) +
      geom_point(aes(y = y_var), alpha = 0.6, color = "#2c3e50") +
      geom_smooth(aes(y = y_var), method = "lm", color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
      labs(
        x = input$xvar,
        y = ifelse(input$logT, "Log(Testosterone)", "Testosterone (ng/dL)"),
        title = paste("Testosterone vs", input$xvar),
        subtitle = "Filtered by user-selected health and lifestyle variables"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # --- Correlation text
  output$corrText <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 2) {
      y <- if (input$logT) log(data$Testosterone) else data$Testosterone
      cor_val <- cor(data[[input$xvar]], y, use = "complete.obs")
      cat("Correlation between", input$xvar, "and Testosterone:", round(cor_val, 3))
    } else {
      cat("Not enough data points to calculate correlation.")
    }
  })
  
  # --- Regression model (run on button click)
  model_result <- eventReactive(input$run_model, {
    data <- filtered_data()
    y <- if (input$logT) "log(Testosterone)" else "Testosterone"
    if (length(input$controls) == 0) {
      formula_str <- paste(y, "~", input$xvar)
    } else {
      formula_str <- paste(y, "~", paste(input$controls, collapse = " + "))
    }
    lm(as.formula(formula_str), data = data)
  })
  
  # --- Formula preview
  output$formulaText <- renderPrint({
    y <- if (input$logT) "log(Testosterone)" else "Testosterone"
    if (length(input$controls) == 0) {
      formula_str <- paste(y, "~", input$xvar)
    } else {
      formula_str <- paste(y, "~", paste(input$controls, collapse = " + "))
    }
    cat(formula_str)
  })
  
  # --- Regression summary
  output$modelSummary <- renderPrint({
    model <- model_result()
    if (!is.null(model)) summary(model)
  })
}

# ------------------------------------------------------------
# RUN APP
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)
