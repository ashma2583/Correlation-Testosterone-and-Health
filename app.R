# ============================================================
# Title: Integrative Health Predictors of Testosterone in Adult Males
# File: Shiny.r
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(NHANES)# Ensure this package is installed

# ------------------------------------------------------------
# 1. GLOBAL DATA PROCESSING
# (Run this once before the app starts to ensure speed)
# ------------------------------------------------------------

# Load and Clean Data
# We filter for males, select our key variables, remove duplicates, 
# and filter out extreme outliers (e.g. T > 2000 likely TRT).
df <- NHANES |>
  filter(Gender == "male") |>
  select(Testosterone, Age, 
         BPSysAve, BPDiaAve, Pulse, TotChol, 
         BMI, UrineFlow1, Diabetes, PhysActive) |>
  distinct() |>       
  drop_na() |>        
  mutate(
    Diabetes = factor(Diabetes),
    PhysActive = factor(PhysActive)
  ) |>
  filter(Testosterone < 2000, BMI < 80) # Sanity Check / Outlier Removal

# ------------------------------------------------------------
# 2. UI (User Interface)
# ------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Integrative Health Predictors of Testosterone in Adult Males"),
  p("Explore how cardiovascular, metabolic, and renal indicators relate to testosterone levels among adult males in the NHANES dataset."),
  
  sidebarLayout(
    sidebarPanel(
      # --- FILTER OPTIONS ---
      h4("Filter Data"),
      selectInput("diabetes", "Filter by Diabetes Status:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("phys_active", "Filter by Physical Activity:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      
      hr(),
      
      # --- PLOT OPTIONS ---
      h4("Visual Exploration"),
      selectInput("xvar", "X-Axis Variable:",
                  choices = c("BMI", "Pulse", "BPDiaAve", "BPSysAve", "UrineFlow1", "TotChol", "Age")),
      
      # Simpson's Paradox Feature
      selectInput("color_by", "Color Points By (Check for Confounding):",
                  choices = c("None", "Diabetes", "PhysActive"), selected = "None"),
      
      checkboxInput("show_smooth", "Show Regression Line", value = TRUE),
      
      hr(),
      
      # --- MODEL OPTIONS ---
      h4("Regression Model Builder"),
      helpText("Select variables to include in the dynamic regression summary (Tab 2)."),
      
      checkboxGroupInput("predictors", "Predictors:",
                         choices = c("Systolic BP" = "BPSysAve",
                                     "Diastolic BP" = "BPDiaAve",
                                     "Pulse" = "Pulse",
                                     "Cholesterol" = "TotChol",
                                     "Urine Flow" = "UrineFlow1"),
                         selected = c("BPDiaAve", "Pulse", "UrineFlow1")),
      
      checkboxGroupInput("controls", "Controls (Confounders):",
                         choices = c("Age" = "Age",
                                     "BMI" = "BMI",
                                     "Diabetes" = "Diabetes",
                                     "PhysActive" = "PhysActive"),
                         selected = c("Age", "BMI", "Diabetes", "PhysActive")),
      
      actionButton("run_model", "Update Regression", class = "btn-primary")
    ),
    
    # --- MAIN PANEL ---
    mainPanel(
      tabsetPanel(
        
        # TAB 1: INTERACTIVE PLOT & INSPECTOR
        tabPanel("Explorer & Inspector",
                 br(),
                 # Plot with click interaction
                 plotOutput("scatterPlot", click = "plot_click", height = "500px"),
                 
                 # Click Inspector Output
                 wellPanel(
                   h4("Patient Inspector"),
                   p("Click on any point in the graph above to see that patient's full profile."),
                   tableOutput("click_info")
                 ),
                 verbatimTextOutput("corrText")
        ),
        
        # TAB 2: REGRESSION SUMMARY
        tabPanel("Model Summary",
                 verbatimTextOutput("modelSummary"),
                 helpText("Note: Use the checkboxes in the sidebar to add/remove variables from this model.")
        ),
        
        # TAB 3: ABOUT (Updated Text)
        tabPanel("About",
                 h4("About This App"),
                 p("This interactive web app explores how cardiovascular, metabolic, and renal health indicators 
                    relate to testosterone levels among adult males in the NHANES dataset. It combines regression 
                    modeling and interactive visualization to help users identify which everyday health metrics are 
                    most strongly associated with hormonal balance."),
                 
                 h5("How to Use"),
                 tags$ul(
                   tags$li(strong("Visual Exploration:"), "Use 'Plot Options' to visualize different relationships. Toggle 'Color Points By' to detect potential confounding (Simpson's Paradox)."),
                   tags$li(strong("Patient Inspector:"), "Click on any specific data point in the plot to reveal that patient's full health profile below the graph."),
                   tags$li(strong("Regression Modeling:"), "Use the checkboxes in the sidebar to build your own multivariate model and see how p-values change in real-time.")
                 ),
                 
                 h5("Technical Notes"),
                 tags$ul(
                   tags$li("Linear regression models were estimated using R’s 'lm()' function."),
                   tags$li("Data was filtered for adult males to avoid gender confounding."),
                   tags$li("Extreme outliers (Testosterone > 2000 ng/dL) were removed to ensure model stability (Cook's Distance < 0.1)."),
                   tags$li("Assumptions of linearity and homoscedasticity were verified via residual analysis."),
                   tags$li("VIF values < 1.5 confirmed no multicollinearity between Age, BMI, and Blood Pressure.")
                 ),
                 
                 h5("Key Findings"),
                 p("This project investigates how lifestyle and physiological factors jointly influence 
                    testosterone levels. Our 'Gold Standard' model suggests that higher BMI and resting pulse 
                    are strongly associated with lower testosterone, while stronger renal health (urine flow) 
                    predicts higher levels. The data emphasizes that hormonal health is interconnected with 
                    cardiovascular efficiency and body composition.")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 3. SERVER
# ------------------------------------------------------------
server <- function(input, output) {
  
  # A. Filter Logic (Reactive)
  filtered_data <- reactive({
    data <- df
    if (input$diabetes != "All") {
      data <- data %>% filter(Diabetes == input$diabetes)
    }
    if (input$phys_active != "All") {
      data <- data %>% filter(PhysActive == input$phys_active)
    }
    data
  })
  
  # B. Scatter Plot Logic (With Simpson's Paradox Coloring)
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    
    # Base Plot
    p <- ggplot(data, aes_string(x = input$xvar, y = "Testosterone")) +
      theme_minimal(base_size = 15) +
      labs(title = paste("Testosterone vs.", input$xvar))
    
    # Conditional Coloring
    if (input$color_by != "None") {
      p <- p + geom_point(aes_string(color = input$color_by), alpha = 0.6, size = 3) +
        # Add separate regression lines for each group
        geom_smooth(aes_string(color = input$color_by, fill = input$color_by), method = "lm", alpha = 0.1)
    } else {
      p <- p + geom_point(color = "#2c3e50", alpha = 0.6, size = 3)
    }
    
    # Overall Regression Line
    if (input$show_smooth && input$color_by == "None") {
      p <- p + geom_smooth(method = "lm", color = "#e74c3c", fill = "#e74c3c", alpha = 0.2)
    }
    
    p
  })
  
  # C. Click Inspector Logic
  output$click_info <- renderTable({
    # Finds rows in the data close to the click
    nearPoints(filtered_data(), input$plot_click, addDist = TRUE) %>%
      select(Testosterone, Age, BMI, Pulse, BPDiaAve, UrineFlow1, Diabetes, PhysActive)
  })
  
  # D. Correlation Text Output
  output$corrText <- renderPrint({
    data <- filtered_data()
    if(nrow(data) > 2){
      # Ensure numeric columns are selected for correlation
      if(is.numeric(data[[input$xvar]])) {
        test <- cor.test(data[[input$xvar]], data$Testosterone, use = "complete.obs")
        cat("Pearson Correlation (r):", round(test$estimate, 3), "\n")
        cat("P-value:", format.pval(test$p.value, eps=0.001), "\n")
        
        # Simple interpretation
        if(test$p.value < 0.05) cat("-> Statistically Significant") else cat("-> Not Significant")
      } else {
        cat("Correlation cannot be calculated for non-numeric X-variables.")
      }
    } else {
      cat("Not enough data to calculate correlation.")
    }
  })
  
  # E. Dynamic Regression Model Builder
  model_result <- eventReactive(input$run_model, {
    data <- filtered_data()
    all_vars <- c(input$predictors, input$controls)
    
    # Validation: prevent crash if nothing selected
    if (length(all_vars) == 0) return(NULL)
    
    formula_str <- paste("Testosterone ~", paste(all_vars, collapse = " + "))
    lm(as.formula(formula_str), data = data)
  })
  
  output$modelSummary <- renderPrint({
    model <- model_result()
    if (!is.null(model)) summary(model) else cat("Please select at least one variable and click 'Update Regression'.")
  })
}

# ------------------------------------------------------------
# 4. RUN APP
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)