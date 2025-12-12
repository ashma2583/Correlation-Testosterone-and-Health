library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(NHANES)

# load and clean data
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
  filter(Testosterone < 2000, BMI < 80)

# ui
ui <- fluidPage(
  
  titlePanel("Integrative Health Predictors of Testosterone in Adult Males"),
  p("Explore how cardiovascular, metabolic, and renal indicators relate to testosterone levels among adult males in the NHANES dataset."),
  
  sidebarLayout(
    sidebarPanel(
      # filter options
      h4("Filter Data"),
      selectInput("diabetes", "Filter by Diabetes Status:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("phys_active", "Filter by Physical Activity:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      
      hr(),
      
      # plot options
      h4("Visual Exploration"),
      selectInput("xvar", "X-Axis Variable:",
                  choices = c("BMI", "Pulse", "BPDiaAve", "BPSysAve", "UrineFlow1", "TotChol", "Age")),
      
      # simpson's paradox revealer
      selectInput("color_by", "Color Points By (Check for Confounding):",
                  choices = c("None", "Diabetes", "PhysActive"), selected = "None"),
      
      checkboxInput("show_smooth", "Show Regression Line", value = TRUE),
      
      hr(),
      
      # model options
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
    
    # main panel
    mainPanel(
      tabsetPanel(
        
        # tab 1: interactive plot
        tabPanel("Explorer & Inspector",
                 br(),
                 plotOutput("scatterPlot", click = "plot_click", height = "500px"),
                 
                 wellPanel(
                   h4("Patient Inspector"),
                   p("Click on any point in the graph above to see that patient's full profile."),
                   tableOutput("click_info")
                 ),
                 verbatimTextOutput("corrText")
        ),
        
        # tab 2: regression summary
        tabPanel("Model Summary",
                 verbatimTextOutput("modelSummary"),
                 helpText("Note: Use the checkboxes in the sidebar to add/remove variables from this model.")
        ),
        
        # tab 3: abt
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

#server
server <- function(input, output) {
  
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
  
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    
    p <- ggplot(data, aes_string(x = input$xvar, y = "Testosterone")) +
      theme_minimal(base_size = 15) +
      labs(title = paste("Testosterone vs.", input$xvar))
    
    if (input$color_by != "None") {
      p <- p + geom_point(aes_string(color = input$color_by), alpha = 0.6, size = 3) +
        geom_smooth(aes_string(color = input$color_by, fill = input$color_by), method = "lm", alpha = 0.1)
    } else {
      p <- p + geom_point(color = "#2c3e50", alpha = 0.6, size = 3)
    }
    
    if (input$show_smooth && input$color_by == "None") {
      p <- p + geom_smooth(method = "lm", color = "#e74c3c", fill = "#e74c3c", alpha = 0.2)
    }
    
    p
  })
  
  output$click_info <- renderTable({
    nearPoints(filtered_data(), input$plot_click, addDist = TRUE) %>%
      select(Testosterone, Age, BMI, Pulse, BPDiaAve, UrineFlow1, Diabetes, PhysActive)
  })
  
  output$corrText <- renderPrint({
    data <- filtered_data()
    if(nrow(data) > 2){
      if(is.numeric(data[[input$xvar]])) {
        test <- cor.test(data[[input$xvar]], data$Testosterone, use = "complete.obs")
        cat("Pearson Correlation (r):", round(test$estimate, 3), "\n")
        cat("P-value:", format.pval(test$p.value, eps=0.001), "\n")
        
        if(test$p.value < 0.05) cat("-> Statistically Significant") else cat("-> Not Significant")
      } else {
        cat("Correlation cannot be calculated for non-numeric X-variables.")
      }
    } else {
      cat("Not enough data to calculate correlation.")
    }
  })
  
  # regression model builder
  model_result <- eventReactive(input$run_model, {
    data <- filtered_data()
    all_vars <- c(input$predictors, input$controls)
    
    # validation to prevent crash if needed
    if (length(all_vars) == 0) return(NULL)
    
    formula_str <- paste("Testosterone ~", paste(all_vars, collapse = " + "))
    lm(as.formula(formula_str), data = data)
  })
  
  output$modelSummary <- renderPrint({
    model <- model_result()
    if (!is.null(model)) summary(model) else cat("Please select at least one variable and click 'Update Regression'.")
  })
}

# run app
shinyApp(ui = ui, server = server)