#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
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
