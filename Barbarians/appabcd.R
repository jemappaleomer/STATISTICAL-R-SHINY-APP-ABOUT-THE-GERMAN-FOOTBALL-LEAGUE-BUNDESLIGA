library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "STAT_292 Project"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Information About Us", tabName = "about_us", icon=icon("person")),
      menuItem("Information About the Project", tabName = "about_project", icon=icon("list-alt")),
      menuItem("Examine the Data", tabName = "examine_data", icon= icon("table")),
      menuItem("Descriptive Statistics", tabName = "descriptive_stats", icon=icon("chart-bar")),
      menuItem("Regression Analysis", tabName = "analysis", icon=icon("chart-line"),
               menuSubItem("Linear Regression", tabName = "simple"),
               menuSubItem("Logistic Regression", tabName = "logistic")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar"),
               menuSubItem("Create a Bar Graph", tabName = "bar_graph"),
               menuSubItem("Create a Regression Line", tabName = "regression_line")),
      menuItem("References", tabName = "references", icon=icon("book")))
  ),
  dashboardBody(
    theme = shinytheme("spacelab"),
    tabItems(
      tabItem(
        tabName = "bar_graph",
        fluidRow(
          column(
            width = 6,
            selectInput("x_var", "X-axis Variable", choices = NULL)),
          
          column(
            width = 6,
            selectInput("y_var", "Y-axis Variable", choices = NULL)),
          
          actionButton("create_bar_graph", "Create Bar Graph", class = "btn-primary"),
          plotOutput("barplot", height = "400px"))),
      
      tabItem(
        tabName = "regression_line",
        fluidRow(
          column(
            width = 6,
            selectInput("x_var_reg", "X-axis Variable", choices = NULL)),
          
          column(
            width = 6,
            selectInput("y_var_reg", "Y-axis Variable", choices = NULL)),
          
          actionButton("create_regression_line", "Create Regression Line", class = "btn-primary"),
          plotOutput("regression_plot", height = "400px"))),
      
      tabItem(
        tabName = "examine_data",
        DTOutput("data_table")),
      
      tabItem(
        tabName = "about_us",
        fluidRow(
          column(
            width = 12,
            h2("About Us"),
            h3("           WE R THE BARBARIANS" , style = "font-size:75px"),
            p("Ömer Faruk KALE", style = "font-size:50px"), 
            p("Boran Aslan", style = "font-size:50px"),
            p("Efe İlker Ekizoğlu", style = "font-size:50px"),
            p("Kaan Metin",style = "font-size:50px"),
            p("Çağdaş Kadıoğlu", style = "font-size:50px")))),
      
      tabItem(
        tabName = "about_project",
        fluidRow(
          column(
            width = 12,
            h2("About the Project"),
            h3("Football analysis has evolved significantly in recent years, with the introduction of advanced statistics 
               and metrics that provide valuable insights into the game. This presentation aims to shed light on three 
               important terms in football analysis: xG (expected goals), Deep, and PPDA (passes allowed per 
               defensive action), and explore their relationships with each other, specifically with the help of 
               Statistical visualization and analysis.", style = "font-size:30px"))),
            h3("
Expected Goals (xG) is a statistical metric used in football analysis to assess the likelihood of a shot resulting in a goal. It considers factors such as shot location, type, and assist type, providing a more accurate evaluation of a team's attacking performance. 
"),
      h3("The term Deep refers to passes played into the final third of the pitch, creating goal-scoring opportunities by penetrating the opposition's defense. Analyzing the number of deep passes made by a team or player helps evaluate their ability to break down defenses effectively."),
      h3("PPDA (Passes Allowed per Defensive Action) coefficient is a statistical measure that quantifies a team's pressing intensity and defensive efficiency. It calculates the average number of opponent passes allowed before a defensive action, such as a tackle or interception. A low PPDA indicates an aggressive and effective pressing strategy, while a high PPDA suggests a passive or ineffective defensive approach."),
      h3("np -> Non-Penalty
"),
      h3("xGA -> xG Against
"),
      h3("oppda -> opposite team ppda")),
      
      tabItem(
        tabName = "descriptive_stats",
        fluidRow(
          column(
            width = 12,
            h2("Descriptive Statistics"),
            uiOutput("variable_selection"),
            DTOutput("stats_table")))),
      
      tabItem(
        tabName = "references",
        fluidRow(
          column(
            width = 12,
            h2("References"),
            p("(n.d.). Quantargo Icons. Quantargo. https://www.quantargo.com/help/r/latest/packages/shiny/1.6.0/icon"),
            p("(n.d.). Bundeliga's Data. Understat. https://understat.com/league/bundesliga")))),
      
      tabItem(
        tabName = "simple",
        fluidRow(
          column(
            width = 12,
            h2("Linear Regression Analysis"),
            p("Simple and Multiple Linear Regression can be analyzed in this part interactively. 
              The equations, models and predictions of them can be reach by selecting related variables. 
              Also, the predicted value can be observed by assigning a value to the independent variable.")),
          column(
            width = 6,
            selectInput("independent_vars1", "Independent Variable (x)", choices = NULL, multiple = TRUE)),
          column(
            width = 6,
            selectInput("dependent_var1", "Dependent Variable (y)", choices = NULL)),
          actionButton("perform_simple_regression", "Perform Linear Regression", class = "btn-primary"),
          h3("Linear Regression Results"),
          textOutput("smpl_equation"),
          verbatimTextOutput("smpl_summary"),
          h3("Prediction"),
          uiOutput("prediction_inputs1"),
          actionButton("perform_prediction1", "Perform Prediction", class = "btn-primary"),
          h3("Prediction Result"),
          textOutput("prediction_result1"))),
      
      tabItem(
        tabName = "logistic",
        fluidRow(
          column(
            width = 12,
            h2("Logistic Regression Analysis"),
            h3("Logistic Regression Analysis can be realized thanks to this part. 
              Models and linear equations of the logistic regression can be obtained by selecting 
              related variables. Also, the predicted value can be reach by assigning a value to the independent
              variable like previous part.")),
          column(
            width = 6,
            selectInput("dependent_var", "Dependent Variable (y)", choices = NULL)),
          column(
            width = 6,
            selectInput("independent_vars", "Independent Variables (x)", choices = NULL, multiple = TRUE)),
          actionButton("perform_logistic_regression", "Perform Logistic Regression", class = "btn-primary"),
          h3("Logistic Regression Results"),
          textOutput("logreg_equation"),
          verbatimTextOutput("logreg_summary"),
          h3("Prediction"),
          uiOutput("prediction_inputs"),
          actionButton("perform_prediction", "Perform Prediction", class = "btn-primary"),
          h3("Prediction Result"),
          textOutput("prediction_result"))))))

server <- function(input, output, session) {
  
  
  data <- reactive({
    read.csv("understat_bundesliga1820.csv")
  })
  
  
  observeEvent(data(), {
    updateSelectInput(session, "x_var", choices = names(data()))
    updateSelectInput(session, "y_var", choices = names(data()))
    updateSelectInput(session, "x_var_reg", choices = names(data()))
    updateSelectInput(session, "y_var_reg", choices = names(data()))
    updateSelectInput(session, "independent_var", choices = names(data()))
    updateSelectInput(session, "dependent_var", choices = names(data()))
    updateSelectInput(session, "independent_var1", choices = names(data()))
    updateSelectInput(session, "dependent_var1", choices = names(data()))
  })
  
  
  output$variable_selection <- renderUI({
    selectInput(
      "selected_variables",
      "Select Variables for Descriptive Statistics:",
      choices = names(data()),
      multiple = TRUE
    )
  })
  
  
  output$barplot <- renderPlot({
    req(input$create_bar_graph, data(), input$x_var, input$y_var)
    
    
    grouped_data <- aggregate(data()[[input$y_var]], by = list(data()[[input$x_var]]), FUN = mean)
    
    
    sorted_data <- grouped_data[order(grouped_data$x, decreasing = TRUE), ]
    
    
    barplot(sorted_data$x, names.arg = sorted_data$Group.1, 
            main = "Bar Graph (Average)", xlab = input$x_var, ylab = paste("Average ", input$y_var),
            col = "lightgreen")
  })
  
  
  output$regression_plot <- renderPlot({
    req(input$create_regression_line, data(), input$x_var_reg, input$y_var_reg)
    plot(data()[[input$x_var_reg]], data()[[input$y_var_reg]], 
         main = "Regression Line", xlab = input$x_var_reg, ylab = input$y_var_reg)
    abline(lm(data()[[input$y_var_reg]] ~ data()[[input$x_var_reg]]), col = "red")
  })
  
  
  output$data_table <- renderDT({
    req(data())
    datatable(data())
  })
  
  output$stats_table <- renderDT({
    req(data(), input$selected_variables)
    
    selected_vars <- input$selected_variables
    
    stats <- sapply(selected_vars, function(var) {
      summary(data()[[var]])
    })
    
    stats <- as.data.frame(t(stats))
    datatable(stats, caption = "Descriptive Statistics")
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "dependent_var", choices = names(data()))
    updateSelectInput(session, "independent_vars", choices = names(data()), selected = NULL)
  })
  
  output$prediction_inputs <- renderUI({
    req(data(), input$independent_vars)
    
    inputs <- lapply(input$independent_vars, function(var) {
      numericInput(paste0("prediction_value_", var), paste("Enter the value for", var), value = NULL)
    })
    
    do.call(tagList, inputs)
  })
  
  output$logreg_equation <- renderText({
    req(input$perform_logistic_regression, data(), input$dependent_var, input$independent_vars)
    
    filtered_data <- na.omit(data()[c(input$dependent_var, input$independent_vars)])
    
    formula <- as.formula(paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + ")))
    model <- glm(formula, data = filtered_data, family = binomial)
    
    coef <- coef(model)
    
    equation1 <- paste("Equation of the Linear Model: ŷ= ",
                       paste(round(coef[1],3), "+", paste(round(coef[-1],3), names(coef[-1]), sep = "*", collapse = " + "), sep = ""))
    
    equation1
  })
  
  output$logreg_summary <- renderPrint({
    req(input$perform_logistic_regression, data(), input$dependent_var, input$independent_vars)
    
    filtered_data <- na.omit(data()[c(input$dependent_var, input$independent_vars)])
    
    formula <- as.formula(paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + ")))
    model <- glm(formula, data = filtered_data, family = binomial)
    
    summary(model)
  })
  
  output$prediction_result <- renderText({
    req(input$perform_prediction, data(), input$independent_vars)
    
    filtered_data <- na.omit(data()[c(input$dependent_var, input$independent_vars)])
    
    formula <- as.formula(paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + ")))
    model <- glm(formula, data = filtered_data, family = binomial)
    
    coef <- coef(model)
    
    prediction_values <- sapply(input$independent_vars, function(var) input[[paste0("prediction_value_", var)]])
    prediction <- plogis(coef[1] + sum(coef[-1] * prediction_values))
    
    prediction_result <- paste("The predicted outcome for the given independent variable values is:", round(prediction,5))
    
    prediction_result
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "dependent_var1", choices = names(data()))
    updateSelectInput(session, "independent_vars1", choices = names(data()), selected = NULL)
  })
  
  output$prediction_inputs1 <- renderUI({
    req(data(), input$independent_vars1)
    
    inputs1 <- lapply(input$independent_vars1, function(var) {
      numericInput(paste0("prediction_value_", var), paste("Enter the value for", var), value = NULL)
    })
    
    do.call(tagList, inputs1)
  })
  
  output$smpl_equation <- renderText({
    req(input$perform_simple_regression, data(), input$independent_vars1, input$dependent_var1)
    
    filtered_data1 <- na.omit(data()[c(input$independent_vars1, input$dependent_var1)])
    
    formula1 <- as.formula(paste(input$dependent_var1, "~", paste(input$independent_vars1, collapse = " + ")))
    model1 <- lm(formula1, data=filtered_data1)
    
    coef1 <- coef(model1)
    
    equation1 <- paste("Linear Regression Equation: ŷ= ",
                       paste(round(coef1[1],3), "+", paste(round(coef1[-1],3), names(coef1[-1]), sep = "*", collapse = " + "), sep = ""))
    
    equation1
  })
  
  output$smpl_summary <- renderPrint({
    req(input$perform_simple_regression, data(), input$independent_vars1, input$dependent_var1)
    
    filtered_data1 <- na.omit(data()[c(input$independent_vars1, input$dependent_var1)])
    
    formula1 <- as.formula(paste(input$dependent_var1, "~", paste(input$independent_vars1, collapse = " + ")))
    model1 <- lm(formula1, data = filtered_data1)
    
    summary(model1)
  })
  
  output$prediction_result1 <- renderText({
    req(input$perform_prediction1, data(), input$independent_vars1)
    
    filtered_data1 <- na.omit(data()[c(input$independent_vars1, input$dependent_var1)])
    
    formula1 <- as.formula(paste(input$dependent_var1, "~", paste(input$independent_vars1, collapse = " + ")))
    model1 <- lm(formula1, data = filtered_data1)
    
    coef1 <- coef(model1)
    
    
    prediction_values1 <- sapply(input$independent_vars1, function(var) input[[paste0("prediction_value_", var)]])
    prediction1 <- (coef1[1] + sum(coef1[-1] * prediction_values1))
    
    prediction_result1 <- paste("The predicted outcome for the given independent variable values is:", round(prediction1,3))
    
    prediction_result1
  })
}

shinyApp(ui, server)
