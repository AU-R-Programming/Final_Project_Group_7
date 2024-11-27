library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Supervised Binary Classification Using Numerical Optimization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV File", accept = c(".csv")),
      selectInput("response", "Select Response Variable:", choices = NULL),
      selectizeInput("predictors", "Select Predictor Variables:", choices = NULL, multiple = TRUE),
      numericInput("bootstrap", "Number of Bootstrap Iterations (B):", value = 1000, min = 100),
      numericInput("alpha", "Significance Level (Alpha):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
      actionButton("run", "Run Binary Classification"),
      downloadButton("downloadResults", "Download Results")
    ),
    
    mainPanel(
      h3("Results"),
      tabsetPanel(
        tabPanel("Coefficients", tableOutput("coefficients")),
        tabPanel("Confidence Intervals", tableOutput("confidence_intervals")),
        tabPanel("Confusion Matrix", tableOutput("confusion_matrix")),
        tabPanel("Performance Metrics", tableOutput("performance_metrics"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive to load data
  dataset <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  # Update UI choices for variables
  observe({
    data <- dataset()
    updateSelectInput(session, "response", choices = colnames(data))
    updateSelectizeInput(session, "predictors", choices = colnames(data))
  })
  
  # Function to calculate logistic regression
  logistic_regression <- function(X, y, B = 20, alpha = 0.05) {
    # Create design matrix
    design <- cbind(rep(1, dim(X)[1]), X)
    
    # Initialize beta with least squares formula
    beta_init <- solve(t(design) %*% design) %*% t(design) %*% y
    
    # Define the negative log-likelihood
    neg_log_likelihood <- function(beta) {
      p <- 1 / (1 + exp(-design %*% beta))
      -sum(y * log(p) + (1 - y) * log(1 - p))
    }
    
    # Optimization using optim
    result <- optim(beta_init, neg_log_likelihood)
    
    # Bootstrap confidence intervals
    booth_data <- cbind(y, rep(1, nrow(X)), X)
    n <- nrow(booth_data)
    B_hat <- matrix(NA, nrow = B, ncol = ncol(booth_data) - 1)
    
    for (i in 1:B) {
      bdata <- as.matrix(booth_data[sample(1:n, n, replace = TRUE), ])
      Xs <- bdata[, 2:ncol(bdata)]
      ys <- bdata[, 1]
      beta_init2 <- solve(t(Xs) %*% Xs) %*% t(Xs) %*% ys
      boot_lm <- optim(beta_init2, neg_log_likelihood)
      B_hat[i, ] <- boot_lm$par
    }
    
    CI <- apply(B_hat, 2, function(x) quantile(x, c(alpha / 2, 1 - alpha / 2)))
    
    # Predict probabilities and evaluate performance
    p_hat <- 1 / (1 + exp(-design %*% result$par))
    y_pred <- ifelse(p_hat >= 0.5, 1, 0)
    
    true_positive <- sum(y == 1 & y_pred == 1)
    true_negative <- sum(y == 0 & y_pred == 0)
    false_positive <- sum(y == 0 & y_pred == 1)
    false_negative <- sum(y == 1 & y_pred == 0)
    
    confusion_matrix <- matrix(c(true_positive, false_negative, false_positive, true_negative), 
                               nrow = 2, 
                               dimnames = list(
                                 Actual = c("Positive", "Negative"),
                                 Predicted = c("Positive", "Negative")
                               ))
    
    accuracy <- (true_positive + true_negative) / length(y)
    sensitivity <- true_positive / (true_positive + false_negative)
    specificity <- true_negative / (true_negative + false_positive)
    
    return(list(
      beta = result$par,
      CI = CI,
      confusion_matrix = confusion_matrix,
      metrics = data.frame(
        Accuracy = accuracy,
        Sensitivity = sensitivity,
        Specificity = specificity
      )
    ))
  }
  
  # Run logistic regression and store results
  results <- eventReactive(input$run, {
    data <- dataset()
    X <- as.matrix(data[, input$predictors])
    y <- as.numeric(data[[input$response]])
    logistic_regression(X, y, B = input$bootstrap, alpha = input$alpha)
  })
  
  # Output results
  output$coefficients <- renderTable({
    req(results())
    data.frame(Coefficients = results()$beta)
  })
  
  output$confidence_intervals <- renderTable({
    req(results())
    CI <- results()$CI
    t(CI)
  })
  
  output$confusion_matrix <- renderTable({
    req(results())
    results()$confusion_matrix
  })
  
  output$performance_metrics <- renderTable({
    req(results())
    results()$metrics
  })
  
  # Download results
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("logistic_regression_results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(results())
      write.csv(results()$metrics, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
