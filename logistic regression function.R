###############################################################
#Function to calculate true parameters
lr <- function(formula = NULL, data = NULL, X = NULL, y = NULL, B = 20, alpha = 0.05) {
  
  # Store factor level mapping information
  factor_mappings <- list()
  
  # Determine if the user provided a formula or X and y directly
  if (!is.null(formula) && !is.null(data)) {
    #Store factors
    for (col_name in names(data)) {
      col <- data[[col_name]]
      if (is.character(col) || is.factor(col)) {
        levels_info <- levels(as.factor(col))
        factor_mappings[[col_name]] <- data.frame(
          Level = levels_info,
          Numeric = as.numeric(as.factor(levels_info)) - 1
        )
        data[[col_name]] <- as.numeric(as.factor(col)) - 1
      }
    }
    
    # Parse formula to extract response and predictor variables
    mf <- model.frame(formula, data)
    y <- model.response(mf)
    X <- model.matrix(formula, data)
    
    # Remove the intercept from the model matrix since we'll add it manually
    X <- X[, -1, drop = FALSE]
  } else if (!is.null(X) && !is.null(y)) {
    # Make sure X is a data frame for easier column-wise conversion
    if (!is.data.frame(X)) {
      X <- as.data.frame(X)
    }
    
    # Convert character or factor covariates to dummy variables
    for (col_name in names(X)) {
      col <- X[[col_name]]
      if (is.character(col) || is.factor(col)) {
        levels_info <- levels(as.factor(col))
        factor_mappings[[col_name]] <- data.frame(
          Level = levels_info,
          Numeric = as.numeric(as.factor(levels_info)) - 1
        )
        X[[col_name]] <- as.numeric(as.factor(col)) - 1
      }
    }
    
    # Convert character or factor covariates to dummy variables
    X <- model.matrix(~ . - 1, data = X)
  } else {
    stop("Please provide either a formula and data, or X and y.")
  }
  
  # Convert dependent variable to binary if it is in character or factor form
  if (is.character(y) || is.factor(y)) {
    unique_levels <- unique(y)
    if (length(unique_levels) != 2) {
      stop("The dependent variable must have exactly two levels to be used in logistic regression.")
    }
    # Convert character or factor levels to 0 and 1
    y <- as.numeric(y == unique_levels[2])
  } else {
    # Convert numeric dependent variable to 0 and 1 if it has more than two unique values
    unique_levels <- unique(y)
    if (length(unique_levels) != 2) {
      stop("The dependent variable must have exactly two levels to be used in logistic regression.")
    }
    y <- as.numeric(y == max(unique_levels))
  }
  
  # Ensure y values are binary (0 or 1)
  if (any(y < 0 | y > 1)) {
    stop("The dependent variable must have values between 0 and 1.")
  }
  
  # Create design matrix with intercept
  design <- cbind(Intercept = rep(1, nrow(X)), X)
  
  # Initialize beta with least squares formula
  beta_init <- solve(t(design) %*% design) %*% t(design) %*% y
  
  # Define the negative log-likelihood
  neg_log_likelihood <- function(beta) {
    p <- 1 / (1 + exp(-design %*% beta))
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }
  
  # Optimization using optim
  result <- optim(beta_init, neg_log_likelihood)
  
  # Create bootstrap data matrix with y and X
  booth_data <- cbind(y, design)
  
  n <- nrow(booth_data)
  
  # Matrices to store bootstrap coefficients
  B_hat <- B_hat2 <- matrix(NA, nrow = B, ncol = ncol(booth_data) - 1)
  
  # Loop to create bootstrap samples
  for (i in 1:B) {
    # Sampling the data for bootstrap
    bdata <- as.matrix(booth_data[sample(1:n, n, replace = TRUE), ])
    
    # Separating covariates
    Xs <- bdata[, -1]
    
    # Separating dependent variable
    ys <- bdata[, 1]
    
    # Calculating coefficients of sampled data
    beta_init2 <- solve(t(Xs) %*% Xs) %*% t(Xs) %*% ys
    
    # Calculating coefficients with optimization
    boot_lm <- optim(beta_init2, neg_log_likelihood)
    
    # Calculating coefficients with glm
    boot_lm2 <- suppressWarnings(glm(ys ~ Xs[, -1], family = binomial))
    
    # Storing bootstrap coefficients
    B_hat[i, ] <- boot_lm$par
    
    # Storing glm coefficients
    B_hat2[i, ] <- boot_lm2$coefficients
  }
  
  # Confidence interval based on bootstrap
  CI <- matrix(NA, nrow = ncol(booth_data) - 1, ncol = 2)
  
  # Loop to calculate CI for coefficients obtained with bootstrap
  for (i in 1:ncol(B_hat)) {
    CI[i, ] <- quantile(B_hat[, i], c(alpha / 2, 1 - alpha / 2))
  }
  
  
  # Predict probabilities based on optimized beta coefficients
  p_hat <- 1 / (1 + exp(-design %*% result$par))
  
  # Set threshold for classification 
  y_pred <- ifelse(p_hat >= 0.5, 1, 0)
  
  # Create confusion matrix
  true_positive <- sum(y == 1 & y_pred == 1)
  true_negative <- sum(y == 0 & y_pred == 0)
  false_positive <- sum(y == 0 & y_pred == 1)
  false_negative <- sum(y == 1 & y_pred == 0)
  
  confusion_matrix <- matrix(c(false_positive, true_positive, true_negative, false_negative), 
                             nrow = 2, 
                             dimnames = list(
                               "Actual" = c("FALSE", "TRUE"),
                               "Predicted" = c("TRUE", "FALSE")
                             ))
  
  # Calculate accuracy, sensitivity, specificity, false discovery rate, and diagnostic odds ratio
  prevalence <- mean(y)
  accuracy <- (true_positive + true_negative) / length(y)
  sensitivity <- true_positive / (true_positive + false_negative)
  specificity <- true_negative / (true_negative + false_positive)
  false_discovery_rate <- false_positive / (true_positive + false_positive)
  diagnostic_odds_ratio <- (true_positive / false_negative) / (false_positive / true_negative)
  
  return(list(beta_init = beta_init, 
              beta_optimized = result$par, 
              CI = CI, 
              confusion_matrix = confusion_matrix, 
              prevalence = prevalence, 
              accuracy = accuracy, 
              sensitivity = sensitivity, 
              specificity = specificity, 
              false_discovery_rate = false_discovery_rate, 
              diagnostic_odds_ratio = diagnostic_odds_ratio,
              factor_mappings = factor_mappings))
}


#' Logistic Regression with Numerical Optimization and Bootstrapping
#'
#' This function performs logistic regression using numerical optimization. It supports both formula-based 
#' and matrix-based inputs for data. The function also computes bootstrap confidence intervals, 
#' confusion matrix, and various classification metrics.
#'
#' @param formula A formula specifying the model structure (e.g., `y ~ x1 + x2`). Required if `data` is provided.
#' @param data A data frame containing the variables in the formula. Used only when `formula` is provided.
#' @param X A matrix or data frame of predictors. Used when `formula` is not provided.
#' @param y A vector of binary response variables (0/1 or factor with two levels). Used when `formula` is not provided.
#' @param B The number of bootstrap samples for computing confidence intervals. Default is 20.
#' @param alpha Significance level for confidence intervals. Default is 0.05 (95% confidence intervals).
#'
#' @return A list containing:
#' \item{beta_init}{Initial coefficient estimates from least squares.}
#' \item{beta_optimized}{Optimized coefficient estimates from numerical optimization.}
#' \item{CI}{Bootstrap confidence intervals for the coefficients.}
#' \item{confusion_matrix}{Confusion matrix showing predicted vs. actual classifications.}
#' \item{prevalence}{Proportion of positive cases in the response variable.}
#' \item{accuracy}{Proportion of correctly classified instances.}
#' \item{sensitivity}{Proportion of true positives correctly identified.}
#' \item{specificity}{Proportion of true negatives correctly identified.}
#' \item{false_discovery_rate}{Proportion of false positives among all predicted positives.}
#' \item{diagnostic_odds_ratio}{Diagnostic odds ratio for the classifier.}
#' \item{factor_mappings}{Mappings of factor levels to numeric values used in the model.}
#'
#' @examples
#' # Example 1: Using formula and data
#' data(iris)
#' iris_binary <- iris[iris$Species %in% c("setosa", "versicolor"), ]
#' iris_binary$Species <- factor(iris_binary$Species)
#' result <- lr(Species ~ Sepal.Length + Sepal.Width, data = iris_binary)
#' print(result$confusion_matrix)
#'
#' # Example 2: Using X and y directly
#' X <- iris_binary[, c("Sepal.Length", "Sepal.Width")]
#' y <- as.numeric(iris_binary$Species) - 1
#' result <- lr(X = X, y = y, B = 50, alpha = 0.01)
#' print(result$CI)
#'
#' @export

