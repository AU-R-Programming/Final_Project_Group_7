###############################################################
#Function to calculate true parameters
logistic_regression <- function(formula = NULL, data = NULL, X = NULL, y = NULL, B = 20, alpha = 0.05) {
  
  # Determine if the user provided a formula or X and y directly
  if (!is.null(formula) && !is.null(data)) {
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
    
    # Convert character or factor covariates to numeric
    X[] <- lapply(X, function(col) {
      if (is.character(col) || is.factor(col)) {
        return(as.numeric(as.factor(col)))
      } else {
        return(col)
      }
    })
    
    # Convert X back to matrix
    X <- as.matrix(X)
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
  
  # Create confidence interval (based on glm, just for comparison)
  CI2 <- matrix(NA, nrow = ncol(B_hat2), ncol = 2)
  
  # Loop to calculate CI for
  for (i in 1:ncol(B_hat2)) {
    CI2[i, ] <- quantile(B_hat2[, i], c(alpha / 2, 1 - alpha / 2))
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
                               "Actual" = c("Negative", "Positive"),
                               "Predicted" = c("Positive", "Negative")
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
              CI2 = CI2, 
              confusion_matrix = confusion_matrix, 
              prevalence = prevalence, 
              accuracy = accuracy, 
              sensitivity = sensitivity, 
              specificity = specificity, 
              false_discovery_rate = false_discovery_rate, 
              diagnostic_odds_ratio = diagnostic_odds_ratio))
}


  

vsmodel<-logistic_regression(X=X1, y=mtcars$vs, B=1000, alpha=0.05)
vsmodel$confusion_matrix
ammodel<-logistic_regression(X=X1, y=mtcars$am, B=1000, alpha=0.05)
ammodel$confusion_matrix

  
predict_lr <- function(model, new_data) {
  # Add intercept to new data
  new_design <- as.matrix(cbind(1, new_data))
  
  # Calculate predicted probabilities
  p_hat <- 1 / (1 + exp(-new_design %*% model$beta_optimized))
  
  # Set threshold for classification (e.g., 0.5)
  y_pred <- ifelse(p_hat >= 0.5, 1, 0)
  
  return(list(predicted_probabilities = p_hat, predicted_class = y_pred))
}

newdata<-matrix(c(22.8, 93, 2.32), ncol=3, nrow=1)
newdata<-matrix(c( 32, 1, 0))
predict_lr(lrm1, newdata)
predict_lr(vsmodel, newdata)
head(mtcars)
data(mtcars)


glm_fit <- glm(am ~ mpg + hp + wt, family = binomial, data = mtcars)
logistic_regression(X1, mtcars$vs, B=1000, alpha=0.05)
glm_fit$coefficients
covid<-covid[complete.cases(covid),]
risk_model<-glm(Outcome~Age+Hospitalization.type+Symptoms+Positive, data=covid, family=binomial)
summary(risk_model)
ys1<-as.matrix(as.numeric(covid$Outcome)-1)
Xs1<-as.matrix(cbind(covid$Age, as.factor(covid$Hospitalization.type), as.factor(covid$Positive)))
lrm1<-logistic_regression(X=Xs1, y=ys1, B=100, alpha=0.05)
lrm2<-logistic_regression(Outcome~Age+Hospitalization.type+Positive, data=covid, B=100, alpha=0.05)
lrm1$beta_optimized
lrm1$confusion_matrix
lrm2$confusion_matrix
table(covid_nona$Outcome, pred_death)
sum(ys1)

length(ys1)


