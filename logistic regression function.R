###############################################################
#Function to calculate true parameters
logistic_regression <- function(X, y, B=20, alpha=0.05) {
  
  #Create design matrix
  design<-cbind(rep(1, dim(X)[1]), X)
  
  # Initialize beta with least squares formula
  beta_init <- solve(t(design) %*% design) %*% t(design) %*% y
  
  # Define the negative log-likelihood
  neg_log_likelihood <- function(beta) {
    p <- 1 / (1 + exp(-design %*% beta))
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }
  
  # Optimization using optim
  result <- optim(beta_init, neg_log_likelihood)
  
  booth_data<-cbind(y, rep(1, nrow(X)), X)
  
  n=nrow(booth_data)

  #going to do. 1 to B
  B_hat=B_hat2=matrix(NA, nrow=B, ncol=ncol(booth_data)-1)
  
  #Loop to create bootstrap
  for (i in 1:B){
    #Sampling the data for bootstrap
    bdata<-as.matrix(booth_data[sample(1:nrow(booth_data), n, replace=T  ),])
    #Separating covariates
    Xs<-bdata[,2:ncol(bdata)]
    #Separating dependent variable
    ys<-bdata[,1]
    #Calculating coefficients of sampled data
    beta_init2 <- solve(t(Xs) %*% Xs) %*% t(Xs) %*% ys
    #Calculating coefficients with optimization 
    boot_lm<- optim(beta_init2, neg_log_likelihood)
    #Calculating coefficients with glm
    boot_lm2<-suppressWarnings(glm(bdata[,1]~bdata[,3:ncol(bdata)], family=binomial))
    #Storing bootstrap coefficients
    B_hat[i,]<-boot_lm$par
    #Storing glm coefficients
    B_hat2[i,]<-boot_lm2$coefficients
    
  }
  #Confidence interval based on bootstrap
  CI<-matrix(NA, nrow=ncol(booth_data)-1, ncol=2)
  #Loop to calculate CI for coefficients obtained with bootstrap
  for(i in 1:ncol(B_hat)){
    CI[i,]<-quantile(B_hat[,i],c(alpha/2, 1-alpha/2) )
  
      
  }
  #Create confidence interval (based on glm, just for comparison)
  CI2<-matrix(NA, nrow=ncol(booth_data)-1, ncol=2)
  #Loop to calculate CI for coefficients obtained with glm
  for(i in 1:ncol(B_hat)){
    CI2[i,]<-quantile(B_hat2[,i],c(alpha/2, 1-alpha/2) )
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
  
  confusion_matrix <- matrix(c(true_positive, false_negative, false_positive, true_negative), 
                             nrow = 2, 
                             dimnames = list(
                               "Actual" = c("Positive", "Negative"),
                               "Predicted" = c("Positive", "Negative")
                             ))
  
  
  # Calculate accuracy, sensitivity, specificity, false discovery rate, and diagnostic 
  #odds ratio
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

  

vsmodel<-logistic_regression(X1, mtcars$vs, B=1000, alpha=0.05)
  
predict_lr <- function(model, new_data) {
  # Add intercept to new data
  new_design <- cbind(1, new_data)
  
  # Calculate predicted probabilities
  p_hat <- 1 / (1 + exp(-new_design %*% model$beta_optimized))
  
  # Set threshold for classification (e.g., 0.5)
  y_pred <- ifelse(p_hat >= 0.5, 1, 0)
  
  return(list(predicted_probabilities = p_hat, predicted_class = y_pred))
}

newdata<-matrix(c(22.8, 93, 2.32), ncol=3, nrow=1)
predict_lr(vsmodel, newdata)
head(mtcars)
data(mtcars)


glm_fit <- glm(am ~ mpg + hp + wt, family = binomial, data = mtcars)
logistic_regression(X1, mtcars$vs, B=1000, alpha=0.05)
glm_fit$coefficients



