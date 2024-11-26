###############################################################
#Function to calculate true parameters
logistic_regression <- function(X, y) {
  
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
  
  return(result$par)  # Optimized coefficients
}

data(mtcars)
X1<-as.matrix(cbind(mtcars$mpg, mtcars$hp, mtcars$wt))
y1<-mtcars$am

glm_fit <- glm(am ~ mpg + hp + wt, family = binomial, data = mtcars)
logistic_regression(X1, y1)
glm_fit$coefficients



