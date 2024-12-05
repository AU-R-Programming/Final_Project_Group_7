predict_new <- function(data, model, threshold = 0.5) {
  # Ensure the data is a dataframe
  data <- as.data.frame(data)
  
  # Extract beta_optimized from the model
  if (!is.list(model) || !"beta_optimized" %in% names(model)) {
    stop("The model parameter must be a list containing the element 'beta_optimized'.")
  }
  beta_optimized <- model$beta_optimized
  
  # Validate beta_optimized
  if (!is.matrix(beta_optimized) || ncol(beta_optimized) != 1) {
    stop("The beta_optimized parameter must be a column matrix.")
  }
  
  # Extract predictor names from beta_optimized, excluding the intercept
  predictor_names <- rownames(beta_optimized)[-1]  # Exclude the intercept
  
  # Check if all predictor names are present in the dataset
  if (!all(predictor_names %in% names(data))) {
    stop("The dataset must contain all the required predictors specified in the model.")
  }
  
  # Select only the necessary columns from the dataset
  data <- data[, predictor_names, drop = FALSE]
  
  # Add intercept column to the data
  data_with_intercept <- cbind(Intercept = 1, data)
  
  # Calculate the linear predictor
  linear_predictor <- as.matrix(data_with_intercept) %*% beta_optimized
  
  # Apply the logistic function to get probabilities
  probabilities <- 1 / (1 + exp(-linear_predictor))
  
  # Make predictions based on the threshold
  predictions <- ifelse(probabilities >= threshold, 1, 0)
  
  # Add the predictions as a new column
  data_with_predictions <- cbind(data, predicted = predictions)
  
  # Return the predictions and the updated dataset
  return(data_with_predictions = data_with_predictions)
}

