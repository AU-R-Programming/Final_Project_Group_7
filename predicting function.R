predict_outcome <- function(new_data, model, dependent_variable_col) {
  # Extract beta_optimized and factor_mappings from the model
  beta_optimized <- model$beta_optimized
  factor_mappings <- model$factor_mappings
  
  # Convert factors to numeric based on factor mappings
  for (factor_name in names(factor_mappings)) {
    if (factor_name %in% colnames(new_data)) {
      mapping <- factor_mappings[[factor_name]]
      levels <- mapping$Level
      numeric_values <- mapping$Numeric
      new_data[[factor_name]] <- as.numeric(factor(new_data[[factor_name]], levels = levels, labels = numeric_values))
    }
  }
  
  # Extract the predictors from the model
  predictor_names <- rownames(beta_optimized)[-1]  # Exclude the intercept
  
  # Check if all predictors are present in the new_data
  missing_predictors <- setdiff(predictor_names, colnames(new_data))
  if (length(missing_predictors) > 0) {
    stop("The following predictors are missing in the new data: ", paste(missing_predictors, collapse = ", "))
  }
  
  # Ensure the order of columns in new_data matches the order in beta_optimized
  predictors_data <- new_data[, predictor_names, drop = FALSE]
  
  # Extract the predictors and add an intercept column
  X <- as.matrix(cbind(Intercept = 1, predictors_data))
  
  # Predict the log-odds
  log_odds <- X %*% beta_optimized
  
  # Convert log-odds to probabilities using the logistic function
  probabilities <- 1 / (1 + exp(-log_odds))
  
  # Classify as 'Positive' or 'Negative' based on a threshold of 0.5
  predicted_outcomes <- ifelse(probabilities >= 0.5, "TRUE", "FALSE")
  
  # Add a column with binary outcomes (0 or 1)
  binary_outcomes <- ifelse(predicted_outcomes == "TRUE", 1, 0)
  
  # Create a data frame with both predicted outcomes and binary outcomes
  result <- data.frame(predicted_outcomes, binary_outcomes)
  
  # Compare the binary outcomes to the actual dependent variable
  actual_outcomes <- as.numeric(as.factor(new_data[[dependent_variable_col]]))-1
  
  # Convert actual outcomes to binary (0 or 1) for comparison if necessary
  if (is.factor(actual_outcomes) || is.character(actual_outcomes)) {
    actual_outcomes <- as.numeric(as.factor(actual_outcomes))-1
  }
  
  # Create a confusion matrix using binary outcomes
  confusion_matrix <- table(Predicted = binary_outcomes, Actual = actual_outcomes)
  
  # Add the confusion matrix to the result
  print("Confusion Matrix:")
  print(confusion_matrix)
  
  return(result)
}
