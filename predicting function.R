predict_logistic_regression <- function(model, newdata) {
  # Ensure newdata is a data frame
  newdata <- as.data.frame(newdata)
  
  # Convert character or factor covariates to numeric using the same levels as in the model
  factor_levels <- model$factor_levels # Assuming factor_levels were saved in the model during training
  
  newdata[] <- lapply(names(newdata), function(col_name) {
    col <- newdata[[col_name]]
    if (is.factor(col) || is.character(col)) {
      if (col_name %in% names(factor_levels)) {
        levels <- factor_levels[[col_name]]
        return(as.numeric(factor(col, levels = levels)))
      } else {
        stop(paste("Factor levels for variable", col_name, "not found in the model."))
      }
    } else {
      return(col)
    }
  })
  
  # Ensure that newdata has the same structure as the training data used in the model
  required_vars <- names(model$beta_optimized)[-1] # Remove intercept from required vars
  missing_vars <- setdiff(required_vars, names(newdata))
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in newdata:", paste(missing_vars, collapse = ", ")))
  }
  
  # Reorder columns to match the training data
  X_new <- newdata[, required_vars, drop = FALSE]
  
  # Create design matrix for newdata
  design_new <- cbind(Intercept = rep(1, nrow(X_new)), X_new)
  
  # Predict probabilities based on optimized beta coefficients from the model
  beta_optimized <- model$beta_optimized
  linear_combination <- design_new %*% beta_optimized
  p_hat <- 1 / (1 + exp(-linear_combination))
  
  # Set threshold for classification
  y_pred <- ifelse(p_hat >= 0.5, 1, 0)
  
  return(list(probabilities = p_hat, predicted_class = y_pred))
}




