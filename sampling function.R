# Define the function
train_test_sampling <- function(data, dependent_var, train_prop = 0.75, return_data = FALSE, seed = NULL) {
  # Ensure the data is a dataframe
  data <- as.data.frame(data)
  
  # Remove incomplete cases
  data <- data[complete.cases(data), ]
  
  # Handle dependent_var as either column index or name
  if (is.numeric(dependent_var)) {
    if (dependent_var > ncol(data) || dependent_var < 1) {
      stop("The specified dependent variable index is out of range.")
    }
    dependent_var <- names(data)[dependent_var]
  } else if (!(dependent_var %in% names(data))) {
    stop("The specified dependent variable is not in the data.")
  }
  
  # Check if the dependent variable is binomial
  unique_levels <- unique(data[[dependent_var]])
  if (length(unique_levels) != 2) {
    stop("The dependent variable must be binomial with exactly two unique levels.")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Split the data by levels of the dependent variable and collect training indices
  train_indices <- unlist(lapply(unique_levels, function(level) {
    subset_data <- which(data[[dependent_var]] == level)
    sample(subset_data, size = round(length(subset_data) * train_prop), replace = FALSE)
  }))
  
  # Return the training indices only or the train and test datasets
  if (return_data) {
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    return(list(train = train_data, test = test_data))
  } else {
    return(train_indices)
  }
}




