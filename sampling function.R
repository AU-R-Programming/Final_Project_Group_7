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


#' Stratified Train-Test Split for Binary Classification
#'
#' This function splits a dataset into training and testing sets while ensuring that the split is stratified 
#' based on the levels of a binary dependent variable. The user can specify the proportion of data for training, 
#' whether to return the actual data or indices, and an optional seed for reproducibility.
#'
#' @param data A data frame containing the dataset to be split.
#' @param dependent_var The name or index of the dependent variable (response) column. It must be binary.
#' @param train_prop A numeric value specifying the proportion of data to include in the training set. Default is 0.75.
#' @param return_data Logical, indicating whether to return the actual train and test datasets (`TRUE`) or only the indices (`FALSE`). Default is `FALSE`.
#' @param seed An optional integer seed for reproducibility. Default is `NULL` (no seed set).
#'
#' @return If `return_data = FALSE` (default), the function returns a vector of indices for the training set.
#' If `return_data = TRUE`, it returns a list with two elements:
#' \item{train}{The training set as a data frame.}
#' \item{test}{The testing set as a data frame.}
#'
#' @examples
#' # Example: Splitting the Iris dataset (binary classification case)
#' data(iris)
#' iris_binary <- iris[iris$Species %in% c("setosa", "versicolor"), ]
#' iris_binary$Species <- factor(iris_binary$Species)
#'
#' # Get indices for the training set
#' train_indices <- train_test_sampling(data = iris_binary, dependent_var = "Species", train_prop = 0.8)
#' head(train_indices)
#'
#' # Split data into training and testing sets
#' split_data <- train_test_sampling(data = iris_binary, dependent_var = "Species", train_prop = 0.8, return_data = TRUE)
#' head(split_data$train)
#' head(split_data$test)
#'
#' @export



