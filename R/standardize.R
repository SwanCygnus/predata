#' Standardize training data and test data
#'
#' Standardize training and test data by the information of training data.
#' Test data is standardized using the information of training data.
#' @importFrom dplyr %>%
#' @param test Test data to standardize.
#' @param train Training data that is standardized
#' and used to standardize test data.
#' @param y_method Decide how outcome will be standardized.
#' If \code{FALSE}, nothing is applied.
#' @details Standardization will be done by following formula:
#' \code{z = (x - mean(x)) / sd(x)}
#' @return A list that has standardized training and test data,
#' which includes standard deviation and mean of outcome,
#' The outcome of test data is not changed.
#' \item{train}{Standardized training data}
#' \item{test}{Standardized test data}
#' \item{y_mean}{A value used for centering
#' (mean of outcome in training data)}
#' \item{y_sd}{A value used for scaling
#' (standard deviation of outcome in test data)}
#' @examples
#' data_train <- data.frame(y = rnorm(10), x1 = rnorm(10), x2 = rnorm(10))
#' data_test <- data.frame(y = rnorm(10), x1 = rnorm(10), x2 = rnorm(10))
#' apply(data_train, 2, mean); apply(data_train, 2, sd)
#' data_scaled <- standardize(test = data_test,
#' train = data_train, y_method = "scale")
#' apply(data_scaled$train, 2, mean); apply(data_scaled$train, 2, sd)
#' @export
standardize <- function(test, train, y_method = c("center", "scale")) {
  mean <- apply(train, 2, mean)
  sd <- apply(train, 2, sd)
  train_scale <- data.frame()
  test_scale <- data.frame()
  data_scale <- data.frame()
  mean_mat_train <- (mean %*% t(rep(1, nrow(train)))) %>% t()
  sd_mat_train <- (sd %*% t(rep(1, nrow(train)))) %>% t()
  mean_mat <- (mean %*% t(rep(1, nrow(test)))) %>% t()
  sd_mat <- (sd %*% t(rep(1, nrow(test)))) %>% t()
  if (isFALSE(y_method)) {
    train_scale <- (train[, -1] - mean_mat_train[, -1]) / sd_mat_train[, -1]
    test_scale <- (test[, -1] - mean_mat[, -1]) / sd_mat[, -1]
    data_scale <- list(train = data.frame(train[, 1], train_scale),
                       test = data.frame(test[, 1], test_scale))
  }else if (length(y_method) == 1 && y_method == "center") {
    train_scale <- (train[, -1] - mean_mat_train[, -1]) / sd_mat_train[, -1]
    test_scale <- (test[, -1] - mean_mat[, -1]) / sd_mat[, -1]
    data_scale <- list(train = data.frame(train[, 1] - mean_mat_train[, 1], train_scale),
                       test = data.frame(test[, 1], test_scale))
  }else if (length(y_method) == 1 && y_method == "scale") {
    train_scale <- (train[, -1] - mean_mat_train[, -1]) / sd_mat_train[, -1]
    test_scale <- (test[, -1] - mean_mat[, -1]) / sd_mat[, -1]
    data_scale <- list(train = data.frame(train[, 1] / sd_mat_train[1, 1], train_scale),
                       test = data.frame(test[, 1], test_scale))
  }else if (length(y_method) == 2 && ((y_method[1] == "center" && y_method[2] == "scale") || (y_method[1] == "scale" && y_method[2] == "center"))) {
    train_scale <- base::scale(train)
    test_scale <- (test[, -1] - mean_mat[, -1]) / sd_mat[, -1]
    data_scale <- list(train = train_scale, test = data.frame(test[, 1], test_scale))
  }else {
    stop("wrong input for y_method. type scale or center or both of them in c()")
  }
  names(data_scale$train)[1] <- "y"
  data_result <- c(data_scale, list(y_mean = mean[1], y_sd = sd[1]))
  return(data_result)
}
