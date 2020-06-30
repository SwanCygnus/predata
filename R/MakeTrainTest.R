#' Data-preprocessing and provide useful data for fitting
#'
#' This function can arrange the data (Move the outcome to the left),
#' standardize the training data and test data
#' using training data's information.
#' You can also choose whether you transform the outcome
#' with Box-Cox transformation or not.
#' If you transform the outcome, it also returns
#' mean value \code{outcome_mean}, standard deviation \code{outcome_sd}
#' and Box-Cox transformation parameter \code{lambda}
#'
#' @importFrom dplyr %>%
#' @importFrom car powerTransform
#' @param outcome_name The outcome's name (Strings)
#' @param features_index The index of columns you use as features
#' @param data_train Training data
#' @param data_test Test data
#' @param y_method Decide how outcome will be standardized.
#' \code{y_method = "scale"}: scale the outcome by dividing with it's standard deviation (\code{y / sd(y)}).
#' \code{y_method = "center"}: centering the outcome (\code{y - mean(y)}).
#' \code{c("scale", "center")}: standardize the outcome (\code{(y - mean(y)) / sd(y)}).
#' If \code{FALSE}, nothing is applied to the outcome.
#' @param boxcox If \code{TRUE}, return transformed outcome
#' with Box-Cox transformation using \code{car} package.
#' @return A list that has scaled data
#' and separate data in features and outcome as matrix.
#' \item{train}{Standardized training data}
#' \item{test}{Standardized test data}
#' \item{train_x}{Drop outcome from \code{train}}
#' \item{train_y}{outcome of training data
#' (if \code{y_method} was specified, It'll be a transformed value).}
#' \item{test_x}{Drop outcome from \code{test}}
#' \item{test_y}{Outcome of test data (not changed)}
#' \item{outcome_mean}{A value used for centering outcome
#' (mean of outcome in training data)}
#' \item{outcome_sd}{A value used for scaling outcome
#' (standard deviation of outcome in test data)}
#' @export
MakeTrainTest <- function(outcome_name, features_index,
                          data_train, data_test,
                          y_method = FALSE, boxcox = FALSE) {
  if (isTRUE(boxcox) && ("center" %in% y_method)) {
    stop("BoxCox transformation is applied only for positive data
         so this transformation (boxcox = TRUE) and
         centering (y_method = \"center\") can't be done together.")
  }else if (any(data_train[, outcome_name] <= 0) && isTRUE(boxcox)) {
    stop("BoxCox transformation is applied only for positive data.")
  }
  # select the outcome's index and necessary data's index
  outcome_name <- outcome_name
  features_index <- features_index
  # split data into training and test set (DataFrame for glmnet, etc)
  train <- cbind(data_train[, outcome_name], data_train[, features_index])
  test <- cbind(data_test[, outcome_name], data_test[, features_index])
  names(train)[1] <- outcome_name
  names(test)[1] <- outcome_name
  # scaling
  data_scale <- standardize(test = test, train = train, y_method = y_method)
  y_sd <- data_scale$y_sd
  y_mean <- data_scale$y_mean
  train_scale <- data_scale$train
  test_scale <- data_scale$test
  names(train_scale)[1] <- outcome_name
  names(test_scale)[1] <- outcome_name
  ### Box-Cox transforamtion
  boxcox_lambda <- NULL
  if (isTRUE(boxcox)) {
    boxcox_lambda <- powerTransform(train_scale[, 1])$lambda
    train_scale[, 1] <- ((train_scale[, 1]) ^ boxcox_lambda - 1) / boxcox_lambda
    names(train_scale[, 1]) <- outcome_name
    }
  train_x <- train_scale[, -1] %>% as.matrix()
  train_y <- train_scale[, 1]
  test_x <- test_scale[, -1]  %>% as.matrix()
  test_y <- test_scale[, 1]

  data_scale <- list(train = train_scale,
                     test = test_scale,
                     train_x = train_x,
                     train_y = train_y,
                     test_x = test_x,
                     test_y = test_y,
                     outcome_mean = y_mean,
                     outcome_sd = y_sd,
                     boxcox_lambda = boxcox_lambda)
  return(data_scale)
}
