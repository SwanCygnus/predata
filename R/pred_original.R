#' Transform the predicted value to the original scale
#'
#' Using given standard deviation, mean, and lambda,
#' transform the predicted value to the original scale.
#' It also works for Box-Cox transformation.
#' @importFrom dplyr %>%
#' @param pred Predicted value that should be scaled back.
#' @param sd The Standard deviation used for scaling the outcome.
#' @param mean The mean value used for centering the outcome
#' @param boxcox_lambda The value used for Box-Cox transformation.
#' @return The predicted value transformed to the scale of the original value.
#' @export
original_pred <- function(pred, sd = 1, mean = 0, boxcox_lambda = NULL) {
  if (is.null(boxcox_lambda)) {
    result <- pred * sd + mean
  }else {
    result <- (pred * boxcox_lambda + 1) ^ (1 / boxcox_lambda) * sd + mean
  }
  return(result)
}
