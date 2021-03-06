% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pred_original.R
\name{original_pred}
\alias{original_pred}
\title{Transform the predicted value to the original scale}
\usage{
original_pred(pred, sd = 1, mean = 0, boxcox_lambda = NULL)
}
\arguments{
\item{pred}{Predicted value that should be scaled back.}

\item{sd}{The Standard deviation used for scaling the outcome.}

\item{mean}{The mean value used for centering the outcome}

\item{boxcox_lambda}{The value used for Box-Cox transformation.}
}
\value{
The predicted value transformed to the scale of the original value.
}
\description{
Using given standard deviation, mean, and lambda,
transform the predicted value to the original scale.
It also works for Box-Cox transformation.
}
