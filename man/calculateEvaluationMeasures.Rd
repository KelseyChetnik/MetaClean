% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateEvaluationMeasures.R
\name{calculateEvaluationMeasures}
\alias{calculateEvaluationMeasures}
\title{Calculate Evaluation Measures}
\usage{
calculateEvaluationMeasures(pred, true)
}
\arguments{
\item{pred}{factor. A vector of factors that represent predicted classes}

\item{true}{factor. A vector of factors that represent the true classes}
}
\value{
A dataframe with the following columns: Model, CVNum, RepNum, Accuracy, PassFScore, PassRecall, PassPrecision,
 FailFScore, FailRecall, FailPrecision
}
\description{
Calculate evaluation measures using the predictions generated during cross-validation.
}
\examples{
# Calculate Evaluation Measures for test data
\donttest{test_evalMeasures <- calculateEvaluationMeasures(pred=test_predictions_class,
pqMetrics_test$Class)}
}
