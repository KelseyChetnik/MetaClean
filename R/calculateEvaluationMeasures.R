#' Calculate Evaluation Measures
#'
#' Calculate evaluation measures using the predictions generated during cross-validation.
#'
#' @param pred factor. A vector of factors that represent predicted classes
#' @param true factor. A vector of factors that represent the true classes
#' @return A dataframe with the following columns: Model, CVNum, RepNum, Accuracy, PassFScore, PassRecall, PassPrecision,
#'  FailFScore, FailRecall, FailPrecision
#'
#' @export
#'
#' @importFrom MLmetrics Precision
#' @importFrom MLmetrics Recall
#' @importFrom MLmetrics F1_Score
#' @importFrom MLmetrics Accuracy
#'
#' @examples
#' # Calculate Evaluation Measures for test data
#' \donttest{test_evalMeasures <- calculateEvaluationMeasures(pred=test_predictions_class,
#' pqMetrics_test$Class)}

calculateEvaluationMeasures <- function(pred, true){
  pred <- as.numeric(pred)
  true <- as.numeric(true)
  if(sum(pred==2)==length(pred)){
    gPrec <- Precision(y_true=true, y_pred=pred, positive = 2)
    bPrec <- NA
    gRec <- 1
    bRec <- 0
    gF <- NA
    bF <- NA
  }else if(sum(pred==2)==0){
    gPrec <- NA
    bPrec <- Precision(y_true=true, y_pred=pred, positive = 1)
    gRec <- 0
    bRec <- 1
    gF <- NA
    bF <- NA
  }else{
    gPrec <- Precision(y_true=true, y_pred=pred, positive = 2)
    bPrec <- Precision(y_true=true, y_pred=pred, positive = 1)
    gRec <- Recall(y_true=true, y_pred=pred, positive = 2)
    bRec <- Recall(y_true=true, y_pred=pred, positive = 1)
    gF <- F1_Score(y_true = true, y_pred = pred, positive = 2)
    bF <- F1_Score(y_true = true, y_pred = pred, positive = 1)
  }
  acc <- Accuracy(y_true = true, y_pred = pred)

  measures <- c(gPrec, gRec, gF, bPrec, bRec, bF, acc)
  names(measures) <- c("Pass_Precision", "Pass_Recall", "Pass_FScore", "Fail_Precision", "Fail_Recall", "Fail_FScore", "Accuracy")
  measures
}
