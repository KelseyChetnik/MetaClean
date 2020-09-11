#' Get MetaClean Predictions
#'
#' Wrapper function for retrieving predictions from a trained MetaClean classifier and a test dataset. Returns a data frame with class
#' predictions as well as the associated probabilities for each class prediciton.
#'
#' @param model The train MetaClean model object.
#' @param testData dataframe. Rows should correspond to peaks, columns should include peak quality metrics and EIC column only.
#' @param eicColumn name of the EIC column
#'
#' @return a dataframe with four columns: EIC, Pred_Class, Pred_Prob_Pass, Pred_Prob_Fail
#'
#' @import caret
#'
#' @examples
#' # train classification algorithms
#' \donttest{best_model <- getPredictions(model = mc_model,
#'                                        testData = pqm_test,
#'                                        eicColumn = "EICNo")}
#'
#' @export

getPredicitons <- function(model,
                           testData,
                           eicColumn){

  eic_nums <- testData[,eicColumn]
  testData <- testData[,colnames(testData) != eicColumn]

  predictions_prob <- stats::predict(model, testData, type="prob")
  colnames(predictions_prob) <- c("Pred_Prob_Fail", "Pred_Prob_Pass")
  predictions_prob <- predictions_prob[,c(2,1)]
  predictions_class <- stats::predict(model, testData)

  model_predictions <- cbind("EIC"=eic_nums, "Pred_Class"=predictions_class, predictions_prob)

  return(model_predictions)

}
