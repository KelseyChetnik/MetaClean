#' Calculate Evaluation Measures
#'
#' Calculate evaluation measures using the predictions generated during cross-validation.
#'
#' @param models list. A list of trained models, like that returned by trainClassifiers()
#' @param k integer. Number of folds used in cross-validation
#' @param repNum integer. Number of cross-validation rounds
#' @return A dataframe with the following columns: Model, RepNum, PosClass.FScore, PosClass.Recall, PosClass.Precision,
#'  NegClass.FScore, NegClass.Recall, NegClass.Precision, Accuracy
#'
#' @examples
#' # calculate all seven evaluation measures for each model and each round of cross-validation
#' \donttest{evalMeasuresDF <- getEvaluationMeasures(models=models, k=5, repNum=10)}
#'
#' @export


getEvaluationMeasures <- function(models, k, repNum){

  numModels <- length(models)

  modelList <- lapply(1:numModels, function(i){models[[i]][[1]]})
  modelNames <- sapply(1:numModels, function(i){models[[i]][[2]]})

  modelSummary_list <- lapply(1:numModels, function(i){
      modelName <- modelNames[i]
      model <- modelList[[i]]
      model_df <- model$pred
      predLabel_list <- lapply(1:repNum, function(j){
        rn <- paste0("Rep",sprintf("%02d", j))
        reSamp <- model_df[grep(rn, model_df$Resample),]
        pred <- reSamp$pred
        true <- reSamp$obs
        measures_list <- calculateEvaluationMeasures(pred, true)
      })

      measures_df <- do.call(rbind, predLabel_list)
      measures_df <- cbind("Model"=modelNames[i], "RepNum"=1:repNum, measures_df)
  })
  modelSummary_df <- as.data.frame(do.call(rbind, modelSummary_list))

  return(modelSummary_df)

}
