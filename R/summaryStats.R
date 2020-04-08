#' Calculate summary statistics for evaluation measures
#'
#' For repeated cross-validation, find the mean and standard error of N rounds for each model.
#'
#' @param i An integer representing 1:N where N is the total number of cross-validation rounds.
#' @param evalMeasuresDF A dataframe with the following columns: Model, RepNum, PosClass.FScore, PosClass.Recall, PosClass.Precision,
#' NegClass.FScore, NegClass.Recall, NegClass.Precision, and Accuracy. The rows of the dataframe will correspond to the results of
#' a particular model and a particular round of cross-validation.
#' @param emNames A list of names of the evaluation measures to visualize. Accepts the following: PosClass.FScore, PosClass.Recall,
#' PosClass.Precision, NegClass.FScore, NegClass.Recall, NegClass.Precision, and Accuracy. Default is "All".
#' @param modelNames A list of the models trained.
#' @return A dataframe with the following columns: Model, evalMeasure, Mean, and SE (Standard Error).
#'
#' @importFrom plotrix std.error
#'
#' @export


summaryStats <- function(i, evalMeasuresDF, emNames, modelNames){
  mn <- modelNames[i]
  emDF <- evalMeasuresDF[evalMeasuresDF$Model==mn,]
  emColIdx <- match(emNames, colnames(emDF))
  emDF[,emColIdx] <- lapply(emDF[,emColIdx], function(x) as.numeric(as.character(x)))
  mfName <- unique(as.character(emDF$Model))
  dfMean <- apply(emDF[,emNames], 2, mean,na.rm=T)
  dfSE <- apply(emDF[,emNames], 2, std.error, na.rm=T)

  sumStatsDF <- as.data.frame(rbind(Mean=dfMean,
                                    SE=dfSE))
  sumStatsDF <- cbind(Model=mfName,
                      evalMeasure=colnames(sumStatsDF),
                      t(sumStatsDF))

  return(sumStatsDF)
}
