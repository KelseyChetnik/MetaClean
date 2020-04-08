#' Generate Bar Plots for the Seven Evaluation Measures
#'
#' Wrapper function for generating bar plots for each classifiers for each of the seven evaluation measures.
#'
#' @param evalMeasuresDF A dataframe with the following columns: Model, RepNum, PosClass.FScore, PosClass.Recall, PosClass.Precision,
#' NegClass.FScore, NegClass.Recall, NegClass.Precision, and Accuracy. The rows of the dataframe will correspond to the results of
#' a particular model and a particular round of cross-validation.
#' @param emNames A list of names of the evaluation measures to visualize. Accepts the following: PosClass.FScore, PosClass.Recall,
#' PosClass.Precision, NegClass.FScore, NegClass.Recall, NegClass.Precision, and Accuracy. Default is "All".
#' @return A list of up to seven bar plots (one for each evaluation measure).
#'
#' @import ggplot2
#'
#' @export

makeBarPlots <- function(evalMeasuresDF, emNames = "All"){

  Mean <- Model <- SE <- NULL

  evalMeasureNameList = c("PosClass.FScore", "PosClass.Precision", "PosClass.Recall", "NegClass.FScore", "NegClass.Precision", "NegClass.Recall",
                          "Accuracy")

  if(emNames == "All"){
    emNames = evalMeasureNameList
  }
  if(any(emNames %in% evalMeasureNameList)==FALSE){
    stop("Unrecognized value for emNames - only the following names allowed:
         PosClass.FScore, PosClass.Precision, PosClass.Recall, NegClass.FScore, NegClass.Precision, NegClass.Recall, Accuracy")
  }

  rounds = unique(as.numeric(as.character(evalMeasuresDF$RepNum)))

  modelNames <- unique(as.character(evalMeasuresDF$Model))
  numModels <- length(modelNames)

  # calculate mean and standard error of all rounds for each model
  summaryStatsList <-  lapply(1:numModels, summaryStats, evalMeasuresDF=evalMeasuresDF, emNames=emNames, modelNames=modelNames)
  barplotSum_df <- as.data.frame(do.call(rbind, summaryStatsList))
  barplotSum_df$Mean <- as.numeric(as.character(barplotSum_df$Mean))
  barplotSum_df$SE <- as.numeric(as.character(barplotSum_df$SE))

  barPlots <- list()
  for(i in 1:length(emNames)){
    emn <- emNames[i]
    df <- barplotSum_df[barplotSum_df$evalMeasure==emn,]
    g <- ggplot(df, aes(x = 1, y = Mean, fill = Model)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE), position = position_dodge(width = 0.9), width = 0.2)
    g <- g + ggtitle(emn)
    g <- g + theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())
    g <- g + geom_text(aes(label=round(Mean,2)), position=position_dodge(width=0.9), vjust=-0.25, size=5)
    g <- g + theme(plot.title = element_text(size = 25, face = "bold"),
                   legend.title=element_text(size=15, face="bold"),
                   legend.text=element_text(size=12),
                   axis.text=element_text(size=12),
                   axis.title=element_text(size=15, face="bold"))
    g <- g + ylim(0,1)

    barPlots[[i]] <- g


  }

  return(barPlots)

}
