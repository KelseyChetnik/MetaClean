#' Train MetaClean Classifier
#'
#' Wrapper function for training one of the 8 classification algorithms using one of the three available metrics sets.
#'
#' @param trainData dataframe. Rows should correspond to peaks, columns should include peak quality metrics and class labels only.
#' @param model Name of the classification algorithm to be trained from the eight available:
#'     DecisionTree, LogisiticRegression, NaiveBayes, RandomForest, SVM_Linear, AdaBoost, NeuralNetwork, and
#'     ModelAveragedNeuralNetwork.
#' @param metricSet The metric set to be run with the selected model. Select from the following: M4, M7, and M11.
#' @param hyperparameters dataframe of the tuned hyperparameters returned by runCrossValidation()
#'
#' @return a trained MetaClean model
#'
#' @import caret
#' @importFrom stats binomial
#'
#' @examples
#' # train classification algorithms
#' \donttest{best_model <- trainClassifier(trainData=pqMetrics_development,
#'                                         model="AdaBoost",
#'                                         metricSet="M11",
#'                                         hyperparameters)}
#'
#' @export

trainClassifier <- function(trainData,
                            model,
                            metricSet,
                            hyperparameters){

  # validate arguments
  metric_set_list <- c("M4", "M7", "M11")
  model_list <- c("DecisionTree","LogisticRegression", "NaiveBayes", "RandomForest", "SVM_Linear", "AdaBoost",
                  "NeuralNetwork", "ModelAveragedNeuralNet")


  if(model %in% model_list==FALSE){
    stop("Unrecognized Model Names! Only the following model names are allowed:
         DecisionTree, LogisticRegression, NaiveBayes, RandomForest, SVM_Linear,
         AdaBoost, NeuralNetwork, ModelAveragedNeuralNet")
  }

  if(metricSet %in% metric_set_list == F){
    stop("Unrecognized Metric Set Names! Only the following metric set names are allowed:\n M4, M7, M11")
  }

  # Get Peak Quality Metrics for Selected Metric Set
  if(metricSet == "M4"){
    mCols <-c("GaussianSimilarity_mean", "Sharpness_mean", "TPASR_mean", "ZigZag_mean")
  }else if(metricSet == "M7"){
    mCols <- c("ApexBoundaryRatio_mean", "ElutionShift_mean", "FWHM2Base_mean", "Jaggedness_mean", "Modality_mean",
               "RetentionTimeCorrelation_mean", "Symmetry_mean")
  }else if(metricSet == "M11"){
    mCols <- c("ApexBoundaryRatio_mean", "ElutionShift_mean", "FWHM2Base_mean", "Jaggedness_mean", "Modality_mean",
               "RetentionTimeCorrelation_mean", "Symmetry_mean", "GaussianSimilarity_mean",
               "Sharpness_mean", "TPASR_mean", "ZigZag_mean")
  }

  if(any(mCols %in% colnames(trainData)==FALSE)){
    stop("Unrecognized Column Names! Only the following column names are allowed:
         ApexBoundaryRatio_mean, ElutionShift_mean, FWHM2Base_mean, Jaggedness_mean, Modality_mean,
         RetentionTimeCorrelation_mean, Symmetry_mean, GaussianSimilarity_mean,
         Sharpness_mean, TPASR_mean, ZigZag_mean, Class")
  }


  trainData <- trainData[,c(mCols, "Class")]
  trControl <- trainControl(method = "none", savePredictions = 'final', classProbs=TRUE)

  if(model == "DecisionTree"){
    mc_model <- train(Class~., trainData,
                      method = "rpart",
                      trControl = trControl,
                      tuneGrid=hyperparameters)
  }else if(model == "LogisticRegression"){
    mc_model <- train(Class~., trainData,
                      method = "glm",
                      trControl = trControl,
                      tuneGrid = hyperparameters,
                      family = binomial())
  }else if(model == "NaiveBayes"){
    suppressWarnings(mc_model <- train(Class~., trainData,
                                       method = "nb",
                                       trControl = trControl,
                                       trace=FALSE,
                                       tuneGrid = hyperparameters))
  }else if(model == "RandomForest"){
    mc_model <- train(Class~., trainData,
                      method = "rf",
                      trControl = trControl,
                      tuneGrid = hyperparameters)
  }else if(model == "SVM_Linear"){
    mc_model <- train(Class~., trainData,
                      method = "svmLinear",
                      trControl = trControl,
                      tuneGrid = hyperparameters)
  }else if(model == "AdaBoost"){
    mc_model <-  train(Class~., trainData,
                        method = "adaboost",
                        trControl = trControl,
                       tuneGrid = hyperparameters)
  }else if(model == "NeuralNetwork"){
    mc_model <-  train(Class~., trainData,
                       method = "nnet",
                       trControl = trControl,
                       tuneGrid = hyperparameters,
                       trace=FALSE)
  }else if(model == "ModelAveragedNeuralNet"){
    mc_model <-  train(Class~., trainData,
                       method = "avNNet",
                       trControl = trControl,
                       tuneGrid = hyperparameters)
  }

  return(mc_model)

}
