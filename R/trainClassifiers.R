#' Train Classifiers on Peak Quality Metric Feature Sets
#'
#' Wrapper function for training up to 9 classification algorithms using one of the two available metrics sets or both sets combined.
#'
#' @param trainData dataframe. Rows should correspond to peaks, columns should include peak quality metrics and class labels only.
#' @param k integer. Number of folds to be used in cross-validation
#' @param repNum integer. Number of cross-validation rounds to perform
#' @param rand.seed integer. State in which to set the random number generator
#' @param models character string or vector. Specifies the classification algorithms to be trained from the nine available:
#'  DecisionTree, LogisiticRegression, NaiveBayes, RandomForest, SVM_Linear, SVM_Radial, AdaBoost, NeuralNetwork, and
#'  ModelAveragedNeuralNetwork. "all" specifies the use of all models. Default is "all".
#' @return a list of up to 9 trained models
#'
#' @import caret
#' @importFrom stats binomial
#'
#'
#' @export


trainClassifiers <- function(trainData, k, repNum, rand.seed=NULL, models="all"){

  # remove EICNo column if present
  if("EICNo" %in% colnames(trainData)){
    colIdx <- match("EICNo", colnames(trainData))
    trainData <- trainData[,-colIdx]
  }

  # check only Peak Quality Metrics and Class columns present
  reqCols <- c("ApexBoundaryRatio_mean", "ElutionShift_mean", "FWHM2Base_mean", "Jaggedness_mean", "Modality_mean",
               "RetentionTimeCorrelation_mean", "Symmetry_mean", "GaussianSimilarity_mean", "PeakSignificance_mean",
               "Sharpness_mean", "TPASR_mean", "ZigZag_mean", "Class")
  if(any(colnames(trainData) %in% reqCols==FALSE)){
    stop("Unrecognized Column Names! Only the following column names are allowed:
         ApexBoundaryRatio_mean, ElutionShift_mean, FWHM2Base_mean, Jaggedness_mean, Modality_mean,
         RetentionTimeCorrelation_mean, Symmetry_mean, GaussianSimilarity_mean, PeakSignificanceLevel_mean,
         Sharpness_mean, TPASR_mean, ZigZag_mean, Class")
  }

  # check model names are valid
  modelNames <- c("DecisionTree","LogisticRegression", "NaiveBayes", "RandomForest", "SVM_Linear", "SVM_Radial", "AdaBoost",
                  "NeuralNetwork", "ModelAveragedNeuralNet")
  if(tolower(models)=="all"){
    models <- modelNames
  }else{
    if(any(models %in% modelNames==FALSE)){
      stop("Unrecognized Model Names! Only the following model names are allowed:
           DecisionTree, LogisticRegression, NaiveBayes, RandomForest, SVM_Linear,
           SVM_Radial, AdaBoost, NeuralNetwork, ModelAveragedNeuralNet")
    }
  }

  metric = "Accuracy"

  if(!is.null(rand.seed)){
    seed = rand.seed
    set.seed(seed)
  }
  cv_folds <- createMultiFolds(trainData$Class, k = k, times=repNum)
  trControl <- trainControl(method = "repeatedcv", number = k, repeats = repNum, index = cv_folds, savePredictions = 'final', classProbs=TRUE)

  trainClass <- trainData$Class
  classIdx <- match("Class", colnames(trainData))
  #trainData <- trainData[,-classIdx]

  modelList <- list()

  ### ------------------------------------------------ TRAIN CLASSIFIERS ------------------------------------------------ ###

  # Decision Tree
  if("DecisionTree" %in% models){
    m_idx <- match("DecisionTree", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    dt_model <- train(x=trainData[,-classIdx],
                      y=trainData$Class,
                      method = "rpart",
                      trControl = trControl,
                      metric = metric,
                      control=list(maxit=1000))
    modelList[[m_idx]] <- list(dt_model, "DecisionTree")
  }

  # Logistic Regression
  if("LogisticRegression" %in% models){
    m_idx <- match("LogisticRegression", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    lr_model <- train(x=trainData[,-classIdx],
                      y=trainData$Class,
                      method = "glm",
                      trControl = trControl,
                      metric = metric,
                      family = binomial(),
                      control=list(maxit=1000))
  modelList[[m_idx]] <- list(lr_model, "LogisticRegression")
  }

  # Naive Bayes
  if("NaiveBayes" %in% models){
    m_idx <- match("NaiveBayes", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    suppressWarnings(nb_model <- train(x=trainData[,-classIdx],
                                       y=trainData$Class,
                      method = "nb",
                      trControl = trControl,
                      metric = metric,
                      trace=FALSE
    ))
    modelList[[m_idx]] <- list(nb_model, "NaiveBayes")
  }

  # Random Forest
  if("RandomForest" %in% models){
    m_idx <- match("RandomForest", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    system.time(rf_model <- train(x=trainData[,-classIdx],
                                   y=trainData$Class,
                                  method = "rf",
                                  trControl = trControl,
                                  metric = metric,
                                  control=list(maxit=1000)))
    modelList[[m_idx]] <- list(rf_model, "RandomForest")
  }

  # SVM Linear Kernel
  if("SVM_Linear" %in% models){
    m_idx <- match("SVM_Linear", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    lsvm_model <- train(x=trainData[,-classIdx],
                        y=trainData$Class,
                        method = "svmLinear",
                        trControl = trControl,
                        metric = metric,
                        #tuneGrid = tunegrid,
                        control=list(maxit=1000))
    modelList[[m_idx]] <- list(lsvm_model, "SVM_Linear")
  }

  # SVM Radial Kernel
  if("SVM_Radial" %in% models){
    m_idx <- match("SVM_Radial", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    rsvm_model <- train(x=trainData[,-classIdx],
                        y=trainData$Class,
                        method = "svmRadial",
                        trControl = trControl,
                        metric = metric,
                        control=list(maxit=1000))
    modelList[[m_idx]] <- list(rsvm_model, "SVM_Radial")
  }

  # AdaBoost
  if("AdaBoost" %in% models){
    m_idx <- match("AdaBoost", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    system.time(ada_model <-  train(x=trainData[,-classIdx],
                                    y=trainData$Class,
                                    method = "adaboost",
                                    trControl = trControl,
                                    metric = metric,
                                    control=list(maxit=1000)))
    modelList[[m_idx]] <- list(ada_model, "AdaBoost")
  }


  # Neural Network
  if("NeuralNetwork" %in% models){
    m_idx <- match("NeuralNetwork", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    system.time(nn_model <-  train(x=trainData[,-classIdx],
                                   y=trainData$Class,
                                   method = "nnet",
                                   trControl = trControl,
                                   metric = metric,
                                   control=list(maxit=1000),
                                   trace=FALSE))
    modelList[[m_idx]] <- list(nn_model, "NeuralNetwork")
  }

  # Model Average Neural Network
  if("ModelAveragedNeuralNet" %in% models){
    m_idx <- match("ModelAveragedNeuralNet", models)
    if(!is.null(rand.seed)){
      set.seed(seed)
    }
    suppressMessages(avNN_model <-  train(x=trainData[,-classIdx],
                                          y=trainData$Class,
                                         method = "avNNet",
                                         trControl = trControl,
                                         metric = metric,
                                         control=list(maxit=1000),
                                         trace=FALSE))
    modelList[[m_idx]] <- list(avNN_model, "ModelAveragedNeuralNet")
  }

  return(modelList)

}
