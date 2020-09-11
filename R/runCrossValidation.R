#' Run Cross-Validation for A List of Algoirthms with Peak Quality Metric Feature Sets
#'
#' Wrapper function for running cross-validation on up to 8 classification algorithms using one or more of the three available
#' metrics sets.
#'
#' @param trainData dataframe. Rows should correspond to peaks, columns should include peak quality metrics and class labels only.
#' @param k integer. Number of folds to be used in cross-validation
#' @param repNum integer. Number of cross-validation rounds to perform
#' @param rand.seed integer. State in which to set the random number generator
#' @param models character string or vector. Specifies the classification algorithms to be trained from the eight available:
#'     DecisionTree, LogisiticRegression, NaiveBayes, RandomForest, SVM_Linear, AdaBoost, NeuralNetwork, and
#'     ModelAveragedNeuralNetwork. "all" specifies the use of all models. Default is "all".
#' @param metricSet The metric set(s) to be run with the selected model(s). Select from the following: M4, M7, and M11. Use c()
#'     to select multiple metrics. "all" specifics the use of all metrics. Default is "M11".
#'
#' @return a list of up to 8 trained models
#'
#' @import caret
#' @importFrom stats binomial
#'
#' @examples
#' # train classification algorithms
#' \donttest{models <- trainClassifiers(trainData=pqMetrics_development, k=5, repNum=10,
#'  rand.seed = 453, models="DecisionTree")}
#'
#' @export


runCrossValidation <- function(trainData, k, repNum, rand.seed=NULL, models="all", metricSet="M11"){

  # remove EICNo column if present
  if("EICNo" %in% colnames(trainData)){
    colIdx <- match("EICNo", colnames(trainData))
    trainData <- trainData[,-colIdx]
  }

  # check only Peak Quality Metrics and Class columns present
  reqCols <- c("ApexBoundaryRatio_mean", "ElutionShift_mean", "FWHM2Base_mean", "Jaggedness_mean", "Modality_mean",
               "RetentionTimeCorrelation_mean", "Symmetry_mean", "GaussianSimilarity_mean",
               "Sharpness_mean", "TPASR_mean", "ZigZag_mean", "Class")
  if(any(colnames(trainData) %in% reqCols==FALSE)){
    stop("Unrecognized Column Names! Only the following column names are allowed:
         ApexBoundaryRatio_mean, ElutionShift_mean, FWHM2Base_mean, Jaggedness_mean, Modality_mean,
         RetentionTimeCorrelation_mean, Symmetry_mean, GaussianSimilarity_mean,
         Sharpness_mean, TPASR_mean, ZigZag_mean, Class")
  }

  # check metricSet names are valid
  metricSetNames <- c("M4", "M7", "M11")
  if(any(metricSet %in% metricSetNames == F)){
    stop("Unrecognized Metric Set Names! Only the following metric set names are allowed:\n M4, M7, M11")
  }

  # check model names are valid
  modelNames <- c("DecisionTree","LogisticRegression", "NaiveBayes", "RandomForest", "SVM_Linear", "AdaBoost",
                  "NeuralNetwork", "ModelAveragedNeuralNet")
  if(tolower(models)=="all"){
    models <- modelNames
  }else{
    if(any(models %in% modelNames==FALSE)){
      stop("Unrecognized Model Names! Only the following model names are allowed:
           DecisionTree, LogisticRegression, NaiveBayes, RandomForest, SVM_Linear,
           AdaBoost, NeuralNetwork, ModelAveragedNeuralNet")
    }
  }

  metModels <- apply(expand.grid(models, metricSet), 1, paste, collapse="_")
  holdData <- trainData

  # metric to use for parameter optimization
  metric = "Accuracy"

  ### ------------------------------------------------ TRAIN CLASSIFIERS ------------------------------------------------ ###

  modelList <- list()
  m_idx <- 1
  for(mm in  metModels){
    trainData <- holdData

    if(endsWith(mm, "_M4")){
      mCols <-c("GaussianSimilarity_mean", "Sharpness_mean", "TPASR_mean", "ZigZag_mean")
    }else if(endsWith(mm, "_M7")){
      mCols <- c("ApexBoundaryRatio_mean", "ElutionShift_mean", "FWHM2Base_mean", "Jaggedness_mean", "Modality_mean",
                 "RetentionTimeCorrelation_mean", "Symmetry_mean")
    }else{
      mCols <- c("ApexBoundaryRatio_mean", "ElutionShift_mean", "FWHM2Base_mean", "Jaggedness_mean", "Modality_mean",
                 "RetentionTimeCorrelation_mean", "Symmetry_mean", "GaussianSimilarity_mean",
                 "Sharpness_mean", "TPASR_mean", "ZigZag_mean")
    }

    trainData <- trainData[,c(mCols, "Class")]

    if(!is.null(rand.seed)){
      seed = rand.seed
      set.seed(seed)
    }
    cv_folds <- createMultiFolds(trainData$Class, k = k, times=repNum)
    trControl <- trainControl(method = "repeatedcv", number = k, repeats = repNum, index = cv_folds, savePredictions = 'final', classProbs=TRUE)

    trainClass <- trainData$Class
    classIdx <- match("Class", colnames(trainData))
    #trainData <- trainData[,-classIdx]

    # Decision Tree
    if(startsWith(mm, "DecisionTree")){
      if(!is.null(rand.seed)){
        set.seed(seed)
      }
      dt_model <- train(x=trainData[,-classIdx],
                        y=trainData$Class,
                        method = "rpart",
                        trControl = trControl,
                        metric = metric,
                        control=list(maxit=1000))
      modelList[[m_idx]] <- dt_model

    }

    # Logistic Regression
    if(startsWith(mm, "LogisticRegression")){
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
      modelList[[m_idx]] <- lr_model

    }

    # Naive Bayes
    if(startsWith(mm, "NaiveBayes")){
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
      modelList[[m_idx]] <- nb_model

    }

    # Random Forest
    if(startsWith(mm, "RandomForest")){
      if(!is.null(rand.seed)){
        set.seed(seed)
      }
      system.time(rf_model <- train(x=trainData[,-classIdx],
                                    y=trainData$Class,
                                    method = "rf",
                                    trControl = trControl,
                                    metric = metric,
                                    control=list(maxit=1000)))
      modelList[[m_idx]] <- rf_model

    }

    # SVM Linear Kernel
    if(startsWith(mm, "SVM_Linear")){
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
      modelList[[m_idx]] <- lsvm_model

    }

    # AdaBoost
    if(startsWith(mm, "AdaBoost")){
      if(!is.null(rand.seed)){
        set.seed(seed)
      }
      system.time(ada_model <-  train(x=trainData[,-classIdx],
                                      y=trainData$Class,
                                      method = "adaboost",
                                      trControl = trControl,
                                      metric = metric,
                                      control=list(maxit=1000)))
      modelList[[m_idx]] <- ada_model

    }


    # Neural Network
    if(startsWith(mm, "NeuralNetwork")){
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
      modelList[[m_idx]] <- nn_model

    }

    # Model Average Neural Network
    if(startsWith(mm, "ModelAveragedNeuralNet")){
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
      modelList[[m_idx]] <- avNN_model

    }

    m_idx = m_idx + 1
  } # end metric sets loop

  names(modelList) <- metModels

  return(modelList)

}
