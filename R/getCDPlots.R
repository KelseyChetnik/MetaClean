#' Generate Bar Plots for the Seven Evaluation Measures
#'
#' Wrapper function for generating CD plots for each classifiers for each of the seven evaluation measures. Code for CD plots
#' adapted from now archived scmamp R package.
#'
#' @param evalMeasuresDF A dataframe with the following columns: Model, RepNum, Pass_FScore, Pass_Recall, Pass_Precision,
#' Fail_FScore, Fail_Recall, Fail_Precision, and Accuracy. The rows of the dataframe will correspond to the results of
#' a particular model and a particular round of cross-validation.
#' @param emNames A list of names of the evaluation measures to visualize. Accepts the following: Pass_FScore, Pass_Recall,
#' Pass_Precision, Fail_FScore, Fail_Recall, Fail_Precision, and Accuracy. Default is "All".
#' @param compareBest Boolean. If T, compare the best performing models from each of the metric sets. Else, compare the models within
#'    eachh metric set. Must have at least two metric sets. Default: F.
#' @param use_abbr Boolean. If T, use abbreviations for model names in the CD plot (e.g. DecisionTree = DT). Default: T.
#' 
#' @return A named list with the following structure: {metric_type}${plots | rankmatrix}${eval_measures}, where metric_type is one of
#'    the three metric sets (M4, M7, or M11) and eval_measures
#' 
#'
#' @import ggplot2
#'
#' @examples
#' # Create a list of bar plots for each evaluation measure
#' \donttest{getCDPlots(evalMeasuresDF = test_evalMeasures, emNames = c("Pass_FScore", "Fail_FScore"))}
#'
#' @export
getCDPlots <- function(evalMeasuresDF, emNames="All", compareBest=F, use_abbr=T){
  
  evalMeasureNameList = c("Pass_FScore", "Pass_Precision", "Pass_Recall", "Fail_FScore", "Fail_Precision", "Fail_Recall",
                          "Accuracy")
  
  if(length(emNames) == 1 && emNames == "All"){
    emNames = evalMeasureNameList
  }
  if(any(emNames %in% evalMeasureNameList)==FALSE){
    stop("Unrecognized value for emNames - only the following names allowed:
         Pass_FScore, Pass_Precision, Pass_Recall, Fail_FScore, Fail_Precision, Fail_Recall, Accuracy")
  }
  
  # check required columns are present
  req_columns = c("Model", "RepNum", emNames)
  if(any(req_columns %in% colnames(evalMeasuresDF) == F)){
    stop("Required columns are missing. The evalMeasuresDF data frame must contain the following columns:
         Model, RepNum,  Pass_FScore, Pass_Precision, Pass_Recall, Fail_FScore, Fail_Precision, Fail_Recall, and Accuracy.")
  }
  
  # ensure eval measure columns as numeric
  evalMeasuresDF[,evalMeasureNameList] <- lapply(evalMeasuresDF[,evalMeasureNameList], as.numeric)
  
  # replace model names with abbreviations
  if(use_abbr){
    model_names <- c("DecisionTree", "LogisticRegression", "NaiveBayes", "RandomForest", "SVM_Linear", "AdaBoost", "NeuralNetwork",
                     "ModelAveragedNeuralNet")
    model_abbr <- c("DT", "LR", "NB", "RF", "SVM_L", "AB", "NN", "MANN")
    
    # separate model name in alg. and number metrics columns
    evalMeasuresDF %<>% tidyr::separate(col = Model, into = c("Model_Name", "Metric_Set"), sep="_M")
    evalMeasuresDF$Metric_Set <- paste0("M", evalMeasuresDF$Metric_Set)
    
    # replace names with abbr
    evalMeasuresDF$Model_Name <- stringr::str_replace_all(evalMeasuresDF$Model_Name, setNames(model_abbr, model_names))
    evalMeasuresDF %<>% dplyr::mutate(Model = paste0(Model_Name, "_", Metric_Set)) %>% dplyr::select(-c(Model_Name, Metric_Set))
    
  }
  
  # initialize CD plot res list to return
  cd_res_list <- list()
  
  # add list entry for every metric set present
  if(any(endsWith(evalMeasuresDF$Model, "_M4"))) cd_res_list[["M4"]] <- list()
  if(any(endsWith(evalMeasuresDF$Model, "_M7"))) cd_res_list[["M7"]] <- list()
  if(any(endsWith(evalMeasuresDF$Model, "_M11"))) cd_res_list[["M11"]] <- list()
  
  for(mName in names(cd_res_list)){
    
    # loop over eval measures
    for(em in emNames){
      metSetDF <- evalMeasuresDF[endsWith(evalMeasuresDF$Model, mName),]
      cdPlotDF <- get_CD_df(metSetDF, em)
      cdPlotRes <- myPlotCD(cdPlotDF)
      
      cd_res_list[[mName]]$plots[[em]] <- cdPlotRes$cd_plot
      cd_res_list[[mName]]$rank_matrices[[em]] <- cdPlotRes$mean_rank
      
    }
    
  }
  
  # if compareBest = T, take best performing model from each metric set and compare against each other 
  if(compareBest){
    
    if(length(cd_res_list) >= 2){
     
      em_rank_matrices <- list()
      for(em in emNames){
        
        em_rank_matrices[[em]] <- c()
        for(mName in names(cd_res_list)){
          
          em_rank_matrices[[em]] <- c(em_rank_matrices[[em]], cd_res_list[[mName]]$rank_matrices[[em]] %>% which.min() %>% names())
          
        }
        
        # create CD plot for intermetric sets
        intMetDF <- evalMeasuresDF[evalMeasuresDF$Model %in% em_rank_matrices[[em]],]
        cdPlotDF <- get_CD_df(intMetDF, em)
        cdPlotRes <- myPlotCD(cdPlotDF)
        
        # save plots and matrices as intermet
        cd_res_list[["InterMet"]]$plots[[em]] <- cdPlotRes$cd_plot
        cd_res_list[["InterMet"]]$rank_matrices[[em]] <- cdPlotRes$mean_rank
        
      }
       
    }else{
      warning("InterMetric comparison cannot be performed on a single metric set. No InterMetric CD plots generated.")
    }
    
  }
  
  cd_res_list
  
}


myPlotCD <- function (results.matrix, alpha = 0.05, cex = 0.75, ...)
{
  k <- dim(results.matrix)[2]
  N <- dim(results.matrix)[1]
  cd <- getNemenyiCD(alpha = alpha, num.alg = k, num.problems = N)
  mean.rank <- sort(colMeans(rankMatrix(results.matrix, ...)))
  lp <- round(k/2)
  left.algs <- mean.rank[1:lp]
  right.algs <- mean.rank[(lp + 1):k]
  max.rows <- ceiling(k/2)
  char.size <- 0.001
  line.spacing <- 0.25
  m <- floor(min(mean.rank))
  M <- ceiling(max(mean.rank))
  max.char <- max(sapply(colnames(results.matrix), FUN = nchar))
  text.width <- (max.char + 4) * char.size
  w <- (M - m) + 2 * text.width
  h.up <- 2.5 * line.spacing
  h.down <- (max.rows + 2.25) * line.spacing
  tick.h <- 0.25 * line.spacing
  label.displacement <- 0.1
  line.displacement <- 0.025
  plot(0, 0, type = "n", xlim = c(m - w/(M - m), M +
                                    w/(M - m)), ylim = c(-h.down, h.up), xaxt = "n",
       yaxt = "n", xlab = "", ylab = "", bty = "n")
  lines(c(m, M), c(0, 0))
  dk <- sapply(m:M, FUN = function(x) {
    lines(c(x, x), c(0, tick.h))
    text(x, 3 * tick.h, labels = x, cex = cex)
  })
  lines(c(m, m + cd), c(1.75 * line.spacing, 1.75 * line.spacing))
  text(m + cd/2, 2.25 * line.spacing, "CD", cex = cex)
  lines(c(m, m), c(1.75 * line.spacing - tick.h/4, 1.75 * line.spacing +
                     tick.h/4))
  lines(c(m + cd, m + cd), c(1.75 * line.spacing - tick.h/4,
                             1.75 * line.spacing + tick.h/4))
  dk <- sapply(1:length(left.algs), FUN = function(x) {
    line.h <- -line.spacing * (x + 2)
    text(x = m - label.displacement, y = line.h, labels = names(left.algs)[x],
         cex = cex, adj = 1)
    lines(c(m, left.algs[x]), c(line.h, line.h))
    lines(c(left.algs[x], left.algs[x]), c(line.h, 0))
  })
  dk <- sapply(1:length(right.algs), FUN = function(x) {
    line.h <- -line.spacing * (x + 2)
    text(x = M + label.displacement, y = line.h, labels = names(right.algs)[x],
         cex = cex, adj = 0)
    lines(c(M, right.algs[x]), c(line.h, line.h))
    lines(c(right.algs[x], right.algs[x]), c(line.h, 0))
  })
  getInterval <- function(x) {
    from <- mean.rank[x]
    diff <- mean.rank - from
    ls <- which(diff > 0 & diff < cd)
    if (length(ls) > 0) {
      c(from, mean.rank[max(ls)])
    }
  }
  intervals <- mapply(1:k, FUN = getInterval)
  aux <- do.call(rbind, intervals)
  to.join <- aux[1, ]
  if(is.null(aux)==F){
    if (nrow(aux) > 1) {
      for (r in 2:nrow(aux)) {
        if (aux[r - 1, 2] < aux[r, 2]) {
          to.join <- rbind(to.join, aux[r, ])
        }
      }
    }
    row <- c(1)
    if (!is.matrix(to.join)) {
      to.join <- t(as.matrix(to.join))
    }
    nlines <- dim(to.join)[1]
    for (r in 1:nlines) {
      id <- which(to.join[r, 1] > to.join[, 2])
      if (length(id) == 0) {
        row <- c(row, tail(row, 1) + 1)
      }
      else {
        row <- c(row, min(row[id]))
      }
    }
    step <- max(row)/2
    dk <- sapply(1:nlines, FUN = function(x) {
      y <- -line.spacing * (0.5 + row[x]/step)
      lines(c(to.join[x, 1] - line.displacement, to.join[x,
                                                         2] + line.displacement), c(y, y), lwd = 3)
    })
  }
  
  gridGraphics::grid.echo()
  p <- grid::grid.grab()
  p2 <- ggplotify::as.ggplot(p)
  
  return(list(mean_rank = mean.rank, cd_plot=p2))
}


getNemenyiCD <- function (alpha = 0.05, num.alg, num.problems)
{
  df <- num.alg * (num.problems - 1)
  qa <- qtukey(p = 1 - alpha, nmeans = num.alg, df = df)/sqrt(2)
  cd <- qa * sqrt((num.alg * (num.alg + 1))/(6 * num.problems))
  return(cd)
}

get_CD_df <- function(df, em){
  df <- df[,c("Model", "RepNum", em)]
  reshape_df <- t(reshape(df, idvar = "Model", timevar = "RepNum", direction = "wide"))
  colnames(reshape_df) <- reshape_df[1,]
  reshape_df <- as.data.frame(reshape_df[2:11,])
  indx <- sapply(reshape_df, is.character)
  reshape_df[indx] <- lapply(reshape_df[indx], function(x) as.numeric(x))
  return(reshape_df)
}

rankMatrix <- function(data, decreasing=TRUE, ...){
  # The rank function is based on an increasing ordering. In case we need to
  # get the rank of the descreasing ordering, just rank -x instead of x
  if (decreasing){
    f <- function(x){
      rank(-x, ties.method="average")
    }
  } else {
    f <- function(x){
      rank(x, ties.method="average")
    }
  }
  
  rankings <- t(apply (data, MARGIN=1, FUN=f))
  colnames(rankings) <- colnames(data)
  rownames(rankings) <- rownames(data)
  return(rankings)
}
