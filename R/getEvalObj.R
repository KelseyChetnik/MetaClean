#' Extract peak data object
#'
#' This function extracts, formats, and combines the chromatographic peak data from the objects returned by the getEIC() and fillPeaks()
#' functions from the XCMS package.
#'
#' @param xs An xcmsEIC object returned by the getEIC() function from the XCMS package
#' @param fill An xcmsSet object with filled in peak groups
#' @return An object of class evalObj
#'
#' @import xcms
#'
#' @examples
#' # call getEvalObj on test data
#' # \donttest{eicEval_test <- getEvalObj(xs = xs_test, fill = fill_test)}
#'
#' @export

getEvalObj <- function(xs, fill){
  eicNums <- match(xs@groupnames, groupnames(fill))

  # get eicPts
  eicPts <- lapply(1:length(eicNums), function(j){
    sampList <- lapply(1:length(xs@eic), function(i){
      unname(xs@eic[[i]][[j]])
      })
    })

  # get eicPeakData
  groupidx <- xs@groupnames[1]
  pks <- peaks(fill)
  pidx <- groupval(fill)
  xsgrpidx <- match(xs@groupnames, groupnames(fill,
                                              template = groupidx))
  xssampidx <- match(names(xs@eic), sampnames(fill))

  eicPeakData <- lapply(1:length(eicNums), function(j){
    sampList <- lapply(1:length(xssampidx), function(i){
      pks[pidx[eicNums[j], xssampidx[i]],]
    })
  })

  # get eicNos
  eicNos <- eicNums

  # Create eicEval object
  eicEvalData <- new("evalObj")
  eicEvalData@eicPts <- eicPts
  eicEvalData@eicPeakData <- eicPeakData
  eicEvalData@eicNos <- eicNos

  return(eicEvalData)
}
