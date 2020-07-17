#' A custom class for storing the chromatographic peak data required by the peak metric functions for each group of samples.
#'
#' @slot eicPts A list of 2D matrices containing the retention time and intensity values of each chromatographic peak
#' @slot eicPeakData A list of vectors for each sample in the group containing characteristic information about each chromatographic peak
#' @slot eicNos A numeric vector of the EIC numbers identifying each feature group
#'
#' @exportClass evalObj
#'

evalObj <- setClass("evalObj", slots=list(eicPts="list",
                                   eicPeakData="list",
                                   eicNos="integer"
 ))
