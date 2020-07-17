#' Calculate Symmetry (of a Chromatographic Peak)
#'
#' Calculates the Symmetry of the integrated region of a chromatographic peak. The Symmetry is found by calcuating the correlation between
#' the left and right halves of the peak.
#'
#' This function repurposed from TargetedMSQC. Toghi Eshghi, S., Auger, P., & Mathews, W. R. (2018). Quality assessment and
#' interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15.
#' https://doi.org/10.1186/s12014-018-9209-x
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @return The Symmetry of the peak (double)
#'
#' @importFrom stats cor
#'
#' @examples
#' # Calculate Symmetry for a peak
#' data(ex_pts)
#' data(ex_peakData)
#' symmetry <- calculateSymmetry(peakData = ex_peakData, pts = ex_pts)
#'
#' @export

calculateSymmetry <- function(peakData, pts){
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]
  intensities <- intPts[,2]

  left <- intensities[1:floor(length(intensities)/2)]
  right <- intensities[seq(length(intensities), length(intensities) + 1 - floor(length(intensities)/2), by=-1)]
  symmetry <- cor(left,right,method = "pearson")
  return(symmetry)

}
