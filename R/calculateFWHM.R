#' Calculate FWHM2Base (of a Chromatographic Peak)
#'
#' Calculates the FWHM2Base of the integrated region of a chromatographic peak. The FWHM2Base is found by determining the peak width
#' at half of the maximum intensity and normalizing this value by the width of the base of the peak.
#'
#' This function repurposed from TargetedMSQC. Toghi Eshghi, S., Auger, P., & Mathews, W. R. (2018). Quality assessment and
#' interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15.
#' https://doi.org/10.1186/s12014-018-9209-x
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @return The FWHM2Base value (double)
#'
#' @importFrom utils tail
#'
#' @export

calculateFWHM <- function(peakData, pts){
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]

  rt <- intPts[,1]
  intensities <- intPts[,2]

  peakmax <- max(intensities)
  left.index <- c(which(intensities - peakmax/2 > 0)[1] - 1, which(intensities -
                                                                     peakmax/2 > 0)[1])
  right.index <- c(tail(which(intensities - peakmax/2 > 0), 1),
                   tail(which(intensities - peakmax/2 > 0), 1) + 1)
  if (left.index[1] == 0 || is.na(left.index[1])) {
    t.left <- rt[1]
  }
  else {
    t.left <- (rt[left.index[2]] - rt[left.index[1]])/(intensities[left.index[2]] -
                                                         intensities[left.index[1]]) * (peakmax/2 - intensities[left.index[1]]) +
      rt[left.index[1]]
  }
  if (right.index[2] > length(rt) || is.na(right.index[2])) {
    t.right <- tail(rt, 1)
  }
  else {
    t.right <- (rt[right.index[2]] - rt[right.index[1]])/(intensities[right.index[2]] -
                                                            intensities[right.index[1]]) * (peakmax/2 - intensities[right.index[1]]) +
      rt[right.index[1]]
  }
  if (length(t.left) == 0)
    t.left <- rt[1]
  if (length(t.right) == 0)
    t.right <- tail(rt, 1)
  fwhm <- t.right - t.left
  fwhm2base <- fwhm/(tail(rt, 1) - rt[1])
  return(fwhm2base)
}
