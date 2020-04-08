#' Calculate Modality (of a Chromatographic Peak)
#'
#' Calculates the Modality of the integrated region of a chromatographic peak. The Modaily is found by taking the ratio of the magnitude
#' of the largest drop in intensity (exluding the apex) and the maximum intensity of the peak.
#'
#' This function repurposed from TargetedMSQC. Toghi Eshghi, S., Auger, P., & Mathews, W. R. (2018). Quality assessment and
#' interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15.
#' https://doi.org/10.1186/s12014-018-9209-x
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @param flatness.factor A numeric value between 0 and 1 that allows the user to adjust the sensitivity of the function to noise. This
#' function calculates the difference between each adjacent pair of points; any value found to be less than flatness.factor * maximum
#' intensity is set to 0.
#' @return The modality of the peak (double)
#'
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @export

calculateModality <- function(peakData, pts, flatness.factor = 0.05){
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]

  time <- intPts[,1]
  intensities <- intPts[,2]

  diff.sig <- diff(intensities)
  diff.sig[which(abs(diff.sig) < flatness.factor * max(abs(intensities)))] <- 0
  first.fall <- head(which(diff.sig < 0), 1)
  last.rise <- tail(which(diff.sig > 0), 1)
  if (length(first.fall) == 0)
    first.fall <- length(time) + 1
  if (length(last.rise) == 0)
    last.rise <- -1
  max.dip <- 0
  if (!is.na(first.fall) & !is.na(last.rise) & first.fall <
      last.rise) {
    max.dip <- max(abs(diff.sig[first.fall:last.rise]))
  }
  if (max(intensities) == 0) {
    modality <- 0
  }
  else {
    modality <- max.dip/max(intensities)
  }

  return(modality)
}
