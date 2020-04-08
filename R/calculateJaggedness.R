#' Calculate Jaggedness (of a Chromatographic Peak)
#'
#' Calculates the Jaggedness of the integrated region of a chromatographic peak. The Jaggedness is found by determining the fraction of
#' time points the intensity of the peak changes direction - excluding the peak apex and any intensity changes below a flatness factor.
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
#' @return The jaggedness of a chromatographic peak (double)
#'
#' @export


calculateJaggedness <- function(peakData, pts, flatness.factor=0.05){

  # get intensities from integrated region
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]
  intensities <- intPts[,2]

  diff.int <- diff(intensities)
  diff.int[which(abs(diff.int) < flatness.factor * max(abs(intensities)))] = 0
  jaggedness <- (sum(abs(diff(sign(diff.int))) > 1) - 1)/(length(diff.int) -
                                                            1)
  jaggedness <- max(0, jaggedness)
  return(round(jaggedness, digits = 4))

}
