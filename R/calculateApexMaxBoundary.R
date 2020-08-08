#' Calculate Apex-Max Boundary Ratio (of a Chromatographic Peak)
#'
#' Calculates the Apex-Max Boundary Ratio of the integrated region of a chromatographic peak. The Apex-Max Boundary Ratio is found by
#' taking the ratio of the intensity of the peak apex over the intensity of the maximum of the two boundary intensities.
#'
#' This function repurposed from TargetedMSQC. Toghi Eshghi, S., Auger, P., & Mathews, W. R. (2018). Quality assessment and
#' interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15.
#' https://doi.org/10.1186/s12014-018-9209-x
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @return The apex-max boundary ratio (double)
#'
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @examples
#' # Calculate Apex Max-Boundary Ratio for a peak
#' data(ex_pts)
#' data(ex_peakData)
#' apexMaxBoundary <- calculateApexMaxBoundaryRatio(peakData = ex_peakData, pts = ex_pts)
#'
#' @export

calculateApexMaxBoundaryRatio <- function(peakData, pts){
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]
  if(length(intPts) > 2){
    intensities <- intPts[,2]

    r.max.intensity <- max(intensities, na.rm = T)
    r.max.boundary.intensity <- max(c(head(intensities,1), tail(intensities,1)), na.rm = T)
    r.apex.max.boundary.ratio <- r.max.boundary.intensity / r.max.intensity
  }else{
    r.apex.max.boundary.ratio <- NA
  }

  return(r.apex.max.boundary.ratio)
}
