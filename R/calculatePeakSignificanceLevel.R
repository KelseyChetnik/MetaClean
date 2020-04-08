#' Calculate Peak Significance Level (of a Chromatographic Peak)
#'
#' Calculates the Peak Significance Level of the integrated region of a chromatographic peak. The Peak Significance Level is found by
#' calculating the ratio of the mean intensity of the points surrounding the apex and the mean intensity of the peak boundary points.
#'
#' This function repurposed from Zhang et al. For details, see Zhang, W., & Zhao, P. X. (2014). Quality evaluation of extracted
#' ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics,
#' 15(Suppl 11), S5. https://doi.org/10.1186/1471-2105-15-S11-S5
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @return The Peak Significance Level (double)
#'
#' @export

calculatePeakSignificanceLevel <- function(peakData, pts){
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]

  peak_intensity <- intPts[,2]
  num_pk_pts <- length(peak_intensity)

  apex_intensity <- max(peak_intensity)
  apex_index <- which(peak_intensity == apex_intensity)
  if(length(apex_index) > 1){
    apex_index <- apex_index[1]
  }

  peak_level = apex_intensity
  base_level = 0.0

  # get peak significance, peak level, and base level
  if(apex_index != 1 & apex_index != num_pk_pts & num_pk_pts > 4){
    sum1 = peak_intensity[1] + peak_intensity[2] + peak_intensity[num_pk_pts] + peak_intensity[num_pk_pts-1]
    sum2 = peak_intensity[apex_index-1] + peak_intensity[apex_index] + peak_intensity[apex_index+1]

    significance = sum2*4.0/(max(sum1, 0.01)*3.0)

  }else{
    significance = NA
  }
  return(significance)
}
