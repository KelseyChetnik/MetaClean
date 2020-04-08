#' Calcualte Triangle Peak Area Similarity Ratio (TPASR) (of a Chromatographic Peak)
#'
#' Calculates the Triangle Peak Area Similarity Ratio (TPASR) of the integrated region of a chromatographic peak. The TPASR is found
#' by calculating the ratio of the difference between the area of a triangle formed by the apex and the two peak boundaries and the
#' integrated area of the peak over the area of the triangle.
#'
#' This function repurposed from Zhang et al. For details, see Zhang, W., & Zhao, P. X. (2014). Quality evaluation of extracted
#' ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics,
#' 15(Suppl 11), S5. https://doi.org/10.1186/1471-2105-15-S11-S5
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @return The TPASR value (double)
#'
#' @export

calculateTPASR <- function(peakData, pts){
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
  if(apex_index != 1 && apex_index != num_pk_pts && num_pk_pts > 4){
    sum1 = peak_intensity[1] + peak_intensity[2] + peak_intensity[num_pk_pts] + peak_intensity[num_pk_pts-1]
    sum2 = peak_intensity[apex_index-1] + peak_intensity[apex_index] + peak_intensity[apex_index+1]

    significance = sum2*4.0/(max(sum1, 0.01)*3.0)

    peak_level = sum2/3.0
    base_level = sum1/4.0
  }else{
    TPASR = NA
    return(TPASR)
  }

  triangular_area = 0.5*length(peak_intensity)*(peak_level - min(peak_intensity[1], peak_intensity[num_pk_pts]))
  peak_area = sum(peak_intensity) - min(peak_intensity[1],peak_intensity[num_pk_pts])*num_pk_pts
  TPASR = abs(triangular_area-peak_area)/triangular_area

  return(TPASR)
}
