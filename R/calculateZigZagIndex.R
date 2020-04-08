#' Calculate the Zig-Zag Index (of a Chromatographic Peak)
#'
#' Calculates the Zig-Zag Index of the integrated region of a chromatographic peak. The Zig-Zag Index is found by calculating the sum of
#' the slope changes between neighboring points normalized by the average intensity of the peak boundaries.
#'
#' This function repurposed from Zhang et al. For details, see Zhang, W., & Zhao, P. X. (2014). Quality evaluation of extracted
#' ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics,
#' 15(Suppl 11), S5. https://doi.org/10.1186/1471-2105-15-S11-S5
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @return The Zig-Zag Index value (double)
#'
#' @export


calculateZigZagIndex <- function(peakData, pts){
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]

  eic <- intPts[,2]

  end <- length(eic)
  EPI=max(eic)-(eic[1]+eic[2]+eic[end]+eic[end-1])/4.0;

  zig_zag_sum=0.0
  for(i in 2:(end-1)){
    local_zig_zag=(2*eic[i]-eic[i-1]-eic[i+1])^2.0
    zig_zag_sum=zig_zag_sum+local_zig_zag
  }

  zig_zag_index = zig_zag_sum/(EPI^2.0*end)
  return(zig_zag_index)
}
