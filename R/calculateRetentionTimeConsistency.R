#' Calculate Retention Time Consistency (of a Peak Group)
#'
#' Calculates the Retention Time Consistency of each chromatographic peak in a group of samples. For each sample, the Retention Time
#' Consistency is found by calculating the difference between the time at the center of the sample peak and the mean
#' time of all peak centers normalized by the mean time of all the peak centers.
#'
#' This function repurposed from TargetedMSQC. Toghi Eshghi, S., Auger, P., & Mathews, W. R. (2018). Quality assessment and
#' interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15.
#' https://doi.org/10.1186/s12014-018-9209-x
#'
#' @param peakDataList A list of vectors containing characteristic information about a chromatographic peak - including the retention time range
#' @param ptsList A list of 2D matrices containing the retention time and intensity values of a chromatographic peak
#' @return The Retention Time Consistency of a Peak Group (double)
#'
#' @examples
#' # Calculate Retention Time Consistency for each peak
#' data(ex_ptsList)
#' data(ex_peakDataList)
#' rtc <- calculateRetentionTimeConsistency(peakDataList = ex_peakDataList, ptsList = ex_ptsList)
#'
#' @export

calculateRetentionTimeConsistency <- function(peakDataList, ptsList){
  peakData <- peakDataList
  pts <- ptsList
  centers <- sapply(1:length(pts), function(i){
    pd <- peakData[[i]]
    pt <- pts[[i]]
    peakrange <- pd[c("rtmin", "rtmax")]
    ptsidx <- pt[, 1] >= peakrange[1] & pt[, 1] <= peakrange[2]
    intPts <- pt[ptsidx, ]

    time <- intPts[,1]
    time_len <- length(time)
    t_end <- time[time_len]
    t_beg <- time[1]
    center_time <- round(t_end - (abs(t_end-t_beg)/2),4)

    return(center_time)
  })

  mean_center <- mean(centers)

  rt_diff <- abs(centers - mean_center) / mean_center
  rt_consistency <- mean(rt_diff)

  return(rt_consistency)
}
