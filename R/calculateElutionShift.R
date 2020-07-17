#' Calculate Elution Shift (of a Peak Group)
#'
#' Calculate the Elution Shift of each chromatographic peak in a group of samples. For each sample, the Elution Shift is found by
#' calculating the difference between the peak apex (max intensity) of that chromatographic peak and the median peak apex of all samples
#' and normalizing it by the peak base (which is equal to the average difference between the two peak boundaries). The Elution Shift of
#' the Peak Group is equal to the mean of the Elution Shift of each chromatographic peak.
#'
#' This function repurposed from TargetedMSQC. Toghi Eshghi, S., Auger, P., & Mathews, W. R. (2018). Quality assessment and
#' interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15.
#' https://doi.org/10.1186/s12014-018-9209-x
#'
#' @param peakDataList A list of vectors containing characteristic information about a chromatographic peak - including the retention time range
#' @param ptsList A list of 2D matrices containing the retention time and intensity values of a chromatographic peak
#' @return The Elution Shift of a Peak Group (double)
#'
#' @importFrom stats median
#'
#' @examples
#' # Calculate Elution Shift for each peak
#' data(ex_ptsList)
#' data(ex_peakDataList)
#' elutionShift <- calculateElutionShift(peakDataList = ex_peakDataList, ptsList = ex_ptsList)
#'
#' @export

calculateElutionShift <- function(peakDataList, ptsList){
  # find the retention time corresponding to the peak apex (max intensity) for each sample in the group
  pts <- ptsList
  peakData <- peakDataList

  max_times <- sapply(1:length(pts), function(i){
    pd <- peakData[[i]]
    pt <- pts[[i]]

    peakrange <- pd[c("rtmin", "rtmax")]
    ptsidx <- pt[, 1] >= peakrange[1] & pt[, 1] <= peakrange[2]
    intPts <- pt[ptsidx, ]

    time <- intPts[,1]
    intensities <- intPts[,2]
    max_int <- max(intensities)
    maxIdx <- which(intensities==max_int)[1]
    max_time <- time[maxIdx]

    return(max_time)

  })

  # find the left peak boundary of the integrated region for each sample in the group
  peak_begs <- sapply(1:length(pts), function(i){
    pd <- peakData[[i]]
    pt <- pts[[i]]
    peakrange <- pd[c("rtmin", "rtmax")]
    ptsidx <- pt[, 1] >= peakrange[1] & pt[, 1] <= peakrange[2]
    intPts <- pt[ptsidx, ]

    time <- intPts[,1]
    peak_beg <- time[1]

    return(peak_beg)

  })

  # find the right peak boundary of the integrated region for each sample in the group
  peak_ends <- sapply(1:length(pts), function(i){
    pd <- peakData[[i]]
    pt <- pts[[i]]
    peakrange <- pd[c("rtmin", "rtmax")]
    ptsidx <- pt[, 1] >= peakrange[1] & pt[, 1] <= peakrange[2]
    intPts <- pt[ptsidx, ]

    time <- intPts[,1]
    peak_end <- time[length(time)]

    return(peak_end)

  })

  peak_base <- mean(peak_ends) - mean(peak_begs)
  med_max_time <- median(max_times)
  time_diff_ratio <- abs(med_max_time - max_times) / peak_base
  mean_shift_ratio <- mean(time_diff_ratio)

  return(mean_shift_ratio)

}
