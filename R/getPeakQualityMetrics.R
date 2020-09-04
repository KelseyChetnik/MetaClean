#' Calculate the 12 Peak Quality Metrics
#'
#' Wrapper function for calculating the each of the 12 peak quality metrics for each feature.
#'
#' @param eicEvalData An object of class evalObj containing the required chromatographic peak information
#' @param eicLabels_df A dataframe with EICNos in the first column and Labels in the second column
#' @param flatness.factor A numeric value between 0 and 1 that allows the user to adjust the sensitivity of the function to noise. This
#' function calculates the difference between each adjacent pair of points; any value found to be less than flatness.factor * maximum
#' intensity is set to 0.
#' @return An Mx14 matrix where M is equal to the number of peaks. There are 14 columns in total, including one column for each of the twelve
#' metrics, one column for EIC numbers, and one column for the class label.
#'
#' @examples
#' # # calculate peak quality metrics for development dataset
#' \donttest{pqMetrics_development <- getPeakQualityMetrics(eicEvalData = eicEval_development,
#' eicLabels_df = eicLabels_development)}
#'
#' @export

getPeakQualityMetrics <- function(eicEvalData, eicLabels_df, flatness.factor=0.05){

  eicPts = eicEvalData@eicPts
  eicPeakData = eicEvalData@eicPeakData
  eicNums = eicEvalData@eicNos
  numCols = length(eicNums)

  # Apex-Boundary Raio
  eic_abr_list <- lapply(1:numCols, function(i) {mapply(calculateApexMaxBoundaryRatio,
                                                       peakData=eicPeakData[[i]], pts=eicPts[[i]])})
  eic_abr_mean <- sapply(1:length(eic_abr_list), function(i){mean(eic_abr_list[[i]], na.rm=T)})

  # Elution Shift
  eic_elution_mean <- sapply(1:numCols, function(k){
                                          peakData=eicPeakData[[k]]
                                          pts=eicPts[[k]]
                                          eic_shift <- calculateElutionShift(peakData, pts)
  })

  # FWHM2Base
  eic_f2b_list <- lapply(1:numCols, function(i) mapply(calculateFWHM,
                                                       peakData=eicPeakData[[i]], pts=eicPts[[i]]))
  eic_f2b_mean <- sapply(1:length(eic_f2b_list), function(i){mean(eic_f2b_list[[i]],na.rm=T)})

  # Jaggedness
  eic_jagged_list <- lapply(1:numCols, function(i) mapply(calculateJaggedness,
                                                          peakData=eicPeakData[[i]], pts=eicPts[[i]], flatness.factor=flatness.factor))
  eic_jagged_mean <- sapply(1:length(eic_jagged_list), function(i){mean(eic_jagged_list[[i]], na.rm=T)})

  # Modality
  eic_modality_list <- lapply(1:numCols, function(i) mapply(calculateModality,
                                                            peakData=eicPeakData[[i]], pts=eicPts[[i]], flatness.factor=flatness.factor))
  eic_modality_mean <- sapply(1:length(eic_modality_list), function(i){mean(eic_modality_list[[i]], na.rm=T)})

  # Retention Time Consistency
  eic_RTC_mean <- sapply(1:numCols, function(k){
                                      peakData=eicPeakData[[k]]
                                      pts=eicPts[[k]]
                                      eic_retention_time <- calculateRetentionTimeConsistency(peakData, pts)
  })

  # Symmetry
  eic_symmetry_list <- suppressWarnings(lapply(1:numCols, function(i) mapply(calculateSymmetry,
                                                            peakData=eicPeakData[[i]], pts=eicPts[[i]])))
  eic_symmetry_mean <- sapply(1:length(eic_modality_list), function(i){mean(eic_symmetry_list[[i]], na.rm=T)})

  # Gaussian Similarity
  eic_gaussian_list <- lapply(1:numCols, function(i) mapply(calculateGaussianSimilarity,
                                       peakData=eicPeakData[[i]], pts=eicPts[[i]]))
  eic_gaussian_mean <- sapply(1:length(eic_gaussian_list), function(i){mean(eic_gaussian_list[[i]], na.rm=T)})

  # Sharpness
  eic_sharpness_list <- suppressWarnings(lapply(1:numCols, function(i) mapply(calculateSharpness,
                                                             peakData=eicPeakData[[i]], pts=eicPts[[i]])))
  eic_sharpness_mean <- sapply(1:length(eic_sharpness_list), function(i){mean(eic_sharpness_list[[i]], na.rm=T)})

  # TPASR
  eic_tpasr_list <- lapply(1:numCols, function(i) mapply(calculateTPASR,
                                                         peakData=eicPeakData[[i]], pts=eicPts[[i]]))
  eic_tpasr_mean <- sapply(1:length(eic_tpasr_list), function(i){mean(eic_tpasr_list[[i]], na.rm=T)})

  # Zig-Zag Index
  eic_zigzag_list <- lapply(1:numCols, function(i) mapply(calculateZigZagIndex,
                                                          peakData=eicPeakData[[i]], pts=eicPts[[i]]))
  eic_zigzag_mean <- sapply(1:length(eic_zigzag_list), function(i){mean(eic_zigzag_list[[i]], na.rm=T)})


  # Combine metrics into dataframe
  metrics_df <- cbind.data.frame(eic_abr_mean, eic_elution_mean, eic_f2b_mean, eic_jagged_mean, eic_modality_mean, eic_RTC_mean,
                                eic_symmetry_mean, eic_gaussian_mean, eic_significance_mean, eic_sharpness_mean, eic_tpasr_mean, eic_zigzag_mean)
  colnames(metrics_df) <- c("ApexBoundaryRatio_mean", "ElutionShift_mean", "FWHM2Base_mean", "Jaggedness_mean", "Modality_mean", "RetentionTimeCorrelation_mean",
                            "Symmetry_mean", "GaussianSimilarity_mean", "PeakSignificance_mean", "Sharpness_mean", "TPASR_mean", "ZigZag_mean")
  metrics_df <- cbind(EICNo=eicNums, metrics_df, Class=eicLabels_df$Label)

  return(metrics_df)

}
