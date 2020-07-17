#' Example Peak Quality Metrics Data Frame for Development Dataset.
#'
#' Data frame with peaks quality metrics and labels for all of the 500 EICs in the example development dataset.
#'
#' @format A data frame with 14 variables (EIC Number, the 12 peak quality metrics, and Class Labels): \code{EICNo},
#' \code{ApexBoundaryRatio_mean}, \code{ElutionShift_mean}, \code{FWHM2Base_mean}, \code{Jaggedness_mean}, \code{Modelaity_mean},
#' \code{RetentionTimeCorrelation_mean}, \code{Symmetry_mean}, \code{GaussianSimilarity_mean}, \code{PeakSignificance_mean},
#' \code{Sharpness_mean}, \code{TPASR_mean}, \code{ZigZag_mean}, and \code{Class}.
#'
"pqMetrics_development"

#' Example Peak Quality Metrics Data Frame for Test Dataset.
#'
#' Data frame with peaks quality metrics and labels for all of the 500 EICs in the example test dataset.
#'
#' @format A data frame with 14 variables (EIC Number, the 12 peak quality metrics, and Class Labels): \code{EICNo},
#' \code{ApexBoundaryRatio_mean}, \code{ElutionShift_mean}, \code{FWHM2Base_mean}, \code{Jaggedness_mean}, \code{Modelaity_mean},
#' \code{RetentionTimeCorrelation_mean}, \code{Symmetry_mean}, \code{GaussianSimilarity_mean}, \code{PeakSignificance_mean},
#' \code{Sharpness_mean}, \code{TPASR_mean}, \code{ZigZag_mean}, and \code{Class}.
#'
"pqMetrics_test"


#' Example pts - value input to caculate... functions (except calculateElutionShift and calculateRetentionTimeConsistency)
#'
#' An example of the input for the pts argument for calcualte... functions. It represents rt and intensity data from one sample for
#' peak of interest.
#'
#' @format A two-column matrix where the first column represents \code{rt} and the second column represents
#' \code{intensity}.
#'
"ex_pts"


#' Example peakData - value input to calculate... functions (except calculateElutionShift and calculateRetentionTimeConsistency)
#'
#' An example of the input for the peakData argument for calculate... functions. It represents data from one sample for the peak of
#' interest.
#'
#' @format A list containing the following entries: \code{mz}, \code{mzmin}, \code{mzmax}, \code{rt}, \code{rtmin}, \code{rtmax}, \code{into},
#' \code{intb}, \code{maxo}, \code{sn}, \code{sample}, and \code{is_filled}.
#'
"ex_peakData"


#' Example ptsList - value input to calculteElutionShift and calculateRetentionTimeConsistency
#'
#' An example of the input for the ptsList argument for calculteElutionShift and calculateRetentionTimeConsistency. Each entry in the
#' list is a two-column matrix consisting of rt and intensity for a sample for the peak of interest.
#'
#' @format A list of two-column matrices (one matrix per sample) where the first column represents \code{rt} and the second column
#'  represents \code{intensity}.
#'
"ex_ptsList"


#' Example peakDataList - value input to calculteElutionShift and calculateRetentionTimeConsistency
#'
#' An example of the input for the peakDataList argument for calculteElutionShift and calculateRetentionTimeConsistency. Each entry
#' in the list is represents data for a sample for the peak of interest.
#'
#' @format A list of lists. Each nested list contains the following entries: \code{mz}, \code{mzmin}, \code{mzmax}, \code{rt},
#' \code{rtmin},  \code{rtmax}, \code{into}, \code{intb}, \code{maxo}, \code{sn}, \code{sample}, and \code{is_filled}.
#'
"ex_peakDataList"
