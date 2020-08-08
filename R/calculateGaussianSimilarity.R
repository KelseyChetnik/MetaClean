#' Calculate Gaussian Similarity (of a Chromatographic Peak)
#'
#' Calculates the Gaussian Similarity of the integrated region of a chromatographic peak. The Gaussian Similarity is found by
#' calculating the dot product of the standard normalized intensity values of a chromatographic peak and the standard normalized
#' intensity values of a Gaussian curve fitted to the intensities of the original curve.
#'
#' This function repurposed from Zhang et al. For details, see Zhang, W., & Zhao, P. X. (2014). Quality evaluation of extracted
#' ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics,
#' 15(Suppl 11), S5. https://doi.org/10.1186/1471-2105-15-S11-S5
#'
#' @param peakData A vector containing characteristic information about a chromatographic peak - including the retention time range
#' @param pts A 2D matrix containing the retention time and intensity values of a chromatographic peak
#' @return The Gaussian Similarity value (double)
#'
#' @importFrom stats fitted
#' @importFrom stats nls
#' @importFrom stats sd
#'
#' @examples
#' # Calculate Gaussian Similarity for a peak
#' data(ex_pts)
#' data(ex_peakData)
#' gaussianSimilarity <- calculateGaussianSimilarity(peakData = ex_peakData, pts = ex_pts)
#'
#' @export

calculateGaussianSimilarity <- function(peakData, pts){
  peakrange <- peakData[c("rtmin", "rtmax")]
  ptsidx <- pts[, 1] >= peakrange[1] & pts[, 1] <= peakrange[2]
  intPts <- pts[ptsidx, ]

  if(length(intPts) > 2){
    num_peak_pts <- length(intPts[,2])
    td <- intPts[,1]
    d <- intPts[,2]
    mu <- peakData["rt"]
    sigma <- peakData["rtmax"] - peakData["rtmin"]
    h <- peakData["maxo"]

    fit <- try(nls(d ~ SSgauss(td, mu, sigma, h)), silent = TRUE)

    if(class(fit) != "try-error"){
      gaussPts <- as.matrix(fitted(fit))
      gaussPts_std <- (gaussPts-mean(gaussPts))/sd(gaussPts)
      gaussPts_scale <- gaussPts_std/norm(gaussPts_std, type="F")

      d <- as.matrix(d)
      peak_intensity_std <- (d-mean(d))/sd(d)
      peak_intensity_scale <- peak_intensity_std/norm(peak_intensity_std, type="F")

      gauss_similarity <- sum(gaussPts_scale*peak_intensity_scale)

    }else{
      gauss_similarity <- NA
    }
  }else{
    gauss_similarity <- NA
  }
  return(gauss_similarity)
}
