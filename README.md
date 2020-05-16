# **MetaClean**: Detection of Low-Quality Peaks in Untargeted LC-MS Metabolomics Data
MetaClean utilizes 12 peak-quality metrics and 9 diverse machine learning algorithms to build a classifier to detect and flag low-quality peaks in untargeted metabolomics data. It is an R package and can be easily incorporated into existing untargeted
LC-MS metabolomics pipelines that utilize the pre-processing software XCMS.

The 12 peak-quality metrics used by MetaClean are adapted from the following publications: <br />
Zhang, W., & Zhao, P. X. (2014). Quality evaluation of extracted ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics, 15(Suppl 11), S5. https://doi.org/10.1186/1471-2105-15-S11-S5 <br />
Eshghi, S. T., Auger, P., & Mathews, W. R. (2018). Quality assessment and interference detection in targeted mass spectrometry data using machine learning. Clinical Proteomics, 15. https://doi.org/10.1186/s12014-018-9209-x


## Installation
To install MetaClean from this GitHub page, run the following command:
```
remotes::install_github("KelseyChetnik/MetaClean")
```

Or install the latest stable CRAN version: <br />
*COMING SOON*

## Usage
Check out this [vignette](https://github.com/KelseyChetnik/MetaClean/blob/master/vignettes/MetaClean_WalkThrough.Rmd) for step-by-step instructions on how to use the MetaClean package.
