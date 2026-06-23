#' PCAoutlierRemover: Interactive outlier removal from PCA
#'
#' Provides an interactive Shiny gadget for identifying and removing
#' outlier samples from 2D PCA scatter plots of population genomic data.
#'
#' @section Typical workflow:
#' 1. Filter your VCF (e.g. with SNPfiltR)
#' 2. Convert to genind with `vcfR::vcfR2genind()`
#' 3. Call `compute_pca()` to get scores
#' 4. Call `remove_outliers()` to launch the gadget
#' 5. Apply the returned ID list to filter your genind/vcfR
#'
#' @keywords internal
"_PACKAGE"
