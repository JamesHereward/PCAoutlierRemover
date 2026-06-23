#' Compute PCA from a genind object
#'
#' Replicates the PCA logic used by `SNPfiltR::assess_missing_data_pca()`:
#' allele frequencies are extracted with NA imputed by column mean, then
#' `ade4::dudi.pca()` is run unscaled.
#'
#' @param genind A `genind` object, typically from `vcfR2genind()` after
#'   SNP filtering.
#' @param popmap Optional data frame with columns `id` and `pop`. If
#'   supplied, populations are matched to `indNames(genind)` and assigned
#'   to the genind before PCA.
#' @param nf Number of axes to retain. Default 10.
#'
#' @return A list with components:
#' \describe{
#'   \item{scores}{Data frame of sample scores with `id`, `pop`, and
#'     `PC1`...`PCn` columns.}
#'   \item{var_pct}{Numeric vector of percent variance explained per axis.}
#'   \item{loadings}{Variable loadings (data frame).}
#'   \item{pca}{The raw `dudi.pca` result object.}
#' }
#'
#' @examples
#' \dontrun{
#' gen <- readRDS("GENIND_filtered.rds")
#' popmap <- read.table("popmap.txt", header = TRUE)
#' res <- compute_pca(gen, popmap)
#' head(res$scores)
#' }
#'
#' @export
compute_pca <- function(genind, popmap = NULL, nf = 10) {

  if (!inherits(genind, "genind"))
    stop("`genind` must be a genind object")

  if (!is.null(popmap)) {
    if (!all(c("id", "pop") %in% colnames(popmap)))
      stop("`popmap` must have columns 'id' and 'pop'")
    pop_ordered <- popmap$pop[match(adegenet::indNames(genind), popmap$id)]
    adegenet::pop(genind) <- pop_ordered
  }

  X <- adegenet::tab(genind, freq = TRUE, NA.method = "mean")
  pca <- ade4::dudi.pca(X, scale = FALSE, scannf = FALSE, nf = nf)

  var_pct <- round((pca$eig / sum(pca$eig)) * 100, 2)

  scores <- as.data.frame(pca$li)
  colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
  scores$id  <- adegenet::indNames(genind)
  scores$pop <- as.character(adegenet::pop(genind))
  scores <- scores[, c("id", "pop", paste0("PC", seq_len(nf)))]

  loadings <- as.data.frame(pca$c1)
  colnames(loadings) <- paste0("PC", seq_len(ncol(loadings)))
  loadings$variable <- rownames(loadings)
  rownames(loadings) <- NULL

  list(
    scores   = scores,
    var_pct  = var_pct,
    loadings = loadings,
    pca      = pca
  )
}


#' Default colour palette for populations
#'
#' Returns a named colour vector for a set of populations, recycling
#' through the built-in palette as needed.
#'
#' @param populations Character vector of population names.
#' @param palette Optional custom hex palette to recycle through. If NULL,
#'   uses the package default.
#'
#' @return A named character vector of hex colours.
#'
#' @examples
#' default_palette(c("popA", "popB", "popC"))
#'
#' @export
default_palette <- function(populations, palette = NULL) {
  if (is.null(palette)) {
    palette <- c(
      "#5E4FBE", "#1D9E75", "#D85A30", "#D4537E",
      "#378ADD", "#639922", "#BA7517", "#E24B4A",
      "#888780", "#7BCCC4", "#F768A1", "#41B6C4"
    )
  }
  stats::setNames(rep_len(palette, length(populations)), populations)
}
