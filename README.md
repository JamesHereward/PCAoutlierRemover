# PCAoutlierRemover

Interactive outlier removal from PCA for population genomic data.
Designed for ddRAD/SNP datasets filtered with `SNPfiltR` or similar
workflows.

## Installation

```r
# install.packages("devtools")
devtools::install_local("path/to/PCAoutlierRemover")
```

Or once on GitHub:

```r
devtools::install_github("jhereward/PCAoutlierRemover")
```

## Usage

```r
library(PCAoutlierRemover)
library(adegenet)

# Load your filtered genind and popmap
Genind.90 <- readRDS("GENIND_filtered.rds")
popmap    <- read.table("popmap.txt", header = TRUE)

# Run PCA (replicates SNPfiltR internals: tab + dudi.pca)
res <- compute_pca(Genind.90, popmap)

# Launch the gadget — returns selected IDs when you hit "Done"
remove_ids <- remove_outliers(res)

# Apply the filter
popmap    <- popmap[!popmap$id %in% remove_ids, ]
Genind.90 <- Genind.90[!indNames(Genind.90) %in% remove_ids, ]
```

You can also pass a precomputed scores data frame directly:

```r
remove_ids <- remove_outliers(my_scores, var_pct = my_var_pct)
```

## How it works in the gadget

- **Drag a rectangle** anywhere on the plot to brush-select all the
  points inside it
- **Click a single point** to toggle it on/off (10 px threshold)
- **Double-click inside a brushed area** to zoom; **double-click empty
  space** to reset the zoom
- **Switch X/Y axes** between any PCs from the dropdowns at the top
- **Adjust point size** with the slider (0.5 – 6)
- **Clear selection** wipes the list; **Export .txt** saves the IDs;
  **Done** returns them to R

A live R filter snippet is shown at the bottom as you select.

## License

MIT
