
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mk

Work-in-progress personal R package to store convenient functions, etc.

## Installation

Use `devtools` for installation

``` r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
```

Install a specific version from
[Releases](https://github.com/mikeknitz/mk/releases)

``` r
devtools::install_github("mikeknitz/mk", ref = "v0.0.0.9010")
```

## Recommended usage

Add lines to the top of a reproducible script to load (and not attach)
the namespace and check if the version number is as expected.

``` r
loadNamespace("mk")
stopifnot(packageVersion("mk") == "0.0.0.9010")
```

Since I am adding lots of various helper functions with no common theme,
best to avoid loading via `library()` and just explicitly use e.g.,
`mk::function_name()` when calling a function. Makes it easy to see in a
script where these custom functions are being used.

## Required and/or suggested dependencies

Suggested dependencies for certain functions, specified in `Suggests`

``` r
# Required for `mk::gene_symbol_to_name()`
BiocManager::install("AnnotationDbi")
BiocManager::install("org.Hs.eg.db")
BiocManager::install("org.Mm.eg.db")

# To make use of SummarizedExperiment related functions
BiocManager::install("SummarizedExperiment")

# To make use of Seurat related functions
install.packages("Seurat")

# For the Cairo::Cairo() device in `mk::saveplot()`, but function
# will work fine otherwise using devices from `grDevices` that
# comes pre-installed with R
install.packages("Cairo")

# For some of the suggested examples in `mk::plot_colors()` 
install.packages("RColorBrewer")
install.packages("viridis")
```

Required non-default dependencies automatically installed (specified in
`Imports`)

``` r
install.packages(dplyr)
install.packages(DT)
install.packages(forcats)
install.packages(ggh4x)
install.packages(ggplot2)
install.packages(htmltools)
install.packages(htmlwidgets)
install.packages(openxlsx)
install.packages(scales)
install.packages(stringr)
install.packages(tibble)
install.packages(tidyr)
```
