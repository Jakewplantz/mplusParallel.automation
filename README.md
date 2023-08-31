# mplusParallel.automation

The `mplusParallel.automation` package provides parallelized automation for Mplus when using R as the data generation method. At present, it supports a specific set of functionalities designed to integrate with Mplus, and uses several underlying packages for its operations, including `doParallel`, `dplyr`, `furrr`, `future`, and `MplusAutomation`. This package is currently in the developmental stage and is not available on CRAN. However, you can install it directly from GitHub:

```r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("YOUR_GITHUB_USERNAME/mplusParallel.automation")
