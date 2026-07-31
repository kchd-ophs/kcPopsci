
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kcPopsci

<!-- badges: start -->

[![R-CMD-check](https://github.com/kchd-ophs/kcPopsci/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kchd-ophs/kcPopsci/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

kcPopsci has tools to help with the following tasks in R:

- Calculating life expectancy estimates
- Downloading and visualizing data from ESSENCE

## Installation

You can install kcPopsci from GitHub:

``` r
remotes::install_github("kchd-ophs/kcPopsci", dependencies = TRUE)
#> Downloading GitHub repo kchd-ophs/kcPopsci@HEAD
#> diffobj (0.3.6     -> 0.3.8 ) [CRAN]
#> pkgload (1.5.2     -> 1.5.3 ) [CRAN]
#> Rcpp    (1.1.1-1.1 -> 1.1.2 ) [CRAN]
#> tinytex (0.59      -> 0.60  ) [CRAN]
#> xfun    (0.58      -> 0.60  ) [CRAN]
#> xml2    (1.5.2     -> 1.6.0 ) [CRAN]
#> plotly  (4.12.0    -> 4.12.1) [CRAN]
#> Installing 7 packages: diffobj, pkgload, Rcpp, tinytex, xfun, xml2, plotly
#> Installing packages into 'C:/Users/emonaco01/AppData/Local/Temp/RtmpQNeZGB/temp_libpath50f47ed57684'
#> (as 'lib' is unspecified)
#> package 'diffobj' successfully unpacked and MD5 sums checked
#> package 'pkgload' successfully unpacked and MD5 sums checked
#> package 'Rcpp' successfully unpacked and MD5 sums checked
#> package 'tinytex' successfully unpacked and MD5 sums checked
#> package 'xfun' successfully unpacked and MD5 sums checked
#> package 'xml2' successfully unpacked and MD5 sums checked
#> package 'plotly' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\emonaco01\AppData\Local\Temp\Rtmps3ZeRk\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>       ✔  checking for file 'C:\Users\emonaco01\AppData\Local\Temp\Rtmps3ZeRk\remotes60c050bd613e\kchd-ophs-kcPopsci-6d090c2/DESCRIPTION' (369ms)
#>       ─  preparing 'kcPopsci':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>       ─  building 'kcPopsci_0.0.0.9002.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/emonaco01/AppData/Local/Temp/RtmpQNeZGB/temp_libpath50f47ed57684'
#> (as 'lib' is unspecified)
```
