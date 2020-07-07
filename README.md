
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cyclingtools <img src='man/figures/logo.png' align="right" height="240" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/cyclingtools)](https://CRAN.R-project.org/package=cyclingtools)
[![R build
status](https://github.com/fmmattioni/cyclingtools/workflows/R-CMD-check/badge.svg)](https://github.com/fmmattioni/cyclingtools/actions)
<br>
<a href="https://www.buymeacoffee.com/XQauwUWGm" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" style="height: 30px !important;width: 174px !important;box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;-webkit-box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;" ></a>
<!-- badges: end -->

The goal of `cyclingtools` is to provide tools for making easier to
analyze data in cycling.

## Installation

You can install the development version of `cyclingtools` from
[GitHub](https://github.com/fmmattioni/cyclingtools) with:

``` r
# install.packages("remotes")
remotes::install_github("fmmattioni/cyclingtools")
```

## Usage

### Critical Power

#### Demo data

The package comes with a demonstration data frame to show how the
functions work and also to show you how you can setup your data:

``` r
library(cyclingtools)

demo_data
#>    PO  TTE
#> 1 446  100
#> 2 385  172
#> 3 324  434
#> 4 290  857
#> 5 280 1361
```

#### Simple analysis

Perform a simple analysis from the chosen critical power models:

``` r
simple_results <- critical_power(
  .data = demo_data, 
  power_output_column = "PO", 
  time_to_exhaustion_column = "TTE", 
  method = c("3-hyp", "2-hyp", "linear", "1/time"), 
  plot = TRUE, 
  all_combinations = FALSE
)

simple_results
#> # A tibble: 4 x 12
#>   method data  model CP    `CP SEE` `W'`  `W' SEE` Pmax  `Pmax SEE` R2    RMSE 
#>   <chr>  <lis> <lis> <chr> <chr>    <chr> <chr>    <chr> <chr>      <chr> <chr>
#> 1 3-hyp  <tib… <nls> 260.3 3.1      2740… 4794     1003… 834.7      0.99… 37.14
#> 2 2-hyp  <tib… <nls> 262.2 1.6      2417… 1888.9   <NA>  <NA>       0.997 34.7 
#> 3 linear <tib… <lm>  265.5 3        2096… 2247.9   <NA>  <NA>       0.99… 3149…
#> 4 1/time <tib… <lm>  273.8 6.2      1778… 1160     <NA>  <NA>       0.98… 9.05 
#> # … with 1 more variable: plot <list>
```

You can also plot the results:

``` r
simple_results %>% 
  dplyr::filter(method == "3-hyp") %>% 
  dplyr::pull(plot)
#> [[1]]
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

#### All possible combinations analysis

You can also perform an analysis with all the possible combinations of
time-to-exhaustion trials provided. All you need to do is to set
`all_combinations = TRUE`:

``` r
combinations_results <- critical_power(
  .data = demo_data, 
  power_output_column = "PO", 
  time_to_exhaustion_column = "TTE", 
  method = c("3-hyp", "2-hyp", "linear", "1/time"), 
  plot = TRUE, 
  all_combinations = TRUE
)

combinations_results
#> # A tibble: 74 x 13
#>    index method data  model CP    `CP SEE` `W'`  `W' SEE` Pmax  `Pmax SEE` R2   
#>    <chr> <chr>  <lis> <lis> <chr> <chr>    <chr> <chr>    <chr> <chr>      <chr>
#>  1 [1,2… 3-hyp  <tib… <nls> 260.3 3.1      2740… 4794     1003… 834.7      0.99…
#>  2 [1,2… 2-hyp  <tib… <nls> 262.2 1.6      2417… 1888.9   <NA>  <NA>       0.997
#>  3 [1,2… linear <tib… <lm>  265.5 3        2096… 2247.9   <NA>  <NA>       0.99…
#>  4 [1,2… 1/time <tib… <lm>  273.8 6.2      1778… 1160     <NA>  <NA>       0.98…
#>  5 [1,2… 3-hyp  <tib… <nls> 246.5 6.2      4269… 7655     594.8 70.2       0.99…
#>  6 [1,2… 2-hyp  <tib… <nls> 261.2 4.7      2481… 3690.8   <NA>  <NA>       0.99…
#>  7 [1,2… linear <tib… <lm>  268.8 5.9      1997… 2877.4   <NA>  <NA>       0.99…
#>  8 [1,2… 1/time <tib… <lm>  278.4 7.9      1719… 1338.8   <NA>  <NA>       0.98…
#>  9 [1,2… 3-hyp  <tib… <nls> 255   1.9      3646… 3328.4   626.9 63.7       0.99…
#> 10 [1,2… 2-hyp  <tib… <nls> 261.7 2.2      2494… 2882.2   <NA>  <NA>       0.99…
#> # … with 64 more rows, and 2 more variables: RMSE <chr>, plot <list>
```

You can also plot the results:

``` r
combinations_results %>% 
  dplyr::slice(1) %>% 
  dplyr::pull(plot)
#> [[1]]
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### You can also do all of these analyses in our shiny app:

[Critical Power Dashboard](https:shiny.fmattioni.me/CPapp/)

## Coming soon

  - Training impulse analyses (iTRIMP, bTRIMP, eTRIMP, luTRIMP)

  - Suggestions? Feel free to open an issue\!

## Related work

[cycleRtools](https://cran.r-project.org/web/packages/cycleRtools/index.html):
A suite of functions for analysing cycling data.

## Citation

``` r
citation("cyclingtools")
#> 
#> Maturana M, Felipe, Fontana, Y F, Pogliaghi, Silvia, Passfield, Louis,
#> Murias, M J (2018). "Critical power: How different protocols and models
#> affect its determination." _Journal of Science and Medicine in Sport_,
#> *21*(7), 1489. doi: 10.1016/j.jsams.2017.11.015 (URL:
#> https://doi.org/10.1016/j.jsams.2017.11.015).
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Critical power: How different protocols and models affect its determination},
#>     author = {Mattioni Maturana and {Felipe} and {Fontana} and Federico Y and {Pogliaghi} and {Silvia} and {Passfield} and {Louis} and {Murias} and Juan M},
#>     journal = {Journal of Science and Medicine in Sport},
#>     volume = {21},
#>     number = {7},
#>     pages = {1489},
#>     year = {2018},
#>     publisher = {Elsevier},
#>     doi = {10.1016/j.jsams.2017.11.015},
#>   }
```
