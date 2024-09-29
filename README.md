
# vigicaen

<!-- badges: start -->

[![R-CMD-check](https://github.com/pharmacologie-caen/vigicaen/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pharmacologie-caen/vigicaen/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of vigicaen is to provide tools for worldwide pharmacovigilance
analysis.

## Installation

You can install the development version of vigicaen from
[GitHub](https://github.com/) with:

> `devtools::install_github("pharmacologie-caen/vigicaen")`

### Solution 2

Look at the **Releases** panel on the right of this Github page. You may
find “vX.XX.X (Latest)”.

Click on the version you want to install.

Download source code as a tar.gz file.

Now go to RStudio, click on “Tools”, “Install Packages…”, select
“Package Archive file” and locate the tar.gz file on your computer.

## Vignettes

//WIP

## Example

Say you want to create columns of drug and adr, then perform a
univariate disproportionality analysis. You may want to use the
`add_drug`, `add_adr`, and `compute_or_abcd` functions.

``` r
library(vigicaen)

demo <-
  demo_ |> 
  add_drug(
    d_code = ex_$d_drecno,
    drug_data = drug_
  ) |> 
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr_
  )

demo |> 
  compute_or_abcd(
    y = "a_colitis",
    x = "nivolumab"
  )
#> # A tibble: 1 × 18
#>   y         x          a     b     c     d n_exp std_er    or low_ci up_ci orl  
#>   <chr>     <chr>  <int> <int> <int> <int> <dbl>  <dbl> <dbl>  <dbl> <dbl> <chr>
#> 1 a_colitis nivol…    44    60   181   465  31.2  0.217  1.88   1.23  2.88 1.88 
#> # ℹ 6 more variables: or_ci <chr>, ic <dbl>, ic_tail <dbl>, ci_level <chr>,
#> #   signif_or <dbl>, signif_ic <dbl>
```

<!-- Footnote for myself
&#10;You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
