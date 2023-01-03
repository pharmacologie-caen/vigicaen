
# pharmacocaen

<!-- badges: start -->
<!-- badges: end -->

The goal of pharmacocaen is to provide tools for worldwide
pharmacovigilance analysis.

## Installation

You can install the development version of pharmacocaen from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cdolladille/pharmacocaen")
```

## Vignettes

There is a detailed vignette to explain simple data management
[here](https://github.com/cdolladille/pharmacocaen/tree/master/vignettes/)

## Example

Say you want to create columns of drug and adr, then perform a
univariate disproportionality analysis. You may want to use the
`add_drug`, `add_adr`, and `compute_or_abcd` functions.

``` r
library(pharmacocaen)

demo <-
  demo_ %>%
  add_drug(
    d_code = ex_$d_drecno,
    drug_data = drug_
  ) %>%
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr_
  )

demo %>%
  compute_or_abcd(
    y = "colitis",
    x = "nivolumab"
  )
#>          y         x    a    b     c     d        or    low_ci     up_ci  orl
#> 1: colitis nivolumab 3137 3935 33592 31413 0.7454926 0.7095811 0.7832216 0.75
#>          or_ci         ic    ic_tail ci_level signif_or signif_ic
#> 1: (0.71-0.78) -0.2000836 -0.2510166      95%         0         0
```

<!-- Footnote for myself

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
