# Compute disproportionality

**\[stable\]** Computes bivariate (reporting) Odds-Ratio and Information
Component for a drug-adr pair.

## Usage

``` r
compute_dispro(
  .data,
  y,
  x,
  alpha = 0.05,
  na_format = "-",
  dig = 2,
  export_raw_values = FALSE,
  min_n_obs = 0
)
```

## Arguments

- .data:

  The data.table to compute from.

- y:

  A character vector, one or more variable to explain (usually an adr).

- x:

  A character vector, one or more explaining variable (usually a drug).

- alpha:

  Alpha risk.

- na_format:

  Character string to fill NA values in ror and ci legends.

- dig:

  Number of digits for rounding (this argument is passed to `cff`)

- export_raw_values:

  A logical. Should the raw values be exported?

- min_n_obs:

  A numeric, compute disproportionality only for pairs with at least
  `min_n_obs` cases.

## Value

A data.table, with ROR, IC, and their confidence/credibility interval
(at `1 - alpha`). Significance of both (as `signif_or` and `signif_ic`,
if `export_raw_values` is TRUE).

A data.table with columns

- `y` and `x`, same as input

- `n_obs` the number of observed cases

- `n_exp` the number of expected cases

- `orl` the formatted Odds-Ratio

- `or_ci` the formatted confidence interval

- `ic` the Information Component

- `ic_tail` the tail probability of the IC

- `ci_level` the confidence interval level

- Additional columns, if `export_raw_values` is `TRUE`:

- `a`, `b`, `c`, `d` the counts in the contingency table

- `std_er` the standard error of the log(OR)

- `or` the Odds-Ratio

- `low_ci` the lower bound of the confidence interval

- `up_ci` the upper bound of the confidence interval

- `signif_or` the significance of the Odds-Ratio

- `signif_ic` the significance of the Information Component

## Details

Significance in pharmacovigilance analysis is only defined if the lower
bound of the confidence/credibility interval is above 1 (i.e.
`low_ci > 1`, or `ic_tail > 0`). Actually, the function computes an
Odds-Ratio, which is not necessarily a **reporting** Odds-Ratio.

## See also

[`compute_or_mod()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_or_mod.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)

## Examples

``` r
# Say you want to perform a disproportionality analysis between colitis and
# nivolumab among ICI cases

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
#> ℹ `.data` detected as `demo` table.
#> ℹ `.data` detected as `demo` table.

demo |>
  compute_dispro(
    y = "a_colitis",
    x = "nivolumab"
  )
#> # A tibble: 1 × 9
#>   y         x         n_obs n_exp or    or_ci          ic ic_tail ci_level
#>   <chr>     <chr>     <dbl> <dbl> <chr> <chr>       <dbl>   <dbl> <chr>   
#> 1 a_colitis nivolumab    44  31.2 1.88  (1.23-2.88) 0.489  0.0314 95%     

# You don't have to use the pipe syntax, if you're not familiar

compute_dispro(
    .data = demo,
    y = "a_colitis",
    x = "nivolumab"
  )
#> # A tibble: 1 × 9
#>   y         x         n_obs n_exp or    or_ci          ic ic_tail ci_level
#>   <chr>     <chr>     <dbl> <dbl> <chr> <chr>       <dbl>   <dbl> <chr>   
#> 1 a_colitis nivolumab    44  31.2 1.88  (1.23-2.88) 0.489  0.0314 95%     


# Say you want to compute more than one univariate ror at a time.

many_drugs <-
  names(ex_$d_drecno)

demo |>
  compute_dispro(
    y = "a_colitis",
    x = many_drugs
  )
#> # A tibble: 8 × 9
#>   y         x             n_obs n_exp or    or_ci           ic  ic_tail ci_level
#>   <chr>     <chr>         <dbl> <dbl> <chr> <chr>        <dbl>    <dbl> <chr>   
#> 1 a_colitis ipilimumab       20 11.9  2.09  (1.21-3.6…  0.722    0.0210 95%     
#> 2 a_colitis atezolizumab      7  9.57 0.68  (0.30-1.5… -0.425   -1.69   95%     
#> 3 a_colitis durvalumab        9  9.43 0.94  (0.45-1.9… -0.0638  -1.16   95%     
#> 4 a_colitis nivolumab        44 31.2  1.88  (1.23-2.8…  0.489    0.0314 95%     
#> 5 a_colitis pembrolizumab    40 41.3  0.94  (0.61-1.4… -0.0464  -0.528  95%     
#> 6 a_colitis avelumab          2  5.96 0.29  (0.07-1.2… -1.37    -3.96   95%     
#> 7 a_colitis cemiplimab        0  3.74 -     -          -3.09   -13.1    95%     
#> 8 a_colitis tremelimumab      8  3.74 2.75  (1.17-6.4…  1.00    -0.166  95%     


# could do the same with adrs

many_adrs <-
  names(ex_$a_llt)

demo |>
compute_dispro(
  y = many_adrs,
  x = many_drugs
)
#> # A tibble: 24 × 9
#>    y             x            n_obs  n_exp or    or_ci      ic  ic_tail ci_level
#>    <chr>         <chr>        <dbl>  <dbl> <chr> <chr>   <dbl>    <dbl> <chr>   
#>  1 a_embolism    ipilimumab       1  1.03  0.96  (0.1… -0.0305  -3.83   95%     
#>  2 a_colitis     ipilimumab      20 11.9   2.09  (1.2…  0.722    0.0210 95%     
#>  3 a_pneumonitis ipilimumab      12 11.8   1.02  (0.5…  0.0220  -0.908  95%     
#>  4 a_embolism    atezolizumab     1  0.828 1.24  (0.1…  0.176   -3.62   95%     
#>  5 a_colitis     atezolizumab     7  9.57  0.68  (0.3… -0.425   -1.69   95%     
#>  6 a_pneumonitis atezolizumab     0  9.48  -     -     -4.32   -14.3    95%     
#>  7 a_embolism    durvalumab       1  0.816 1.26  (0.1…  0.189   -3.61   95%     
#>  8 a_colitis     durvalumab       9  9.43  0.94  (0.4… -0.0638  -1.16   95%     
#>  9 a_pneumonitis durvalumab       6  9.34  0.58  (0.2… -0.598   -1.97   95%     
#> 10 a_embolism    nivolumab        1  2.7   0.29  (0.0… -1.09    -4.89   95%     
#> # ℹ 14 more rows

# Export raw values if you want to built plots, or other tables.

demo |>
  compute_dispro(
    y = "a_colitis",
    x = "nivolumab",
    export_raw_values = TRUE
  )
#> # A tibble: 1 × 19
#>   y       x     n_obs n_exp or    or_ci    ic ic_tail ci_level     a     b     c
#>   <chr>   <chr> <dbl> <dbl> <chr> <chr> <dbl>   <dbl> <chr>    <dbl> <dbl> <dbl>
#> 1 a_coli… nivo…    44  31.2 1.88  (1.2… 0.489  0.0314 95%         44    60   181
#> # ℹ 7 more variables: d <dbl>, std_er <dbl>, or_raw <dbl>, low_ci <dbl>,
#> #   up_ci <dbl>, signif_or <dbl>, signif_ic <dbl>

# Set a minimum number of observed cases to compute disproportionality

demo |>
 compute_dispro(
 y = "a_colitis",
 x = "nivolumab",
 min_n_obs = 5
 )
#> # A tibble: 1 × 9
#>   y         x         n_obs n_exp or    or_ci          ic ic_tail ci_level
#>   <chr>     <chr>     <dbl> <dbl> <chr> <chr>       <dbl>   <dbl> <chr>   
#> 1 a_colitis nivolumab    44  31.2 1.88  (1.23-2.88) 0.489  0.0314 95%     
```
