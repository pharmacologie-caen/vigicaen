# Compute interaction disproportionality

**\[experimental\]** Returns the information component of interaction
for a set of 3 variables, usually 2 drugs and an adr.

## Usage

``` r
compute_interaction(
  .data,
  y,
  x,
  z,
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

  A character vector, one or more variable to explain.

- x:

  A character vector, one or more explaining variable.

- z:

  A character vector, one or more explaining variable.

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

A data.table, with Information Component (IC) of interaction, and its
credibility interval (at `1 - alpha`). Significance as `signif_ic`, if
`export_raw_values` is TRUE).

A data.table with columns

- `y`, `x` and `z`, same as input

- `n_obs` the number of observed cases

- `n_exp` the number of expected cases

- `ic` the Information Component

- `ic_tail` the tail probability of the IC

- `ci_level` the confidence interval level

- Additional columns, if `export_raw_values` is `TRUE`:

- `a`, `b`, `c`, `d` the counts in the contingency table

- `signif_ic` the significance of the Information Component

- Additional columns, if `export_raw_values` is `TRUE`:

- `n_*` the counts of each setting

- `signif_ic` the significance of the Information Component

## Details

Significance is similar to usual disproportionality (see
[`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md)).

## See also

[`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md),
[`compute_or_mod()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_or_mod.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)

## Examples

``` r
# Interaction on reporting of colitis with ipilimumab and nivolumab
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
  compute_interaction(
    y = "a_colitis",
    x = "nivolumab",
    z = "ipilimumab"
  )
#> # A tibble: 1 × 8
#>   y         x         z          n_obs n_exp    ic ic_tail ci_level
#>   <chr>     <chr>     <chr>      <dbl> <dbl> <dbl>   <dbl> <chr>   
#> 1 a_colitis nivolumab ipilimumab    18  7.88  1.14   0.400 95%     
```
