# Compute (r)OR from a model summary

**\[stable\]** Compute and format Odds-Ratio from a model summary.

## Usage

``` r
compute_or_mod(.coef_table, estimate, std_er, p_val = NULL, alpha = 0.05)
```

## Arguments

- .coef_table:

  A coefficient table, see details.

- estimate:

  Quasiquoted name of estimate parameter.

- std_er:

  Quasiquoted name of standard error parameter.

- p_val:

  Quasiquoted name of p-value parameter. Optional.

- alpha:

  alpha risk.

## Value

A data.table with OR, confidence intervals (at `1 - alpha`),
significance (`low_ci > 1`) and (optionally) p-value.

## Details

Helper to compute and format Odds-Ratio based on
`summary(glm)$coefficients`, or any equivalent in other modelling
packages. (see examples). Preferably, it is transformed into a
data.table or data.frame before being evaluated in the function.
Otherwise, `compute_or_mod()` will transform it. Significant OR-or
column means low_ci is \> 1. The `p_val` argument is only required if
you wished to display a
[`nice_p()`](https://pharmacologie-caen.github.io/vigicaen/reference/nice_p.md).

Output is a data.table. Actually, the function computes an Odds-Ratio,
which is not necessarily a *reporting* Odds-Ratio.

## See also

[`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)

## Examples

``` r
# Reporting Odds-Ratio of colitis with nivolumab among ICI cases.

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

# Compute the model
mod <- glm(a_colitis ~ nivolumab, data = demo, family = "binomial")

# Extract coefficients
mod_summary <-
 mod |>
 summary()

coef_table <-
 mod_summary$coefficients

# Transform coefficients into ORs with their CI

coef_table |>
  compute_or_mod(
  estimate = Estimate,
  std_er = Std..Error,
  p_val = Pr...z..)
#>             rn   Estimate Std..Error    z.value     Pr...z..        or
#>         <char>      <num>      <num>      <num>        <num>     <num>
#> 1: (Intercept) -2.0476928  0.1371736 -14.927752 2.174746e-50 0.1290323
#> 2:   nivolumab  0.6333854  0.2169532   2.919456 3.506429e-03 1.8839779
#>        low_ci     up_ci    orl          ci ci_level signif_ror  p_val
#>         <num>     <num> <char>      <char>   <char>      <num> <char>
#> 1: 0.09861341 0.1688343   0.13 (0.10-0.17)      95%          0 <.0001
#> 2: 1.23141622 2.8823501   1.88 (1.23-2.88)      95%          1   <.01

# Also works if you don't have a p_val column
 coef_table |>
  compute_or_mod(
  estimate = Estimate,
  std_er = Std..Error)
#>             rn   Estimate Std..Error    z.value     Pr...z..        or
#>         <char>      <num>      <num>      <num>        <num>     <num>
#> 1: (Intercept) -2.0476928  0.1371736 -14.927752 2.174746e-50 0.1290323
#> 2:   nivolumab  0.6333854  0.2169532   2.919456 3.506429e-03 1.8839779
#>        low_ci     up_ci    orl          ci ci_level signif_ror
#>         <num>     <num> <char>      <char>   <char>      <num>
#> 1: 0.09861341 0.1688343   0.13 (0.10-0.17)      95%          0
#> 2: 1.23141622 2.8823501   1.88 (1.23-2.88)      95%          1
```
