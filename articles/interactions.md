# Interactions

``` r
library(vigicaen)
library(rlang)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Introduction

It is possible to explore interactions between drugs on an adr
reporting.

This tutorial does not aim at covering the concepts underlying
interactions in pharmacovigilance. It is about running them in practice.

In particular, we will not cover the differences between additive
interactions, statistical/synergistic interactions.

[`compute_interaction()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_interaction.md)
use is shown [at the end of the vignette](#compute_int).

We use built-in example dataset.

``` r

# ---- Tables ---- ####

demo <- demo_
drug <- drug_

# ---- Dictionary step ---- ####

d_drecno <- ex_$d_drecno
a_llt <- ex_$a_llt

# #### Data management #### ####

# ---- Drugs ---- ####

demo <-
    demo |>
    add_drug(
      d_code = d_drecno,
      drug_data = drug_
    ) 
#> ℹ `.data` detected as `demo` table.

# ---- Adrs ---- ####

demo <- 
  demo |>
    add_adr(
      a_code = a_llt,
      adr_data = adr_
    )
#> ℹ `.data` detected as `demo` table.

# ---- Sex ---- ####

demo <- 
  demo |> 
  mutate(
    sex = case_when(Gender == "1" ~ 1,
                    Gender == "2" ~ 2,
                    TRUE ~ NA_real_)
    )
```

## Additive interactions

### Multivariate analysis

Additive effect of two covariates can be obtained by multiplying the
Odds-Ratio of each.

``` r
mod3 <- glm(a_colitis ~ ipilimumab + sex,
            data = demo,
            family = "binomial")

mod_or <- 
  compute_or_mod(
  summary(mod3)$coefficients,
      estimate = Estimate,
    std_er = Std..Error
  ) |> select(rn, orl, ci, up_ci)

mod_or
#>             rn    orl          ci     up_ci
#>         <char> <char>      <char>     <num>
#> 1: (Intercept)   0.15 (0.08-0.28) 0.2830974
#> 2:  ipilimumab   2.00 (1.14-3.53) 3.5274694
#> 3:         sex   1.06 (0.69-1.62) 1.6167786
```

With reporting Odds-Ratio of ipilimumab being 2.00 and the reporting
Odds-Ratio of sex being 1.06, the additive effect of both is 2.00 \*
1.06.

### Subgroup comparisons

Some way to approach multiplicative interactions is to compare the
disproportionality signal in subgroups.

The
[`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md)
function can be used for these analyses, assuming the initial dataset is
filtered on the appropriate subgroup.

Say we want to investigate the interaction between ipilimumab and
nivolumab and colitis reporting.

``` r
demo |> 
  filter(nivolumab == 1) |> 
  compute_dispro(
    y = "a_colitis",
    x = "ipilimumab"
    )
#> # A tibble: 1 × 9
#>   y         x          n_obs n_exp or    or_ci          ic ic_tail ci_level
#>   <chr>     <chr>      <dbl> <dbl> <chr> <chr>       <dbl>   <dbl> <chr>   
#> 1 a_colitis ipilimumab    18  11.5 2.36  (1.18-4.73) 0.620  -0.123 95%
```

The overall analysis implies to perform additional analysis in different
settings.

In our example:

- If ipilimumab alone leads to an overreporting of colitis
- If nivolumab, among ipilimumab cases, leads to an overreporting of
  colitis
- If the combination ipilimumab + nivolumab versus any control (or the
  whole dataset) increases the notifications of colitis.

Both IC and ROR can be used here.

## Statistical interactions

### Logistic regression model

The true statistical interaction is obtained with the following model

``` r
mod4 <- glm(a_colitis ~ ipilimumab + sex + ipilimumab * sex,
            data = demo,
            family = "binomial")

compute_or_mod(
  summary(mod4)$coefficients,
  estimate = Estimate,
  std_er = Std..Error
)
#>                rn   Estimate Std..Error   z.value     Pr...z..         or
#>            <char>      <num>      <num>     <num>        <num>      <num>
#> 1:    (Intercept) -2.2464633  0.3632657 -6.184077 6.246672e-10  0.1057727
#> 2:     ipilimumab  2.7892022  0.8927900  3.124141 1.783247e-03 16.2680357
#> 3:            sex  0.2910744  0.2363515  1.231532 2.181241e-01  1.3378641
#> 4: ipilimumab:sex -1.4905928  0.6289493 -2.369973 1.778940e-02  0.2252391
#>        low_ci      up_ci    orl           ci ci_level signif_ror
#>         <num>      <num> <char>       <char>   <char>      <num>
#> 1: 0.05189925  0.2155687   0.11  (0.05-0.22)      95%          0
#> 2: 2.82742376 93.6007499  16.27 (2.83-93.60)      95%          1
#> 3: 0.84183934  2.1261543   1.34  (0.84-2.13)      95%          0
#> 4: 0.06565701  0.7726920   0.23  (0.07-0.77)      95%          0
```

### Bayesian Information Component, `compute_interaction()`

The formula for the interaction between 3 variables ($y$, the event of
interest, $x1$ and $x2$, two potential explanatory factors) in
information component is

$log_{2}\frac{n_{y,x1,x2}}{n.expected_{interaction}}$

with $n.expected_{interaction}$ equal to

$\frac{n_{x1,x2}*n_{y,x1}*n_{y,x2}*n.total}{n_{x1}*n_{x2}*n_{y}}$

The parameters are read as follows

| Parameter     | Case                                              |
|---------------|---------------------------------------------------|
| $n_{x1}$      | number of cases reporting x1                      |
| $n_{x1,x2}$   | number of cases reporting x1 **AND** x2           |
| $n_{y,x1,x2}$ | number of cases reporting x1 **AND** x2 **AND** y |
| $n.total$     | total number of cases in the study population     |

The credibility interval is calculated as for the usual IC.

[`compute_interaction()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_interaction.md)
produces this interaction statistic.

``` r

demo |>
  compute_interaction(
    y = "a_colitis",
    x = "ipilimumab",
    z = "nivolumab"
  )
#> # A tibble: 1 × 8
#>   y         x          z         n_obs n_exp    ic ic_tail ci_level
#>   <chr>     <chr>      <chr>     <dbl> <dbl> <dbl>   <dbl> <chr>   
#> 1 a_colitis ipilimumab nivolumab    18  7.88  1.14   0.400 95%
```
