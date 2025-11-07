# Fast formatting of numbers

This is a formatting function for consistent number reporting.

## Usage

``` r
cff(num, low_ci, up_ci, dig = 0, method = c("num_only", "num_ci", "ci"))
```

## Arguments

- num:

  A numeric. The number to format.

- low_ci:

  A numeric. Lower end of a confidence interval

- up_ci:

  A numeric. Upper end of a confidence interval

- dig:

  A numeric. Number of digits

- method:

  What sort of printing do you need? (see Details)

## Value

A character vector with the formatted number(s)

## Details

Set `method` according to the printing you like: a unique number with
`num_only` (default), the number and its confidence interval with
`num_ci`, a `ci` only (for example a range of time to onset) The
function properly returns `NA` when input is missing.

## Examples

``` r
num <- c(0.1, 0.02, 1.658)

cff(num)
#> [1] "0" "0" "2"

cff(num, dig = 2)
#> [1] "0.10" "0.02" "1.66"

cff(num = num[[1]],
     low_ci = num[[2]],
     up_ci = num[[3]],
     method = "num_ci",
     dig = 2)
#> [1] "0.10 (0.02-1.66)"
```
