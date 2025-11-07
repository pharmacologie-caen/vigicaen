# Nice printing of p-values

**\[stable\]** Formatting function for consistent p-value reporting.

You can choose to print the leading zero (e.g. `0.01`) or not (e.g.
`.01`) with `print_zero`.

## Usage

``` r
nice_p(p_val, print_zero = FALSE)
```

## Arguments

- p_val:

  A numeric. The p-value to format.

- print_zero:

  A logical. Should leading zero be printed? (see Details)

## Value

A character vector with the formatted p-value(s)

## Examples

``` r
pvals <-
  c(0.056548, 0.0002654, 0.816546, 0.0493321)
nice_p(pvals)
#> [1] ".06"   "<.001" ".82"   ".049" 

nice_p(pvals, print_zero = TRUE)
#> [1] "0.06"   "<0.001" "0.82"   "0.049" 
```
