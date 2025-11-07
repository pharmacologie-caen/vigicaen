# Check binary variables

**\[stable\]** Quick check that your data management steps through
[`add_adr`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)
or
[`add_drug`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
found cases.

## Usage

``` r
check_dm(.data, cols)
```

## Arguments

- .data:

  A data.frame to be checked

- cols:

  A character vector, name of columns to look at (usually will be
  `d_names`, `a_names`)

## Value

A transposed data.frame, with row.names equal to `cols`, and first
column is the number of lines in `.data` where each col is equal to `1`.

## Details

It is a simple wrapper around
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
Be careful not to supply factors with \> 2 levels or continuous outcome
(the function does NOT have a checker for this, so that it is faster).
Also, the function WONT work with NAs. Use
[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md).
if you need more detailed description of your dataset.

## See also

[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)

## Examples

``` r
# first create some new variables

demo <- demo_

demo <-
  demo |>
    add_adr(
      a_code = ex_$a_llt,
      adr_data = adr_
    )
#> ℹ `.data` detected as `demo` table.

 # then check the number of reports with each feature

demo |>
  check_dm(names(ex_$a_llt))
#>               [,1]
#> a_embolism       9
#> a_colitis      104
#> a_pneumonitis  103
```
