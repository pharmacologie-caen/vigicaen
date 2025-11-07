# Summarise categorical variables

**\[stable\]** Summarize categorical data and handle output format.

## Usage

``` r
desc_facvar(
  .data,
  vf,
  format = "n_/N_ (pc_%)",
  digits = 0,
  pad_width = 12,
  ncat_max = 20,
  export_raw_values = FALSE
)
```

## Arguments

- .data:

  A data.frame, where `vf` are column names of categorical variables

- vf:

  A character vector

- format:

  A character string, formatting options.

- digits:

  A numeric. Number of digits for the percentage (passed to interval
  formatting function).

- pad_width:

  A numeric. Minimum character length of value output (passed to
  [`stringr::str_pad()`](https://stringr.tidyverse.org/reference/str_pad.html)).

- ncat_max:

  A numeric. How many levels should be allowed for all variables? See
  details.

- export_raw_values:

  A logical. Should the raw values be exported?

## Value

A data.frame with columns

- `var` the variable name

- `level` the level of the variable

- `value` the formatted value with possible number of cases `n_`, number
  of available cases `N_`, and percentage `pc_`, depending on format
  argument.

- `n_avail` the number of cases with available data for this variable.

## Details

Many other packages provide tools to summarize data. This one is just
the package author's favorite. Important `format` inputs are

- `n_` number of patients with the categorical variable at said level

- `N_` the first quartile number of patients with an available value for
  this variable

- `pc_` percentage of n / N

The format argument should contain at least the words "n\_", "N\_", and
optionally "pc\_". `ncat_max` ensures that you didn't provided a
continuous variable to `desc_facvar()`. If you have many levels for one
of your variables, set to `Inf` or high value. Equivalent for continuous
data is
[`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md).

## See also

[`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md)

## Examples

``` r
df1 <-
  data.frame(
    smoke_status = c("smoker", "non-smoker",
           "smoker", "smoker",
           "smoker", "smoker",
           "non-smoker"
           ),
   hypertension = c(1, 1, 0, 1, 1, 1, 1),
    age = c(60, 50, 56, 49, 75, 69, 85),
    bmi = c(18, 30, 25, 22, 23, 21, 22)
  )

# Use default formatting
desc_facvar(.data = df1, vf = c("hypertension", "smoke_status"))
#> # A tibble: 4 × 4
#>   var          level      value          n_avail
#>   <chr>        <chr>      <chr>            <int>
#> 1 hypertension 0          " 1/7 (14%)  "       7
#> 2 hypertension 1          " 6/7 (86%)  "       7
#> 3 smoke_status non-smoker " 2/7 (29%)  "       7
#> 4 smoke_status smoker     " 5/7 (71%)  "       7

# Use custom formatting
desc_facvar(.data = df1,
           vf = c("hypertension", "smoke_status"),
           format = "n_ out of N_, pc_%",
           digits = 1)
#> # A tibble: 4 × 4
#>   var          level      value             n_avail
#>   <chr>        <chr>      <chr>               <int>
#> 1 hypertension 0          1 out of 7, 14.3%       7
#> 2 hypertension 1          6 out of 7, 85.7%       7
#> 3 smoke_status non-smoker 2 out of 7, 28.6%       7
#> 4 smoke_status smoker     5 out of 7, 71.4%       7

# You might want to export raw values, to run plotting or
# other formatting functions

desc_facvar(.data = df1,
            vf = c("hypertension", "smoke_status"),
            export_raw_values = TRUE)
#> # A tibble: 4 × 6
#>   var          level      value          n_avail     n    pc
#>   <chr>        <chr>      <chr>            <int> <int> <dbl>
#> 1 hypertension 0          " 1/7 (14%)  "       7     1  14.3
#> 2 hypertension 1          " 6/7 (86%)  "       7     6  85.7
#> 3 smoke_status non-smoker " 2/7 (29%)  "       7     2  28.6
#> 4 smoke_status smoker     " 5/7 (71%)  "       7     5  71.4
```
