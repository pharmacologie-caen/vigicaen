# Summarise continuous variables

**\[stable\]** Summarize continuous data and handle output format.

## Usage

``` r
desc_cont(
  .data,
  vc,
  format = "median (q1-q3) [min-max]",
  digits = 1,
  export_raw_values = FALSE
)
```

## Arguments

- .data:

  A data.frame, where `vc` are column names of continuous variables

- vc:

  A character vector, list of column names. Should only contain
  continuous variables

- format:

  A character string. How would you like the output? See details.

- digits:

  A numeric. How many digits? This argument calls internal formatting
  function

- export_raw_values:

  A logical. Should the raw values be exported?

## Value

A data.frame with columns

- `var` the variable name

- `level` NA, it is provided to have a consistent output with
  [`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md)

- `value` the formatted value with possibly the median, interquartile
  range, and range (see details)

- `n_avail` the number of cases with available data for this variable.

## Details

Many other packages provide tools to summarize data. This one is just
the package author's favorite. This makes it much easier to map to nice
labeling thereafter. The `format` argument shows the output of the
function. You can change square and round brackets, spaces,
separators... Important `format` inputs are

- `median` the median value

- `q1` the first quartile

- `q3` the third quartile

- `min` the minimum value

- `max` the maximum value

The analogous for categorical variables is
[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md).

## See also

[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md)

## Examples

``` r
df <-
  data.frame(
    smoke_status = c("smoker", "non-smoker",
           "smoker", "smoker",
           "smoker", "smoker",
           "non-smoker"
           ),
    age = c(60, 50, 56, 49, 75, 69, 85),
    bmi = c(18, 30, 25, 22, 23, 21, 22)
  )

# Use default formatting

desc_cont(.data = df, vc = c("age", "bmi"))
#>   var level                        value n_avail
#> 1 age  <NA> 60.0 (53.0-72.0) [49.0-85.0]       7
#> 2 bmi  <NA> 22.0 (21.5-24.0) [18.0-30.0]       7

# Use custom formatting

desc_cont(.data = df,
          vc = c("age", "bmi"),
          format = "median (q1;q3)"
          )
#>   var level            value n_avail
#> 1 age  <NA> 60.0 (53.0;72.0)       7
#> 2 bmi  <NA> 22.0 (21.5;24.0)       7

# You might want to export raw values, to run plotting or
# other formatting functions

desc_cont(.data = df, vc = c("age", "bmi"),
          export_raw_values = TRUE)
#>   var level                        value n_avail median   q1 q3 min max
#> 1 age  <NA> 60.0 (53.0-72.0) [49.0-85.0]       7     60 53.0 72  49  85
#> 2 bmi  <NA> 22.0 (21.5-24.0) [18.0-30.0]       7     22 21.5 24  18  30
```
