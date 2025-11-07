# Screening of drugs

**\[experimental\]** The `screen_drug()` function identifies and ranks
the most frequently reported drugs (by active ingredient) in a dataset.

## Usage

``` r
screen_drug(.data, mp_data, freq_threshold = NULL, top_n = NULL)
```

## Arguments

- .data, :

  An `drug` data.table. See
  [`drug_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md)

- mp_data:

  An `MP` data.table. See
  [`mp_`](https://pharmacologie-caen.github.io/vigicaen/reference/mp_.md)

- freq_threshold:

  A numeric value indicating the minimum frequency (as a proportion) of
  cases where a drug must appear to be included in the results. Defaults
  to `NULL`.

- top_n:

  An integer specifying the number of most frequently occurring drugs to
  return. Defaults to `NULL`.

## Value

A `data.frame` with the following columns:

- `Drug name`: The drug name.

- `DrecNo`: The drug record number

- `N`: The number of unique reports (cases) where the drug appears.

- `percentage`: The percentage of total unique reports where the drug
  appears.

The results are sorted in descending order of `percentage`.

## Details

- If `freq_threshold` is set (e.g., `0.05`), the function filters drugs
  appearing in at least 5% of unique reports in `.data`.

- If `top_n` is specified, only the most frequent `n` drugs are
  returned. If both `freq_threshold` and `top_n` are provided, only
  `top_n` is applied (a warning is raised in such cases).

- Counts are computed at the *case* level, not the drug mention level.
  This means frequencies reflect the proportion of unique reports
  (cases) where a drug is mentioned, rather than the total mentions
  across all reports.

## Examples

``` r
# Set up start
data.table::setDTthreads(1)

# Filter drugs appearing in at least 10% of reports
screen_drug(
  .data = drug_,
  mp_data = mp_,
  freq_threshold = 0.10
)
#> # A tibble: 3 × 4
#>   `Drug name`      DrecNo     N percentage
#>   <chr>             <dbl> <int>      <dbl>
#> 1 pembrolizumab  20116296   298       39.7
#> 2 nivolumab     111841511   225       30  
#> 3 ipilimumab    133138448    86       11.5

# Get the top 5 most reported drugs
screen_drug(
  .data = drug_,
  mp_data = mp_,
  top_n = 5
)
#> # A tibble: 5 × 4
#>   `Drug name`      DrecNo     N percentage
#>   <chr>             <dbl> <int>      <dbl>
#> 1 pembrolizumab  20116296   298      39.7 
#> 2 nivolumab     111841511   225      30   
#> 3 ipilimumab    133138448    86      11.5 
#> 4 atezolizumab  112765189    69       9.2 
#> 5 durvalumab    125456180    68       9.07

# nb: in the example datasets, not all drugs are recorded in mp_,
# leading to NAs in screen_drug output.

# Set up end
data.table::setDTthreads(0)
```
