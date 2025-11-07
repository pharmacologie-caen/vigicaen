# Screening of Adverse Drug Reactions

**\[experimental\]** Identify and rank the most frequently reported
adverse drug reaction (ADR) terms in a dataset, based on a specified
MedDRA term level. It allows users to filter terms by a frequency
threshold or extract the top `n` most frequently occurring terms.

## Arguments

- .data, :

  An `adr` data.table. See
  [`adr_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md)

- meddra:

  A `meddra` data.table. See
  [`meddra_`](https://pharmacologie-caen.github.io/vigicaen/reference/meddra_.md)

- term_level:

  A character string specifying the MedDRA hierarchy level. Must be one
  of `"soc"`, `"hlgt"`, `"hlt"`, `"pt"`, or `"llt"`.

- freq_threshold:

  A numeric value indicating the minimum frequency (as a proportion) of
  cases where a term must appear to be included in the results. For
  example, `0.05` means 5%. Defaults to `NULL`, meaning no threshold is
  applied unless `top_n` is different from `NULL`.

- top_n:

  An integer specifying the number of most frequently occurring terms to
  return. Defaults to `NULL`. Overrides `freq_threshold` if both are
  provided.

## Value

A `data.frame` with the following columns:

- **term**: The MedDRA term at the specified hierarchy level.

- **n**: The number of unique reports (cases) where the term appears.

- **percentage**: The percentage of total unique reports where the term
  appears.

The results are sorted in descending order of `percentage`.

## Details

- If `freq_threshold` is set (e.g., `0.05`), the function filters ADR
  terms appearing in at least 5% of unique reports in `.data`.

- If `top_n` is specified, only the most frequent `n` terms are
  returned. If both `freq_threshold` and `top_n` are provided, only
  `top_n` is applied (a warning is issued in such cases).

- Counts are computed at the *case* level, not the ADR level. This means
  frequencies reflect the proportion of unique reports (cases) where a
  term is mentioned, rather than the total mentions across all reports.

The function processes an ADR dataset (`adr_`) and a MedDRA dataset
(`meddra_`) to generate results that are linked to a specific MedDRA
hierarchy level (`soc`, `hlgt`, `hlt`, `pt`, or `llt`).

## Examples

``` r
# Example 1: Filter terms appearing in at least 5% of reports
screen_adr(
  .data = adr_,
  meddra = meddra_,
  term_level = "pt",
  freq_threshold = 0.05
)
#>           term     n percentage
#>         <char> <int>      <num>
#> 1:        <NA>   678   90.40000
#> 2: Pneumonitis    92   12.26667
#> 3:   Diarrhoea    69    9.20000

# Example 2: Get the top 5 most frequent terms
screen_adr(
  .data = adr_,
  meddra = meddra_,
  term_level = "hlt",
  top_n = 5
)
#>                                                               term     n
#>                                                             <char> <int>
#> 1:                                                            <NA>   678
#> 2: Lower respiratory tract inflammatory and immunologic conditions    92
#> 3:                                      Diarrhoea (excl infective)    69
#> 4:                                        Colitis (excl infective)    40
#> 5:                                  Parenchymal lung disorders NEC    11
#>    percentage
#>         <num>
#> 1:  90.400000
#> 2:  12.266667
#> 3:   9.200000
#> 4:   5.333333
#> 5:   1.466667
```
