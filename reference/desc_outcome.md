# Outcome descriptive

**\[experimental\]** Compute outcome description over a set of adr and
drugs.

## Usage

``` r
desc_outcome(.data, drug_s = "drug1", adr_s = "adr1")
```

## Arguments

- .data, :

  An `adr` data.table. See
  [`adr_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md)

- drug_s:

  A character vector, the drug column(s)

- adr_s:

  A character vector, the adverse drug reaction column(s).

## Value

A data.table with one row per drug-adr pair.

- `drug_s` and `adr_s`, same as input

- `n_cas`, number of cases for each category

- `out_label`, the worst outcome for this drug-adr pair

## Details

You need an `adr` data.table. Be careful that you cannot directly filter
`adr` data.table on drugs! You first have to add drug columns to `adr`,
with
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md).
The function reports the worst outcome into consideration for a given
case, if many are reported. Outcomes, from best to worst are:

- Recovered/resolved

- Recovering/resolving

- Recovered/resolved with sequelae

- Not recovered/not resolved

- Fatal

- Died- unrelated to reaction

- Died- reaction may be contributory

See
[`vignette("descriptive")`](https://pharmacologie-caen.github.io/vigicaen/articles/descriptive.md)
for more details.

## See also

[`adr_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)

## Examples

``` r
adr_ <-
  adr_ |>
  add_drug(
    d_code = ex_$d_groups_drecno,
    drug_data = drug_
  ) |>
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr_
  )
#> ℹ `.data` detected as `adr` table.
#> ℹ `.data` detected as `adr` table.

desc_outcome(
  adr_,
  drug_s = "pd1",
  adr_s = "a_colitis"
  )
#> # A tibble: 6 × 4
#>   drug_s adr_s     n_cas out_label                       
#>   <chr>  <chr>     <int> <chr>                           
#> 1 pd1    a_colitis    13 Unknown                         
#> 2 pd1    a_colitis    39 Recovered/resolved              
#> 3 pd1    a_colitis    17 Recovering/resolving            
#> 4 pd1    a_colitis     1 Recovered/resolved with sequelae
#> 5 pd1    a_colitis    10 Not recovered/not resolved      
#> 6 pd1    a_colitis     3 Fatal                           

# you can vectorize over multiple adrs and drugs

desc_outcome(
  adr_,
  drug_s = c("pd1", "pdl1"),
  adr_s = c("a_colitis", "a_pneumonitis")
  )
#> # A tibble: 18 × 4
#>    drug_s adr_s         n_cas out_label                       
#>    <chr>  <chr>         <int> <chr>                           
#>  1 pd1    a_colitis        13 Unknown                         
#>  2 pd1    a_colitis        39 Recovered/resolved              
#>  3 pd1    a_colitis        17 Recovering/resolving            
#>  4 pd1    a_colitis         1 Recovered/resolved with sequelae
#>  5 pd1    a_colitis        10 Not recovered/not resolved      
#>  6 pd1    a_colitis         3 Fatal                           
#>  7 pdl1   a_colitis         5 Unknown                         
#>  8 pdl1   a_colitis         7 Recovered/resolved              
#>  9 pdl1   a_colitis         6 Not recovered/not resolved      
#> 10 pd1    a_pneumonitis    18 Unknown                         
#> 11 pd1    a_pneumonitis    33 Recovered/resolved              
#> 12 pd1    a_pneumonitis    16 Recovering/resolving            
#> 13 pd1    a_pneumonitis     2 Recovered/resolved with sequelae
#> 14 pd1    a_pneumonitis    16 Not recovered/not resolved      
#> 15 pd1    a_pneumonitis    12 Fatal                           
#> 16 pdl1   a_pneumonitis     4 Unknown                         
#> 17 pdl1   a_pneumonitis     1 Recovering/resolving            
#> 18 pdl1   a_pneumonitis     1 Fatal                           
```
