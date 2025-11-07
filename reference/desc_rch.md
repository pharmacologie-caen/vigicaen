# Rechallenge descriptive

**\[stable\]** Computes counts of rechallenge cases, over a set of adr
and drug pairs.

## Usage

``` r
desc_rch(.data, drug_s = "drug1", adr_s = "adr1")
```

## Arguments

- .data:

  A `link` data.table. See
  [`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md).

- drug_s:

  A character string. The name of the drug column. Drug columns can be
  created with
  [`add_drug`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md).

- adr_s:

  A character string. The name of the adr column. Adr columns can be
  created with
  [`add_adr`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md).

## Value

A data.table with one row per drug-adr pair

- `drug_s` and `adr_s`, same as input.

- Counts of **overall**, **rch**, **inf**, and **rec** cases (see
  details).

## Details

Counts are provided at the **case** level (not the drug-adr pair level).
Description span from number of rechallenge cases to **informative**
rechallenge cases (those cases where the outcome is known). You will
need a `link` data.table, see
[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
on which you have added drugs and adrs with
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
and
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md).
Terminology

- `Overall` as opposed to `rch` for rechallenged (`rch` + `no_rch` =
  `overall`).

- Among `rch`, `inf` (informative) as opposed to `non_inf` (`inf` +
  `non_inf` = `rch`)

- Among `inf`, `rec` (recurring) as opposed to `non_rec` (`rec` +
  `non_rec` = `inf`)

## See also

[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md),
[`desc_dch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_dch.md),
[`desc_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_tto.md)

## Examples

``` r
link_ <-
  link_ |>
  add_drug(
    d_code = ex_$d_groups_drecno,
    drug_data = drug_
  ) |>
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr_
  )
#> ℹ `.data` detected as `link` table.
#> ℹ `.data` detected as `link` table.

desc_rch(.data = link_,
         drug_s = "pd1",
         adr_s = "a_colitis")
#>    drug_s     adr_s n_overall n_rch n_inf n_rec
#>    <char>    <char>     <int> <int> <int> <int>
#> 1:    pd1 a_colitis        81    54    44    16

# You can vectorize over drugs and adrs

desc_rch(.data = link_,
         adr_s = c("a_colitis", "a_pneumonitis"),
         drug_s = c("pd1", "pdl1")
         )
#>    drug_s         adr_s n_overall n_rch n_inf n_rec
#>    <char>        <char>     <int> <int> <int> <int>
#> 1:    pd1     a_colitis        81    54    44    16
#> 2:   pdl1     a_colitis        16     7     0     0
#> 3:    pd1 a_pneumonitis        96    66    55    12
#> 4:   pdl1 a_pneumonitis         6     3     0     0
```
