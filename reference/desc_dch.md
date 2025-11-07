# Dechallenge descriptive

**\[stable\]** Computes positive dechallenge counts over a set of adr
and drug pairs.

## Usage

``` r
desc_dch(.data, drug_s = "drug1", adr_s = "adr1")
```

## Arguments

- .data:

  A `link` data.table.

- drug_s:

  A character vector, the drug column(s)

- adr_s:

  A character vector, the adverse drug reaction column(s).

## Value

A data.table with one row per drug-adr pair.

- `drug_s` and `adr_s`, same as input

- `pos_dch`, number of positive dechallenge cases

## Details

Counts are provided at the **case** level (not the drug-adr pair level).
Positive dechallenge refers to cases where drug was withdrawn or
dose-reduced and reaction abated (in part or in full). You will need a
`link` data.table, see
[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
on which you have added drugs and adrs with
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
and
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md).

## See also

[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md),
[`desc_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_tto.md),
[`desc_rch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_rch.md)

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


desc_dch(link_,
         drug_s = "pd1",
         adr_s = "a_colitis")
#>   drug_s     adr_s pos_dch
#> 1    pd1 a_colitis      46


# you can vectorize over multiple adrs and drugs

desc_dch(link_,
         drug_s = c("pd1", "pdl1"),
         adr_s = c("a_colitis", "a_pneumonitis"))
#>   drug_s         adr_s pos_dch
#> 1    pd1     a_colitis      46
#> 2   pdl1     a_colitis       2
#> 3    pd1 a_pneumonitis      48
#> 4   pdl1 a_pneumonitis       1
```
