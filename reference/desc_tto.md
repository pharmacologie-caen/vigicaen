# Time to onset descriptive

**\[stable\]** `desc_tto()` provides a drug-adr pair description of time
to onset.

## Usage

``` r
desc_tto(.data, adr_s, drug_s, tto_time_range = 1, ...)
```

## Arguments

- .data:

  A `link` data.table. See
  [`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md).

- adr_s:

  A character string. The name of the adr column. (see details)

- drug_s:

  A character string. The name of the drug column. (see details)

- tto_time_range:

  Incertitude range of Time to onset, in days. Defaults to 1 as
  recommended by umc

- ...:

  Additional parameters to be passed to
  [`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md).
  E.g. `format`, `digits`...

## Value

A data.table with one row per drug-adr pair

- A descriptive of time to onsets for this combination (column
  `tto_max`).

- It is Median (Quartile 1 - Quartile 3) and min-max by default, change
  with `format` arg (passed to
  [`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md)).

## Details

Description of time (maximum available time) between drug initiation and
event onset. This runs at the drug-adr pair level. Internally, it uses
[`extract_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/extract_tto.md)
and
[`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md),
You will need a `link` data.table, see
[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
on which you have added drugs and adrs with
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
and
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md).
you can supply extra arguments to
[`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md)
with `...`. Uppsala Monitoring Centre recommends to use only cases where
the incertitude on time to onset is less than **1 day**. You can change
this with `tto_time_range`.

## See also

[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
[`extract_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/extract_tto.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md),
[`desc_dch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_dch.md),
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

desc_tto(.data = link_,
         adr_s = "a_colitis",
         drug_s = "pd1")
#>   drug_s     adr_s     var level                           value n_avail
#> 1    pd1 a_colitis tto_max  <NA> 59.0 (26.5-190.0) [1.0-1,207.0]      39


desc_tto(.data = link_,
         adr_s = c("a_colitis", "a_pneumonitis"),
         drug_s = c("pd1", "ctla4"))
#>   drug_s         adr_s     var level                           value n_avail
#> 1    pd1     a_colitis tto_max  <NA> 59.0 (26.5-190.0) [1.0-1,207.0]      39
#> 2  ctla4     a_colitis tto_max  <NA>    42.0 (15.8-71.8) [5.0-113.0]      10
#> 3    pd1 a_pneumonitis tto_max  <NA> 61.5 (32.8-127.8) [0.0-1,050.0]      40
#> 4  ctla4 a_pneumonitis tto_max  <NA>   71.0 (52.0-94.0) [23.0-181.0]       8
```
