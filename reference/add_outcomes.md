# Add outcome columns to a dataset

**\[experimental\]** `add_death()`, `add_serious()`, and `add_fup()`
create outcome columns in a vigibase dataset (typically `demo`), using
data from the `out` and `followup` tables. These functions handle both
in-memory and out-of-memory (Arrow) tables.

## Usage

``` r
add_death(.data, out_data, col_name = "death")

add_serious(.data, out_data, col_name = "serious")

add_fup(.data, fup_data, col_name = "fup")
```

## Arguments

- .data:

  The dataset to update (usually `demo`).

- out_data:

  A data.frame containing the outcome data (usually `out`).

- col_name:

  A character string. Name of the new column. Defaults to `"death"`,
  `"serious"`, or `"fup"` respectively.

- fup_data:

  A data.frame containing the follow-up data (usually `followup`).

## Value

A dataset with the new outcome column.

## Details

- `add_death()` adds a column indicating whether the case resulted in
  death (i.e., `Seriousness == "1"` in the `out` table). Cases with no
  outcome data are coded `NA`. Cases with outcome data but no death are
  coded `0`. Cases with death are coded `1`.

- `add_serious()` adds a column indicating whether the case was serious
  (i.e., `Serious == "Y"` in the `out` table). Cases with no outcome
  data are coded `NA`. Cases with outcome data but not serious are coded
  `0`. Serious cases are coded `1`.

- `add_fup()` adds a column indicating whether the case has a follow-up
  (i.e., `UMCReportId` appears in the `followup` table). Cases with a
  follow-up are coded `1`. Others are coded `0`.

## See also

[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)

## Examples

``` r
demo <- demo_
out  <- out_

demo <- add_death(demo, out_data = out)
#> ℹ `.data` detected as `demo` table.
demo <- add_serious(demo, out_data = out)
#> ℹ `.data` detected as `demo` table.

followup <- followup_

demo <- add_fup(demo, fup_data = followup)
#> ℹ `.data` detected as `demo` table.

desc_facvar(demo, c("death", "serious", "fup"))
#> # A tibble: 6 × 4
#>   var     level value         n_avail
#>   <chr>   <chr> <chr>           <int>
#> 1 death   0     612/747 (82%)     747
#> 2 death   1     135/747 (18%)     747
#> 3 serious 0     181/747 (24%)     747
#> 4 serious 1     566/747 (76%)     747
#> 5 fup     0     458/750 (61%)     750
#> 6 fup     1     292/750 (39%)     750
```
