# Add drug column(s) to a dataset

**\[stable\]** Creates drug columns. in vigibase datasets (demo, link,
adr, drug, and ind).

## Usage

``` r
add_drug(
  .data,
  d_code,
  d_names = names(d_code),
  repbasis = "sci",
  method = c("DrecNo", "MedicinalProd_Id"),
  drug_data,
  data_type = deprecated()
)
```

## Arguments

- .data:

  The dataset used to identify individual reports (usually, it is
  `demo`)

- d_code:

  A named list of drug codes (DrecNos or MPI). See Details.

- d_names:

  A character vector. Names for drug columns (must be the same length as
  d_code), default to `names(d_code)`

- repbasis:

  Suspect, interacting and/or concomitant. Type initial of those you
  wish to select ("s" for suspect, "c" for concomitant and "i" for
  interacting ; default to all, e.g. "sci").

- method:

  A character string. The type of drug code (DrecNo or
  MedicinalProd_Id). See details.

- drug_data:

  A data.frame containing the drug data (usually, it is `drug`)

- data_type:

  **\[deprecated\]**. Data_type is now detected internally.

## Value

A dataset with the new drug columns. Each element of `d_names` will add
a column with the same name in `.data`. The value can be 0 (the
corresponding drug is absent) or 1 (the drug is present in the case if
`.data` is `demo` or `adr`, or "this row correspond to this drug", if
`.data` is `drug` or `link`).

## Details

`d_code` is a named list containing drug codes. Either drug record
numbers (e.g., from
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)),
or medicinalprod_ids (e.g., from
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)).
Default method is to DrecNos.

## Argument `repbasis`

Drugs can be reported according to one of three reputation bases:

- `s` for suspect

- `c` for concomitant

- `i` for interacting

in the occurrence of the adverse drug reaction. To study only one of
these reputation basis, type only the corresponding letter in
`repbasis`, e.g. "s" for suspects, or "si" for suspect **or**
interacting.

You can add drug identification to a `demo`, `link`, `adr`, `drug` or
`ind` dataset.(if working with `drug`, you must provide `drug` twice, as
`.data` and `drug_data`)

## See also

[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md),
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md),
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)

## Examples

``` r
# create a nivolumab column in demo_

d_sel_names <- list(nivolumab = "nivolumab")

d_drecno <- get_drecno(d_sel_names,
                        mp = mp_)
#> 
#> ── get_drecno() ────────────────────────────────────────────────────────────────
#> 
#> ── `d_sel`: Matching drugs ──
#> 
#> ── ✔ Matched drugs 
#> 
#> → `nivolumab`: "nivolumab" and "ipilimumab;nivolumab"
#> 
#> ℹ Set `verbose` to FALSE to suppress this section.
#> 
#> ────────────────────────────────────────────────────────────────────────────────
demo_ <-
  add_drug(
    .data = demo_,
    d_code = d_drecno,
    method = "DrecNo",
    repbasis = "sci",
    drug_data = drug_
  )
#> ℹ `.data` detected as `demo` table.

# remember to assign the result to your actual demo dataset

# do you want to work only with cases where nivolumab was a "suspected" drug?
# change argument repbasis to "s"

demo_ <-
  add_drug(
    .data = demo_,
    d_code = d_drecno,
    d_names = "nivolumab_suspected",
    method = "DrecNo",
    repbasis = "s",
    drug_data = drug_
  )
#> ℹ `.data` detected as `demo` table.

check_dm(demo_, cols = c("nivolumab", "nivolumab_suspected"))
#>                     [,1]
#> nivolumab            225
#> nivolumab_suspected  214
```
