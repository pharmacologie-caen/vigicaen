# Add adverse drug reaction column(s) to a dataset

**\[stable\]**Creates adr columns in vigibase datasets (demo, link,
drug, and adr).

## Usage

``` r
add_adr(
  .data,
  a_code,
  a_names = names(a_code),
  adr_data,
  data_type = deprecated()
)
```

## Arguments

- .data:

  The dataset to update (demo, link, drug, adr).

- a_code:

  A named list of low level terms codes (llt_codes).

- a_names:

  A character vector. Names for adr columns (must be the same length as
  adr_list), default to `names(a_code)`

- adr_data:

  A data.frame containing the adr data (usually, it is `adr`)

- data_type:

  **\[deprecated\]**. Data_type is now detected internally.

## Value

A dataset with the new adr columns. Each element of `a_names` will add a
column with the same name in `.data`. The value can be 0 (the
corresponding adr is absent) or 1 (the adr is present in the case if
`.data` is `demo` or `drug`, or "this row correspond to this adr", if
`.data` is `adr` or `link`).

## Details

Low-level term codes are the preferred level of requesting in Vigibase
extract case level since they capture all possible codes for a given
Preferred Term. Collect low-level terms with
[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
and
[`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md).
You can add adr identification to a `demo`, a `link`, `drug` or even an
`adr` dataset (in this latter case, you must provide `adr` twice, as
`.data` and `adr_data`). Column names of these dataset should not have
been modified from the original vigibase dataset (as created with
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md)).

## See also

[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md),
[`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)

## Examples

``` r
# create adr_colitis, adr_embolism and adr_pneumonitis columns in demo

# be careful, this example may overwrite your own demo dataset
demo <- demo_

a_pt_sel <- ex_$pt_sel


adr <- adr_

a_llt <-
  get_llt_soc(
  term_sel = a_pt_sel,
  term_level = "pt",
  meddra = meddra_
  )
#> 
#> ── get_llt_soc() ───────────────────────────────────────────────────────────────
#> 
#> ── ✔ Matched reactions at `pt` level (number of codes) ──
#> 
#> → `colitis`: "Autoimmune colitis (1)", "Colitis (25)", "Colitis microscopic
#>   (3)", "Diarrhoea (53)", "Diarrhoea haemorrhagic (8)", "Duodenitis (5)",
#>   "Enteritis (8)", "Enterocolitis (4)", "Enterocolitis haemorrhagic (10)", and
#>   "Ulcerative gastritis (1)"
#> → `pneumonitis`: "Organising pneumonia (9)" and "Pneumonitis (6)"
#> 
#> ℹ Set `verbose` to FALSE to suppress this section.
#> 

demo <-
  demo |>
    add_adr(
      a_code = a_llt,
      adr_data = adr
    )
#> ℹ `.data` detected as `demo` table.

demo |>
  check_dm(names(a_pt_sel))
#>             [,1]
#> colitis      104
#> pneumonitis  103
```
