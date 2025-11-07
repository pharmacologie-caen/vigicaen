# Extract of subset of Vigibase

**\[stable\]** Create a subset of the VigiBase ECL datasets

## Usage

``` r
tb_subset(
  wd_in,
  wd_out,
  subset_var = c("drecno", "medprod_id", "meddra_id", "age"),
  sv_selection,
  rm_suspdup = TRUE
)
```

## Arguments

- wd_in:

  Source directory pathway (character)

- wd_out:

  Output directory pathway (character)

- subset_var:

  One of `"drecno"`, `"medprod_id"`, `"meddra_id"`, `"age"`

- sv_selection:

  A named list or a vector containing the appropriate ids (according to
  the method, see details)

- rm_suspdup:

  A logical. Should suspected duplicates be removed? TRUE by default

## Value

Parquet files in the output directory. All files from a vigibase ECL
main folder are returned subsetted (including suspectedduplicates).

## Details

You must select a subset variable with `subset_var` and provide an
appropriate list according to this variable in `sv_selection`. Available
`subset_var` :

- `drecno` will use Drug Record Number (DrecNo), from WHO Drug, and will
  subset from `drug` (see
  [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)).

- `medprod_id` will use MedicinalProd_Id, also from `drug`. May be
  useful if requesting from ATC classes. (see
  [`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)).

- `meddra_id` will use MedDRA_Id, subset from `adr`. (see
  [`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
  or See
  [`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)).

- `age` will use AgeGroup from `demo`. See below.

Age groups ids are as follows:

- 1 0 - 27 days

- 2 28 days to 23 months

- 3 2 - 11 years

- 4 12 - 17 years

- 5 18 - 44 years

- 6 45 - 64 years

- 7 65 - 74 years

- 8 \>= 75 years

- 9 Unknown

Example: To work with patients aged 18 to 74, provide `c(5, 6, 7)` as
`sv_selection`.

Use
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)
to load the tables afterward.

## See also

[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md),
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md),
[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md),
[`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md),
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)

## Examples

``` r
if (FALSE) { # interactive()

# --- technical steps ---- #

wd_in <- paste0(tempdir(), "/", "tbsubsetex")
dir.create(wd_in)
create_ex_main_pq(wd_in)

# Select a subset_var and corresponding data

# Subset on adr colitis codes

adr_llt <-
 list(
   colitis = "Colitis"
   ) |>
   get_llt_soc(term_level = "pt", meddra_, verbose = FALSE)

wd_out <- paste0(wd_in, "/", "colitis_subset", "/")

tb_subset(wd_in, wd_out,
          subset_var = "meddra_id",
          sv_selection = adr_llt)

# Subset on drug codes

 d_drecno <-
   list(
    ipi = "ipilimumab") |>
    get_drecno(mp = mp_, verbose = FALSE)

wd_out <- paste0(wd_in, "/", "nivolumab_subset", "/")

tb_subset(wd_in, wd_out,
          subset_var = "drecno",
          sv_selection = d_drecno)

 # Subset on age > 65 year-old

 sv_selection <-
    c(7, 8)

wd_out <- paste0(wd_in, "/", "more_than_65_subset", "/")

tb_subset(wd_in, wd_out,
          subset_var = "age",
          sv_selection = sv_selection)

unlink(wd_in, recursive = TRUE)
}
```
