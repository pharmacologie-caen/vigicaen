# Get ATC codes (DrecNos or MPIs)

**\[stable\]** Collect Drug Record Numbers or MedicinalProd_Ids
associated to one or more ATC classes.

## Usage

``` r
get_atc_code(atc_sel, mp, thg_data, vigilyze = TRUE)
```

## Arguments

- atc_sel:

  A named list of ATC codes. See Details.

- mp:

  A modified MP data.table. See
  [`mp_`](https://pharmacologie-caen.github.io/vigicaen/reference/mp_.md)

- thg_data:

  A data.table. Correspondence between ATC codes and MedicinalProd_Id
  (usually, it is `thg`)

- vigilyze:

  A logical. Should ATC classes be retrieved using the vigilyze style?
  See details

## Value

A named list of integers. **DrecNos** if `vigilyze` is set to `TRUE`, or
**MedicinalProd_Ids** if `vigilyze` is set to `FALSE`.

## Details

`get_atc_code()` is an *ID collector* function. Provide `atc_sel` in the
same way as `d_sel` in
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
but remember to specify its method arg as `MedicinalProd_Id` if
`vigilyze` is set to `FALSE`. Vigilyze style means all conditioning of
drugs will be retrieved after requesting an ATC class (i.e., drugs are
identified with their DrecNos), even if a specific conditioning is not
present in the ATC class. This is the default behavior in vigilyze.

## See also

[`mp_`](https://pharmacologie-caen.github.io/vigicaen/reference/mp_.md),
[`thg_`](https://pharmacologie-caen.github.io/vigicaen/reference/mp_.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)

## Examples

``` r
# ## Find codes associated with one or more atc classes

# First, define which atc you want to use

atc_sel <-
     rlang::list2(penicillins_gcsf = c("L03AA", "J01CA"),
                  ace_inhibitors = c("C09AA")
     )

# You can get DrecNos for you ATCs (if vigilyze is TRUE)

atc_drecno <-
  get_atc_code(atc_sel = atc_sel,
               mp = mp_,
               thg_data = thg_,
               vigilyze = TRUE)
#> ℹ vigilyze set to TRUE, extracting DrecNos (?get_atc_code for details)

# Or you can get MedicinalProd_Ids (if vigilyze is FALSE)

atc_mpi <-
  get_atc_code(atc_sel = atc_sel,
               mp = mp_,
               thg_data = thg_,
               vigilyze = FALSE)
#> ℹ vigilyze set to FALSE, extracting MedicinalProd_ids (?get_atc_code for details)
```
