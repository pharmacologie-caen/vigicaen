# Get DrecNo from drug names or MedicinalProd_Id

**\[stable\]** Collect Drug Record Numbers associated to one or more
drugs.

## Usage

``` r
get_drecno(
  d_sel,
  mp,
  allow_combination = TRUE,
  method = c("drug_name", "mpi_list"),
  verbose = TRUE,
  show_all = deprecated(),
  inspect = deprecated()
)
```

## Arguments

- d_sel:

  A named list. Selection of drug names or medicinalprod_id. See details

- mp:

  A modified MP data.table. See
  [`mp_`](https://pharmacologie-caen.github.io/vigicaen/reference/mp_.md)

- allow_combination:

  A logical. Should fixed associations including the drug of interest be
  retrieved? See details.

- method:

  Should DrecNo be found from drug names or from MedicinalProd_Id?

- verbose:

  A logical. Allows you to see matching drug names in the console. Turn
  to FALSE once you've checked the matching.

- show_all:

  **\[deprecated\]** Use `verbose` instead.

- inspect:

  **\[deprecated\]** Use `verbose` instead.

## Value

A named list of integers. DrecNos.

## Details

`get_drecno()` is an *ID collector* function. Collected IDs can be used
to create drug columns in datasets like `demo`, `link`, etc. (see
[`vignette("basic_workflow")`](https://pharmacologie-caen.github.io/vigicaen/articles/basic_workflow.md))

## Argument `verbose`

The `verbose` argument is here to let you check the result of
`get_drecno()`. This is an important step in your project setup: You
must ensure that the drugs you are looking for are correctly matched.

## Argument `d_sel`

`d_sel` must be a named list of character vectors. To learn why, see
[`vignette("basic_workflow")`](https://pharmacologie-caen.github.io/vigicaen/articles/basic_workflow.md).
Names of `d_sel` are automatically lowered and trimed.

## Matching drugs

With "drug_name" method, either exact match or perl regex match can be
used. The latter is built upon lookarounds to ensure that a string does
not match to composite drug names including the string, i.e.
`trastuzumab emtasine` for `trastuzumab`, or close names like
`alitretinoin` when looking for `tretinoin`.

Exact match is used for "mpi_list" method.

## Choosing a method

"drug_name" let you work with drug names. It's likely to be the
appropriate method in most of the cases.

"mpi_list" is used when you have a list of MedicinalProd_Ids. A drug can
have multiple MedicinalProd_Ids, corresponding to different packagings.
The MedicinalProd_Id matching is typically used to identify DrecNo(s)
contained in an ATC class (extracted from `thg`), since not all MPI of
drugs are present in `thg` (explanations in
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)).

## WHO names

WHO names are attributed to drugs by... the WHO. A drug only has one WHO
name, but can have multiple international nonproprietary names (e.g.
"tretinoin" and "all-trans retinoic acid").

You should use WHO names to ensure proper identification of drugs and
DrecNos, especially if you work with combinations.

## Argument `allow_combination`

Fixed associations of drugs refers to specialty containing more than one
active ingredient (for example, acetylsalicylic acid and clopidogrel).
In VigiLyze, the default is **NOT** to account for these fixed
associations. For example, when you call "acetylsalicylic acid" in
VigiLyze, you don't have the cases reported with the fixed-association
"acetylsalicylic acid; clopidogrel" **unless the substances were
distinctly coded by the reporter.** Here, the default is to find a drug
even if it is prescribed in a fixed association. Importantly, when
retrieving fixed-association drugs, the non-of-interest drug alone
drecno is not found, hence the cases related to this drug will not be
added to those of the drug of interest.

## See also

[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)

## Examples

``` r
if (FALSE) { # interactive()

# ## Get drecnos for a list a drugs. Check spelling and use WHO name,
# in lowercase

d_sel_names <- list(
  nivolumab = "nivolumab",
  ipilimumab = "ipilimumab",
  nivo_ipi = c("nivolumab", "ipilimumab")
  )

# Read mp with get_drecno(), to identify drugs without combinations

# Take the time to read the matching drugs. Did you forget a drug?

d_drecno <-
  get_drecno(d_sel_names,
             mp = mp_,
             allow_combination = FALSE,
             method = "drug_name")
d_drecno

# And DrecNos of drugs allowing for combinations

d_drecno <-
  get_drecno(d_sel = d_sel_names,
             mp = mp_,
             allow_combination = TRUE,
             method = "drug_name")
d_drecno
}
```
