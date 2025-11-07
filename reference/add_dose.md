# Add drug dose column(s) to a dataset, in milligram per day

**\[experimental\]** `add_dose()` creates drug dose columns in vigibase
datasets (demo, link, adr, drug, ind) for specified drugs in a dataset.
It calculates daily dose values based on dose amount, frequency, and
their corresponding units. The function is compatible with `demo`,
`link`, `adr`, `drug` and `ind` datasets.

## Usage

``` r
add_dose(
  .data,
  d_code,
  d_dose_names = names(d_code),
  repbasis = "sci",
  method = c("DrecNo", "MedicinalProd_Id"),
  drug_data,
  verbose = TRUE
)
```

## Arguments

- .data:

  The dataset used to identify individual reports (usually, it is
  `demo`)

- d_code:

  A named list of drug codes (DrecNos or MPI). See Details.

- d_dose_names:

  A character vector. Names for drug dose columns (must be the same
  length as d_code), default to `names(d_code)`. Will be followed by a
  fixed suffix "\_dose_mg_per_day".

- repbasis:

  Suspect, interacting and/or concomitant. Type initial of those you
  wish to select ("s" for suspect, "c" for concomitant and "i" for
  interacting ; default to all, e.g. "sci").

- method:

  A character string. The type of drug code (DrecNo or
  MedicinalProd_Id). See details.

- drug_data:

  A data.frame containing the drug data (usually, it is `drug`)

- verbose:

  Logical, whether to display messages about added doses.

## Details

Actual supported dosage regimens are any combination of:

- Kilograms, grams, milligrams, micrograms, nanograms, or picograms

- Per minute, hour, day, week, month, or year.

Note that the result will be expressed in **milligrams per day**,
whatever the aforementioned combination is. This may lead to very small
or very large amounts in `drug_dose_mg_per_day` columns, depending on
the actual dosage regimen. The function identifies drug doses in a
dataset by cross-referencing with a `drug_data` table. If either the
amount unit (grams, etc.) *or* the frequency (days, etc.) is missing in
`drug_data`, the corresponding row will be omitted. Drugs may be
filtered based on reputation bases (suspect, concomitant, or
interacting). Either drug record numbers (e.g., from
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)),
or medicinalprod_ids (e.g., from
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md))
can be used to identify drugs. Default method is to DrecNos.

**It is very important to check the results**, as coding issues are
common for dose data, and some results may seem unreliable.

## See also

[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md),
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)

## Examples

``` r
# Example: Adding doses for paracetamol
d_code <- list(paracetamol = c(97818920, 97409107))
demo <-
  add_dose(
    .data = demo_,
    d_code = d_code,
    d_dose_names = "paracetamol",
    drug_data = drug_
    )
#> ℹ `.data` detected as `demo` table.
#> 
#> ── ✔ Drug dose found in mg/day 
#> 
#> • `paracetamol`: 4 rows
#> ℹ Important: Check dose results,
#> coding issues are common for drug dose.
#> Some values may seem unreliable.
#> 
#> ℹ Dose summary (mg/day) - median (Q1-Q3) [min-max]
#> • `paracetamol`: 512.0 (24.0-1,250.0) [24.0-2,000.0]
#> 

desc_cont(demo, "paracetamol_dose_mg_per_day")
#>                           var level                               value n_avail
#> 1 paracetamol_dose_mg_per_day  <NA> 512.0 (24.0-1,250.0) [24.0-2,000.0]       4

# Use only drug dose where paracetamol had a "suspect" reputation base.
demo <-
  add_dose(
    .data = demo_,
    d_code = d_code,
    d_dose_names = "para_susp",
    repbasis = "s",
    drug_data = drug_
  )
#> ℹ `.data` detected as `demo` table.
#> 
#> ── ✔ Drug dose found in mg/day 
#> 
#> • `para_susp`: 1 rows
#> ℹ Important: Check dose results,
#> coding issues are common for drug dose.
#> Some values may seem unreliable.
#> 
#> ℹ Dose summary (mg/day) - median (Q1-Q3) [min-max]
#> • `para_susp`: 2,000.0 (2,000.0-2,000.0) [2,000.0-2,000.0]
#> 

desc_cont(demo, "para_susp_dose_mg_per_day")
#>                         var level                                       value
#> 1 para_susp_dose_mg_per_day  <NA> 2,000.0 (2,000.0-2,000.0) [2,000.0-2,000.0]
#>   n_avail
#> 1       1
```
