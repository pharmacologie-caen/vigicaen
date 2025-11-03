# vigicaen (development version)

## Enhancements for low specification computers

* *New* `add_ind()` complements the `add_*` family, to add indication columns
to a demo, drug, adr, link or ind table. It's process is slightly simpler
than the other add functions, as their is no `get_` step at the moment.

*  `vigi_routine()` is internally optimized to minimize memory use,
making it more likely to success on low spec computers <16GB RAM (#158)

*  `dt_parquet()` now internally calls 
`arrow::open_dataset()` rather than `arrow::read_parquet()`
if arg `in_memory = FALSE`, to increase chances of success on low spec
computers <16GB RAM (#158)

# vigicaen 0.16.1

## New features

* `tb_vigibase()` now accepts a new argument `rm_suspdup` (default TRUE) 
to automatically remove cases identified as suspected duplicates 
(from `SUSPECTEDDUPLICATES.txt`) when creating the main tables. 
You can disable this behavior by setting `rm_suspdup = FALSE`.

* `tb_vigibase()` now supports a new argument `overwrite_existing_tables` (default FALSE). If set to FALSE, the function will skip the creation of any .parquet tables that already exist, allowing you to resume the process after a crash or interruption without rebuilding completed tables. Set to TRUE to force rebuilding all tables. This makes the table-building process more robust and efficient for large datasets or limited hardware.

* `vigi_routine()` now supports two new arguments:
  - `suspect_only` (default FALSE): controls whether only suspected 
  drug cases are included in IC analysis.
  - `d_code_2` (optional): if provided, a single analysis is performed 
  on cases exposed to both drugs in `d_code` and `d_code_2`.
  A CLI message is displayed when dual drug analysis is performed.

## Bug fixes

* `vigi_routine()` now has left/right justification of the patient label 
centered on 3 months (90 days) instead of the observed median. 
This resolves an issue where the label could be misplaced on the time 
to onset plot when the median did not match the visual center of the graph.(#145)

## Documenting

* The 'smq' argument in `get_llt_smq()` no longer requires length 1.(#126)

* The example for `get_atc_code()` now uses explicit names for atc_sel.(#126)

* The Value section of `desc_tto()` now clarifies the default descriptive format and how to change it.(#126)

* Typos in routine pharmacovigilance vignette.(#126)

# vigicaen 0.15.6

## New features

* `vigi_routine()` now plots details on drug liability and rechallenge
when available (#119).

* `desc_rch()`, `desc_dch()` and `desc_outcome()` now handle out of 
memory arrow Table (#127).

* `compute_dispro()` now handles factors as `x` or `y` arguments,
if levels are strictly equal to 0 and 1 (#134).

## Minor

* Error and warnings all turned into `cli` syntax. Gathered redundant checkers
internally (#133).

* `vigi_routine()` now works with out of memory arrow Tables. Additional
checkers to `vigi_routine()` for input data type.

* `compute_or_mod()` now has an informative error message if either args
`estimate` or `std_er` were not supplied (#123).

* `get_drecno()` now handles non-WHO names matching a unique DrecNo that has
several WHO names, and throws an error if matching multiple DrecNos (#139).

* Adding dependency to `glue`.

# vigicaen 0.15.5

## Minor

* Working on examples per CRAN query (#128).

* `create_ex_*` family mostly aimed at example set up or internal use (#128).

# vigicaen 0.15.4

## Minor

* Fixing typos in DESCRIPTION, per CRAN query.

# vigicaen 0.15.3

## Minor 

* Addressing CRAN queries on package DESCRIPTION.

* Condensing examples and `tb_vigibase()` default output.

# vigicaen 0.15.2

## Minor

* Clearing remaining temp files from tests

# vigicaen 0.15.1

## New features

* `tb_*` family now has a `cli` style progress bar

* `get_llt_soc()` output is reworked with `cli` features.
The function also now correctly supports non-data.table data.frame.

* `vigi_routine()` fails if there are no drug or adr cases found in `demo_data`.

# vigicaen 0.15.0

## Breaking changes

* `mp` replaces `mp_short` throughout the package (#110). 
Users will have to change existing code to replace calls to `mp_short` with `mp`.
 
  ```R
  # the old way
  mp_short <- dt_parquet(path_who, "mp_short")
  d_names <- list(drug1 = "paracetamol")
  d_drecno <- get_drecno(d_names, mp_short = mp_short)
  
  # the new way
  mp <- dt_parquet(path_who, "mp")
  d_names <- list(drug1 = "paracetamol")
  d_drecno <- get_drecno(d_names, mp = mp)
  ```
  
* In `get_llt_smq()`, `smq_list` and `smq_content` replace `smq_list_content`(#74).
Users will have to change existing code to replace calls 
to `smq_list_content` with `smq_list` and `smq_content`.
Example tables `smq_list_` and `smq_content_` are added to the package. This 
change was made to streamline the process of querying sub-SMQs.
  
  ```R
  # the old way
  smq_list_content <- dt_parquet(path_meddra, "smq_list_content")
  
  smq_llt <- 
    get_llt_smq(
      list(ihd = "Ischaemic heart disease (SMQ)"),
      smq_list_content)
  
  # the new way
  smq_list <- dt_parquet(path_meddra, "smq_list")
  smq_content <- dt_parquet(path_meddra, "smq_content")
  
  smq_llt <- 
    get_llt_smq(
       list(ihd = "Ischaemic heart disease (SMQ)"),
       smq_list = smq_list,
       smq_content = smq_content)
  ```
  
* `compute_dispro()` is the new name of `compute_or_abcd()`, to reflect that
other disproportionality measures are available.

## New features  

* Args `inspect` and `show_all` of `get_drecno()` 
are replaced by `verbose` (#102).

* `get_drecno()` and `get_llt_smq()` messages are cleaner and 
written with the `cli` package.

* New `top_n` argument added to `screen_adr()` (#86).

* `screen_drug()` let you screen most reported drugs in `drug` (#103).

* `get_llt_smq()` now queries sub-SMQs and return all relevant codes (#74).

* Order of output columns in `compute_dispro()` has changed, to allow easier
access to formatted values. Also new arguments `min_n_obs` and `export_raw_values`
(#101).

* `tb_subset()` handles ID code lists as input, instead of numeric vectors,
to make workflow more consistent with other processes like `add_*`. (#112)

## Minor and bug Fixes  

* `screen_adr()` now counts effects at the case level (#86).  

* arg `data_type` of `add_*` functions is now deprecated.
Data_type is internally detected.

* `add_drug()` and `add_adr()` have informative error if id code list is
incorrect (#88).

## Documenting

* Several documenting issues were adressed (#64, #77, #82, #106, #108, #109, #111)

# vigicaen 0.14.0

## New features

#90 Introducing `vigi_routine()` to streamline the process for
pharmacovigilance practitioners. This function is intended for 
non-expert users, to produce an Information Component and 
time to onset graph.

#94 The `luda` table no longer exists. It is replaced by an augmented
version of `link` - built directly from `tb_vigibase()`, 
which is used downstream in place of luda.

## Minor

#92 `tb_main()` and `tb_sub()` are fusionned into `tb_vigibase()`.

#93 It is now possible to export raw data composing the `value` column
of `desc_facvar()` and `desc_cont()`, with argument `export_raw_values`.

#83 `vignette("descriptive")` was translated in English.

## Bug fixes

#89 and #87 the `get_*` family of functions now correctly handle out of memory tables
(by pulling them into memory). The same is true for `extract_tto()`.

#85 Useless default values to some arguments of `extract_tto()` were removed. 

# vigicaen 0.13.5

## Patch

#91 tb_* family correctly handles different ways of providing paths,
and check for the existence of the files **before** working on tables.

# vigicaen 0.13.4

## Patch

#78 compute_or_abcd() now correctly handles large amount of reports.

#80 hot fix on windows 1224 error

# vigicaen 0.13.3

## Patch

#75 clearer example for `get_llt_soc()`

#76 SizeUnit_Lx file in sub, which is encoded in an old format, is now appropriately
treated as such. 

# vigicaen 0.13.2

## Patch

Failed ubuntu latest oldrel-1 for licensing issue. Put license in license file.

# vigicaen 0.13.1

## Patch

Fixing Windows 1224, by using different temp sub folder for each test

# vigicaen 0.13.0

## New features

Changing the package's name!! All set for a public release.

Improved documentation by adding tags, translating to english the 
basic_workflow vignette. #62, #67, and #69

## Patchs

Increased code coverage back to 100%.

# pharmacocaen 0.12.0

## New features

#60 add table builders to arrow format (`tb_*` functions). This is supposed to overwrite the
current fst and dt_fst process, which is being deprecated as of this release.

New `getting_started` vignette shows the process.

`dt_parquet` function is to replace `dt_fst` in the workflow.


# pharmacocaen 0.11.1

## Minor

Sounds like minor but... Added tests to hit 100% test coverage for the package!

#56 Had R CMD CHECK pass on 3 different OS! Test coverage also works.

#58 Further anonymized datasets.

get_drecno now correctly inspects DrecNos identified from an mpi_list.

Fix a bug, get_drecno wasn't correctly returning DrecNos from mpi_list.

# pharmacocaen 0.11.0

## New features

#55 add `soc` option to argument `term_level` of `get_llt_soc`.

#15 `compute_or_abcd` is now internally vectorized on drugs and adrs.

#53 `get_atc_code` now appropriately tolowers and trims names as it states,
even when argument `vigilyze` is set to `FALSE`.

#39 added a `d_names` argument to `add_drug`.

Introducing `tb_custom` to pharmacocaen.

## Minor

Improved description of `add_drug` and `add_adr`.

#7 was fixed a long time ago. Internal improvement to `get_llt_smq`

# pharmacocaen 0.10.0

## New features

#54 introduce the `desc_outcome` function to collect adr outcome. Help in the
descriptive vignette.

## Minor

Fix add_adr data_type checker.

# pharmacocaen 0.9.1

Patches:

#47 `get_drecno` now handles correctly non lower-case names in `d_sel`.

#43 improved documentation of `add_drug`

# pharmacocaen 0.9.0

## New features

#50 Experimental `screen_adr` function to identify main adverse drug reactions in an adr dataset.

#28 documentation of `desc_*` functions is now available in vignettes descriptive, template_main and template_dictionary.

## Miscellaneous but very important

Vanquished the `codetools::checkUsagePackage(pack = "pharmacocaen")` note!

## Patchs

#52 fixed incorrect behavior of get_drecno with arg inspect set to TRUE

#51 desc_facvar now shows appropriate big.marks for large numbers

#46 desc_facvar and desc_cont now use a more reliable regex logic to display
results according to `format` argument.

#44 columns names of `desc_dch`, `desc_tto` and `desc_rch` now consistently use `drug_s` and `adr_s` (instead of `drug` and `adr` for desc_rch in the past)

#42 improved get_drecno documentation.

#36 and #40 improved add_drug documentation.

#45 removed argument `tto_time_range` from
`desc_rch` (formerly, the function also
provided time to onset descriptives)
