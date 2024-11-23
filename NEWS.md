## Minor

#92 `tb_main()` and `tb_sub()` are fusionned into `tb_vigibase()`.

#93 It is now possible to export raw data composing the `value` column
of `desc_facvar()` and `desc_cont()`, with argument `export_raw_values`.

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

New `build_tables` vignette shows the process.

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

#28 documentation of `desc_*` functions is now available in vignettes descriptive, generic_main and generic_dictionary.

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
