## New features

#55 add `soc` option to argument `term_level` of `get_llt_soc`.

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
