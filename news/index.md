# Changelog

## vigicaen 1.0.0

CRAN release: 2026-02-06

### Time for 1.0!

- We are so happy to deliver the v1.0 of vigicaen! The package has now
  reached a sufficient number of capabilities with a consistent enough
  internal structure that we’ve thought it would deserve moving to the
  first major release. The general architecture is unlikely to change in
  the near future, whereas there is always room for performance
  improvement and additional functions. The latter should come as minor
  releases in the foreseeable future. We hope you will enjoy using the
  package and look forward for your feedbacks. Sincerely, the vigicaen
  team.

### New features

- *New*
  [`add_ind()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_ind.md)
  complements the `add_*` family, to add indication columns to a demo,
  drug, adr, link or ind table. It’s process is slightly simpler than
  the other add functions, as their is no `get_` step at the moment.

- *New*
  [`add_dose()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_dose.md)
  complements the `add_*` family, to add drug dose in mg per day columns
  to a demo, drug, adr, link or ind table. It calculates daily dose
  values based on dose amount, frequency, and their corresponding
  units.([\#104](https://github.com/pharmacologie-caen/vigicaen/issues/104))

- [`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
  now supports `ind` tables as .data argument.

- [`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md)
  now supports arrow style tables.

- An error message is raised if trying to pass an `ind` table to
  [`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)
  as .data argument.

- `dt_fst()` is definitively removed.

### Enhancements for low specification computers

- [`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
  is internally optimized to minimize memory use, making it more likely
  to success on low spec computers \<16GB RAM
  ([\#158](https://github.com/pharmacologie-caen/vigicaen/issues/158))

- [`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)
  and now internally calls
  [`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
  rather than
  [`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html)
  if arg `in_memory = FALSE`, to increase chances of success on low spec
  computers \<16GB RAM
  ([\#158](https://github.com/pharmacologie-caen/vigicaen/issues/158))

- [`tb_subset()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_subset.md)
  now internally calls
  [`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
  rather than
  [`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html)
  for the same reason.

## vigicaen 0.16.1

CRAN release: 2025-07-25

### New features

- [`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md)
  now accepts a new argument `rm_suspdup` (default TRUE) to
  automatically remove cases identified as suspected duplicates (from
  `SUSPECTEDDUPLICATES.txt`) when creating the main tables. You can
  disable this behavior by setting `rm_suspdup = FALSE`.

- [`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md)
  now supports a new argument `overwrite_existing_tables` (default
  FALSE). If set to FALSE, the function will skip the creation of any
  .parquet tables that already exist, allowing you to resume the process
  after a crash or interruption without rebuilding completed tables. Set
  to TRUE to force rebuilding all tables. This makes the table-building
  process more robust and efficient for large datasets or limited
  hardware.

- [`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
  now supports two new arguments:

  - `suspect_only` (default FALSE): controls whether only suspected drug
    cases are included in IC analysis.
  - `d_code_2` (optional): if provided, a single analysis is performed
    on cases exposed to both drugs in `d_code` and `d_code_2`. A CLI
    message is displayed when dual drug analysis is performed.

### Bug fixes

- [`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
  now has left/right justification of the patient label centered on 3
  months (90 days) instead of the observed median. This resolves an
  issue where the label could be misplaced on the time to onset plot
  when the median did not match the visual center of the
  graph.([\#145](https://github.com/pharmacologie-caen/vigicaen/issues/145))

### Documenting

- The ‘smq’ argument in
  [`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)
  no longer requires length
  1.([\#126](https://github.com/pharmacologie-caen/vigicaen/issues/126))

- The example for
  [`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)
  now uses explicit names for
  atc_sel.([\#126](https://github.com/pharmacologie-caen/vigicaen/issues/126))

- The Value section of
  [`desc_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_tto.md)
  now clarifies the default descriptive format and how to change
  it.([\#126](https://github.com/pharmacologie-caen/vigicaen/issues/126))

- Typos in routine pharmacovigilance
  vignette.([\#126](https://github.com/pharmacologie-caen/vigicaen/issues/126))

## vigicaen 0.15.6

CRAN release: 2025-03-13

### New features

- [`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
  now plots details on drug liability and rechallenge when available
  ([\#119](https://github.com/pharmacologie-caen/vigicaen/issues/119)).

- [`desc_rch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_rch.md),
  [`desc_dch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_dch.md)
  and
  [`desc_outcome()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_outcome.md)
  now handle out of memory arrow Table
  ([\#127](https://github.com/pharmacologie-caen/vigicaen/issues/127)).

- [`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md)
  now handles factors as `x` or `y` arguments, if levels are strictly
  equal to 0 and 1
  ([\#134](https://github.com/pharmacologie-caen/vigicaen/issues/134)).

### Minor

- Error and warnings all turned into `cli` syntax. Gathered redundant
  checkers internally
  ([\#133](https://github.com/pharmacologie-caen/vigicaen/issues/133)).

- [`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
  now works with out of memory arrow Tables. Additional checkers to
  [`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
  for input data type.

- [`compute_or_mod()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_or_mod.md)
  now has an informative error message if either args `estimate` or
  `std_er` were not supplied
  ([\#123](https://github.com/pharmacologie-caen/vigicaen/issues/123)).

- [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
  now handles non-WHO names matching a unique DrecNo that has several
  WHO names, and throws an error if matching multiple DrecNos
  ([\#139](https://github.com/pharmacologie-caen/vigicaen/issues/139)).

- Adding dependency to `glue`.

## vigicaen 0.15.5

CRAN release: 2025-02-21

### Minor

- Working on examples per CRAN query
  ([\#128](https://github.com/pharmacologie-caen/vigicaen/issues/128)).

- `create_ex_*` family mostly aimed at example set up or internal use
  ([\#128](https://github.com/pharmacologie-caen/vigicaen/issues/128)).

## vigicaen 0.15.4

### Minor

- Fixing typos in DESCRIPTION, per CRAN query.

## vigicaen 0.15.3

### Minor

- Addressing CRAN queries on package DESCRIPTION.

- Condensing examples and
  [`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md)
  default output.

## vigicaen 0.15.2

### Minor

- Clearing remaining temp files from tests

## vigicaen 0.15.1

### New features

- `tb_*` family now has a `cli` style progress bar

- [`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
  output is reworked with `cli` features. The function also now
  correctly supports non-data.table data.frame.

- [`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
  fails if there are no drug or adr cases found in `demo_data`. \#
  vigicaen 0.15.0

### Breaking changes

- `mp` replaces `mp_short` throughout the package
  ([\#110](https://github.com/pharmacologie-caen/vigicaen/issues/110)).
  Users will have to change existing code to replace calls to `mp_short`
  with `mp`.

  ``` r
  # the old way
  mp_short <- dt_parquet(path_who, "mp_short")
  d_names <- list(drug1 = "paracetamol")
  d_drecno <- get_drecno(d_names, mp_short = mp_short)

  # the new way
  mp <- dt_parquet(path_who, "mp")
  d_names <- list(drug1 = "paracetamol")
  d_drecno <- get_drecno(d_names, mp = mp)
  ```

- In
  [`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md),
  `smq_list` and `smq_content` replace
  `smq_list_content`([\#74](https://github.com/pharmacologie-caen/vigicaen/issues/74)).
  Users will have to change existing code to replace calls to
  `smq_list_content` with `smq_list` and `smq_content`. Example tables
  `smq_list_` and `smq_content_` are added to the package. This change
  was made to streamline the process of querying sub-SMQs.

  ``` r
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

- [`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md)
  is the new name of `compute_or_abcd()`, to reflect that other
  disproportionality measures are available.

### New features

- Args `inspect` and `show_all` of
  [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
  are replaced by `verbose`
  ([\#102](https://github.com/pharmacologie-caen/vigicaen/issues/102)).

- [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
  and
  [`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)
  messages are cleaner and written with the `cli` package.

- New `top_n` argument added to
  [`screen_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/screen_adr.md)
  ([\#86](https://github.com/pharmacologie-caen/vigicaen/issues/86)).

- [`screen_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/screen_drug.md)
  let you screen most reported drugs in `drug`
  ([\#103](https://github.com/pharmacologie-caen/vigicaen/issues/103)).

- [`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)
  now queries sub-SMQs and return all relevant codes
  ([\#74](https://github.com/pharmacologie-caen/vigicaen/issues/74)).

- Order of output columns in
  [`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md)
  has changed, to allow easier access to formatted values. Also new
  arguments `min_n_obs` and `export_raw_values`
  ([\#101](https://github.com/pharmacologie-caen/vigicaen/issues/101)).

- [`tb_subset()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_subset.md)
  handles ID code lists as input, instead of numeric vectors, to make
  workflow more consistent with other processes like `add_*`.
  ([\#112](https://github.com/pharmacologie-caen/vigicaen/issues/112))

### Minor and bug Fixes

- [`screen_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/screen_adr.md)
  now counts effects at the case level
  ([\#86](https://github.com/pharmacologie-caen/vigicaen/issues/86)).

- arg `data_type` of `add_*` functions is now deprecated. Data_type is
  internally detected.

- [`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
  and
  [`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)
  have informative error if id code list is incorrect
  ([\#88](https://github.com/pharmacologie-caen/vigicaen/issues/88)).

### Documenting

- Several documenting issues were adressed
  ([\#64](https://github.com/pharmacologie-caen/vigicaen/issues/64),
  [\#77](https://github.com/pharmacologie-caen/vigicaen/issues/77),
  [\#82](https://github.com/pharmacologie-caen/vigicaen/issues/82),
  [\#106](https://github.com/pharmacologie-caen/vigicaen/issues/106),
  [\#108](https://github.com/pharmacologie-caen/vigicaen/issues/108),
  [\#109](https://github.com/pharmacologie-caen/vigicaen/issues/109),
  [\#111](https://github.com/pharmacologie-caen/vigicaen/issues/111))

## vigicaen 0.14.0

### New features

[\#90](https://github.com/pharmacologie-caen/vigicaen/issues/90)
Introducing
[`vigi_routine()`](https://pharmacologie-caen.github.io/vigicaen/reference/vigi_routine.md)
to streamline the process for pharmacovigilance practitioners. This
function is intended for non-expert users, to produce an Information
Component and time to onset graph.

[\#94](https://github.com/pharmacologie-caen/vigicaen/issues/94) The
`luda` table no longer exists. It is replaced by an augmented version of
`link` - built directly from
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
which is used downstream in place of luda.

### Minor

[\#92](https://github.com/pharmacologie-caen/vigicaen/issues/92)
`tb_main()` and `tb_sub()` are fusionned into
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md).

[\#93](https://github.com/pharmacologie-caen/vigicaen/issues/93) It is
now possible to export raw data composing the `value` column of
[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md)
and
[`desc_cont()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_cont.md),
with argument `export_raw_values`.

[\#83](https://github.com/pharmacologie-caen/vigicaen/issues/83)
[`vignette("descriptive")`](https://pharmacologie-caen.github.io/vigicaen/articles/descriptive.md)
was translated in English.

### Bug fixes

[\#89](https://github.com/pharmacologie-caen/vigicaen/issues/89) and
[\#87](https://github.com/pharmacologie-caen/vigicaen/issues/87) the
`get_*` family of functions now correctly handle out of memory tables
(by pulling them into memory). The same is true for
[`extract_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/extract_tto.md).

[\#85](https://github.com/pharmacologie-caen/vigicaen/issues/85) Useless
default values to some arguments of
[`extract_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/extract_tto.md)
were removed.

## vigicaen 0.13.5

### Patch

[\#91](https://github.com/pharmacologie-caen/vigicaen/issues/91) tb\_\*
family correctly handles different ways of providing paths, and check
for the existence of the files **before** working on tables.

## vigicaen 0.13.4

### Patch

[\#78](https://github.com/pharmacologie-caen/vigicaen/issues/78)
compute_or_abcd() now correctly handles large amount of reports.

[\#80](https://github.com/pharmacologie-caen/vigicaen/issues/80) hot fix
on windows 1224 error

## vigicaen 0.13.3

### Patch

[\#75](https://github.com/pharmacologie-caen/vigicaen/issues/75) clearer
example for
[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)

[\#76](https://github.com/pharmacologie-caen/vigicaen/issues/76)
SizeUnit_Lx file in sub, which is encoded in an old format, is now
appropriately treated as such.

## vigicaen 0.13.2

### Patch

Failed ubuntu latest oldrel-1 for licensing issue. Put license in
license file.

## vigicaen 0.13.1

### Patch

Fixing Windows 1224, by using different temp sub folder for each test

## vigicaen 0.13.0

### New features

Changing the package’s name!! All set for a public release.

Improved documentation by adding tags, translating to english the
basic_workflow vignette.
[\#62](https://github.com/pharmacologie-caen/vigicaen/issues/62),
[\#67](https://github.com/pharmacologie-caen/vigicaen/issues/67), and
[\#69](https://github.com/pharmacologie-caen/vigicaen/issues/69)

### Patchs

Increased code coverage back to 100%.
