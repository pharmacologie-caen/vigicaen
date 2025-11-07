# Example source tables for VigiBase and MedDRA

**\[experimental\]** Write some example tables as source
text/ascii/parquet files.

## Usage

``` r
create_ex_main_txt(path)

create_ex_sub_txt(path)

create_ex_who_txt(path)

create_ex_meddra_asc(path)

create_ex_main_pq(path)

create_ex_sub_pq(path)
```

## Arguments

- path:

  Character string. A folder on your computer where the tables should be
  written.

## Value

A set of text/ascii files, as received by the Uppsala Monitoring Centre
or MedDRA

- For `create_ex_main_txt()`, DEMO.txt, DRUG.txt, LINK.txt,
  FOLLOWUP.txt, ADR.txt, OUT.txt, SRCE.txt, and IND.txt

- For `create_ex_sub_txt()`, AgeGroup_Lx.txt, Dechallenge_Lx.txt,
  Dechallenge2_Lx.txt, Frequency_Lx.txt, Gender_Lx.txt, Notifier_Lx.txt,
  Outcome_Lx.txt, Rechallenge_Lx.txt, Rechallenge2_Lx.txt,
  Region_Lx.txt, RepBasis_Lx.txt, ReportType_Lx.txt, RouteOfAdm_Lx.txt,
  Seriousness_Lx.txt, and SizeUnit_Lx.txt

- For `create_ex_who_txt()`, ATC.txt, CCODE.txt, ING.txt, MP.txt,
  ORG.txt, PF.txt, PP.txt, PRT.txt, PRG.txt, SRCE.txt, STR.txt, SUN.txt,
  ThG.txt, and Unit-X.txt

- For `create_ex_meddra_asc()`, llt.asc, mdhier.asc, smq_content.asc,
  smq_list.asc

- For `create_ex_main_pq()`, demo.parquet, adr.parquet, drug.parquet,
  link.parquet, srce.parquet, ind.parquet, out.parquet,
  followup.parquet, suspdup.parquet

- For `create_ex_sub_pq()`, agegroup.parquet, dechallenge.parquet,
  dechallenge2.parquet, frequency.parquet, gender.parquet,
  notifier.parquet, outcome.parquet, rechallenge.parquet,
  rechallenge2.parquet, region.parquet, repbasis.parquet,
  reporttype.parquet, routeofadm.parquet, seriousness.parquet, and
  sizeunit.parquet

## Details

VigiBase tables and MedDRA tables are provided respectively as text
files and ascii files. The `tb_*` family turns them into parquet files.
These `create_example_*` functions are only used to produce example
source files to illustrate the `tb_*` family, and parquet files for the
same purpose.

## Functions

- `create_ex_sub_txt()`: sub txt tables

- `create_ex_who_txt()`: WHO txt tables

- `create_ex_meddra_asc()`: MedDRA txt tables

- `create_ex_main_pq()`: main parquet tables

- `create_ex_sub_pq()`: subsidiary parquet tables

## See also

[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
[`tb_who()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_who.md),
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md)

## Examples

``` r
path <- paste0(tempdir(), "/crex/")

dir.create(path)

# You may want to use different paths for each type of tables
create_ex_main_txt(path)

create_ex_sub_txt(path)

create_ex_who_txt(path)

create_ex_meddra_asc(path)

create_ex_main_pq(path)

create_ex_sub_pq(path)

# Remove temporary folders when you're done
unlink(path, recursive = TRUE)
```
