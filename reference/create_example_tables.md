# Example source tables for VigiBase and MedDRA

**\[stable\]** Write some example tables as source csv/ascii/parquet
files.

## Usage

``` r
create_ex_meddra_asc(path)

create_ex_main_pq(path)

create_ex_sub_pq(path)

create_ex_main_csv(path)

create_ex_who_csv(path)

create_ex_sub_csv(path)
```

## Arguments

- path:

  Character string. A folder on your computer where the tables should be
  written.

## Value

A set of text/ascii files, as received by the Uppsala Monitoring Centre
or MedDRA

- For `create_ex_main_csv()`, DEMO.csv, DRUG.csv, LINK.csv,
  FOLLOWUP.csv, ADR.csv, OUT.csv, SRCE.csv, and IND.csv

- For `create_ex_sub_csv()`, AgeGroup_Lx.csv, Dechallenge_Lx.csv,
  Dechallenge2_Lx.csv, Frequency_Lx.csv, Gender_Lx.csv, Notifier_Lx.csv,
  Outcome_Lx.csv, Rechallenge_Lx.csv, Rechallenge2_Lx.csv,
  Region_Lx.csv, RepBasis_Lx.csv, ReportType_Lx.csv, RouteOfAdm_Lx.csv,
  Seriousness_Lx.csv, SizeUnit_Lx.csv, and SUSPECTEDDUPLICATES.csv

- For `create_ex_who_csv()`, ATC.csv, CCODE.csv, ING.csv, MP.csv,
  ORG.csv, PF.csv, PP.csv, PRT.csv, PRG.csv, SRCE.csv, STR.csv, SUN.csv,
  ThG.csv, and Unit-X.csv

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

VigiBase tables and MedDRA tables are provided respectively as csv files
and ascii files. The `tb_*` family turns them into parquet files. These
`create_example_*` functions are only used to produce example source
files to illustrate the `tb_*` family, and parquet files for the same
purpose. Note that there is a little difference among main and sub
tables, whether created in csv or parquet, as suspected duplicates moves
from sub to main during
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md)
process.

## Functions

- `create_ex_main_pq()`: main parquet tables

- `create_ex_sub_pq()`: subsidiary parquet tables

- `create_ex_main_csv()`: main csv tables

- `create_ex_who_csv()`: who csv tables

- `create_ex_sub_csv()`: sub csv tables

## See also

[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
[`tb_who()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_who.md),
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md)

## Examples

``` r
path <- paste0(tempdir(), "/crex/")

dir.create(path)

# You may want to use different paths for each type of tables

create_ex_meddra_asc(path)

create_ex_main_pq(path)

create_ex_sub_pq(path)

create_ex_main_csv(path)

create_ex_who_csv(path)

create_ex_sub_csv(path)

# Remove temporary folders when you're done
unlink(path, recursive = TRUE)
```
