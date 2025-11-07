# Data of immune checkpoint inhibitors.

Demo, drug, adr, link, ind, out, srce, and followup are the main table
in Vigibase Extract Case Level data. In a regular workflow, you will
work with those tables as R objects (e.g. `demo`, `drug`, `adr`, `link`,
`ind`, `out`, `srce`, `followup`). These built-in example datasets use
an underscore "\_" to avoid ambiguity with your own tables (e.g.
`demo_`, `drug_`, `adr_`, `link_`, `ind_`, `out_`, `srce_`,
`followup_`). This is a relational database, which means every table has
a primary key variable (e.g., `UMCReportId` for `demo_`. Keys will allow
joints with other tables The full details on the original structure can
be found in "VigiBase Extract Case Level - file description.pdf" in your
VigiBase ECL folders. `demo_` will typically be your cornerstone table,
since it contains one row per report. It is the preferred table to
update for drugs and adrs identification before performing
disproportionality analyses. These tables are subsets of the original
ones, with some of the immune checkpoint inhibitor cases or
immune-related adverse events. All data shown in these example data are
**FAKE**, which means you shouldn't consider the counts and computations
as accurate. Immune checkpoint inhibitors drugs include "Ipilimumab",
"Atezolizumab", "Durvalumab", "Nivolumab", "Pembrolizumab", "Avelumab",
"Cemiplimab","REGN 2810", "Tremelimumab". More details on how to use
vigibase tables can be found in the vignettes.
[`vignette("basic_workflow")`](https://pharmacologie-caen.github.io/vigicaen/articles/basic_workflow.md),
[`vignette("descriptive")`](https://pharmacologie-caen.github.io/vigicaen/articles/descriptive.md).
To build your own tables, use
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md).
See
[`vignette("getting_started")`](https://pharmacologie-caen.github.io/vigicaen/articles/getting_started.md).

## Usage

``` r
data(demo_)

drug_

adr_

link_

followup_

ind_

out_

srce_
```

## Format

`demo_` is a data.table with 7 variables and 750 rows.

- `UMCReportId` Integer. The unique identifier of the case report.

- `AgeGroup` Character. The age group of the patient. Correspondence
  table is `path_sub/AgeGroup.parquet`.

- `Gender` Character. Case gender. `path_sub/Gender.parquet`

- `DateDatabase` Character (not date or numeric!). The date of the
  latest update of the report in the database.

- `Type` Character. The type of report. `path_sub/ReportType.parquet`

- `Region` Character. The world region where the report comes from
  `path_sub/Region.parquet`.

- `FirstDateDatabase` Character. The date the report was first submitted
  to the database.

`drug_` is a data.table with 10 variables and 3514 rows.

- `UMCReportId` Integer. See `demo_`.

- `Drug_Id` Integer. The unique identifier of each drug report.

- `MedicinalProd_Id` Integer. The medicinalproduct identifier. See
  [`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md).

- `DrecNo` Integer. Drug Record Number, pivotal to identify drugs with
  [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md).

- `Seq1`, `Seq2` Character. Seq 1 and 2 complement DrecNo, in WHODrug
  dictionary.

- `Route` Character. The route of administration of the drug.

- `Basis` Character. The reputation basis of the drug (suspect,
  concomitant, or interacting). `path_sub/RepBasis.parquet`

- `Amount` Character. The amount of drug administered.

- `AmountU` Character. The unit of the amount of drug administered.
  `path_sub/SizeUnit.parquet`

- `Frequency` Character. The frequency of drug administration.

- `FrequencyU` Character. The unit of the frequency of drug
  administration. `path_sub/Frequency.parquet`

`adr_` is a data.table with 4 variables and 2133 rows.

- `UMCReportId` Integer. See `demo_`.

- `Adr_Id` Integer. The unique identifier of each adverse event report.

- `MedDRA_Id` Integer. The MedDRA identifier of the adverse event. It is
  used in
  [`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
  and
  [`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md).

- `Outcome` Character. The outcome of the adverse event.
  `path_sub/Outcome.parquet`

`link_` is a data.table with 3 variables and 3514 rows. The version
built with
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md)
is slightly different than the original one.

- `Drug_Id` and `Adr_Id` . Integers. Together, they are the key variable
  of `link`. See `drug_` and `adr_`.

- `Dechallenge1` and `2` Characters. Dechallenge action and outcome.
  `path_sub/Dechallenge.parquet`, `path_sub/Dechallenge2.parquet`

- `Rechallenge1` and `2` Characters. Rechallenge action and outcome.
  `path_sub/Rechallenge.parquet`, `path_sub/Rechallenge2.parquet`

- `TimeToOnsetMin` and `Max` Numerics. The minimum and maximum time to
  onset of the adverse event.

- `tto_mean` Numeric. The mean time to onset of the adverse event. It is
  the average of `TimeToOnsetMin` and `Max`.

- `range` Numeric. The incertitude around `tto_mean`. See
  [`vignette("descriptive")`](https://pharmacologie-caen.github.io/vigicaen/articles/descriptive.md).

- `UMCReportId` Integer. See `demo_`.

`ind_` is a data.table with 2 variables and 2426 rows.

- `Drug_Id` Integer. See `drug_`.

- `Indication` Character. The indication of the drug.

`out_` is a data.table with 3 variables and 747 rows.

- `UMCReportId` Integer. See `demo_`.

- `Seriousness` Character. The seriousness criteria of the report.
  `path_sub/Seriousness.parquet`

- `Serious` Character. Whether the case is serious or not ("N" No, "Y"
  Yes)

`srce_` is a data.table with 2 variables and 729 rows.

- `UMCReportId` Integer. See `demo_`.

- `Type` Character. The Type of Reporter. `path_sub/Notifier.parquet`

`followup_` is a data.table with 2 variables and 902 rows.

- `UMCReportId` Integer. See `demo_`.

- `ReplacedUMCReportId` Integer. Previous version of the case, which is
  no longer available in `demo_`.

An object of class `data.table` (inherits from `data.frame`) with 3514
rows and 12 columns.

An object of class `data.table` (inherits from `data.frame`) with 2133
rows and 4 columns.

An object of class `data.table` (inherits from `data.frame`) with 5136
rows and 11 columns.

An object of class `data.table` (inherits from `data.frame`) with 902
rows and 2 columns.

An object of class `data.table` (inherits from `data.frame`) with 2426
rows and 2 columns.

An object of class `data.table` (inherits from `data.frame`) with 747
rows and 3 columns.

An object of class `data.table` (inherits from `data.frame`) with 729
rows and 2 columns.

## Source

None

## References

There is none

## Examples

``` r
data(demo_)
demo_ |> dplyr::count(AgeGroup)
#>    AgeGroup     n
#>      <char> <int>
#> 1:        2     1
#> 2:        5    43
#> 3:        6   173
#> 4:        7   174
#> 5:        8   108
#> 6:        9   251
```
