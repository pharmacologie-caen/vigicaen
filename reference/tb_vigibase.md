# Create VigiBase ECL tables

**\[stable\]** Transform VigiBase .txt files to .parquet files.

## Usage

``` r
tb_vigibase(
  path_base,
  path_sub,
  force = FALSE,
  rm_suspdup = TRUE,
  overwrite_existing_tables = FALSE
)
```

## Arguments

- path_base:

  Character string, a directory containing vigibase txt tables. It is
  also the output directory.

- path_sub:

  Character string, a directory containing subsidiary tables.

- force:

  Logical, to be passed to
  [`cli::cli_progress_update()`](https://cli.r-lib.org/reference/cli_progress_bar.html).
  Used for internal purposes.

- rm_suspdup:

  Logical, should suspected duplicates (from SUSPECTEDDUPLICATES.txt) be
  removed from main tables? Default is TRUE. Set to FALSE to keep all
  cases, including suspected duplicates.

- overwrite_existing_tables:

  Logical, should existing parquet tables be overwritten? Default is
  FALSE.

## Value

- .parquet files of all main tables into the `path_base` directory:
  demo, adr, drug, link, ind, out, srce, followup, and the suspdup
  (suspected duplicates) table. Check
  [`?demo_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md)
  for more information on the tables.

- The link table is augmented with `tto_mean` and `range`, to analyze
  time to onset according to WHo's recommendations (see
  [`vignette("descriptive")`](https://pharmacologie-caen.github.io/vigicaen/articles/descriptive.md).

- .parquet files of all other subsidiary tables into the `path_sub`
  directory: AgeGroup, Dechallenge, Dechallenge2, Frequency, Gender,
  Notifier, Outcome, Rechallenge, Rechallenge2, Region, RepBasis,
  ReportType, RouteOfAdm, Seriousness, and SizeUnit.

.parquet files into the `path_base` directory (**including suspected
duplicates tables**). Some columns are returned as `integer`
(UMCReportId, Drug_Id, MedicinalProd_Id, Adr_Id, MedDRA_Id), and some
columns as `numeric` (TimeToOnsetMin, TimeToOnsetMax) All other columns
are `character`.

## Details

Vigibase Extract Case Level is delivered as zipped text files, that you
should transform to a more efficient format. Parquet format from `arrow`
has many advantages: It works with out-of-memory data, which makes it
possible to process Vigibase tables on a computer with not-so-much RAM.
It is also lightweighted and standard across different langages. The
function also creates variables in each table. The `suspectedduplicates`
table will be added to the base directory. Use
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)
to load the tables afterward.

The argument `overwrite_existing_tables` is especially useful if the
function crashes or is interrupted: it allows you to resume the process
without rebuilding tables that were already successfully created. If set
to FALSE (the default), the function will skip the construction of any
.parquet tables that already exist, so you do not have to start from
scratch after a failure. Set to TRUE to force rebuilding all tables.

## See also

[`tb_who()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_who.md),
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md),
[`tb_subset()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_subset.md),
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)

## Examples

``` r
if (FALSE) { # interactive()

# --- Set up example source files ---- ####

path_base <- paste0(tempdir(), "/", "main", "/")

path_sub  <- paste0(tempdir(), "/", "sub",  "/")

dir.create(path_base)
dir.create(path_sub)

create_ex_main_txt(path_base)
create_ex_sub_txt(path_sub)

 # ---- Running tb_vigibase

 tb_vigibase(path_base = path_base,
        path_sub  = path_sub)

 # Clear temporary files when you're done
 unlink(path_base, recursive = TRUE)
 unlink(path_sub, recursive = TRUE)
}
```
