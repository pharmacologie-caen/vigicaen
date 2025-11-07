# Create main WHO tables

**\[stable\]** Transform Vigibase WHO .txt files to .parquet files

WHODrug is delivered as zipped text files folder, that you should
transform to a more efficient format. Parquet format from arrow has many
advantages: It can work with out-of-memory data, which makes it possible
to process tables on a computer with not-so-much RAM. It is also
lightweighted and standard across different languages. The function also
creates variables in each table. See
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md)
for some running examples, and try
[`?mp_`](https://pharmacologie-caen.github.io/vigicaen/reference/mp_.md)
or
[`?thg_`](https://pharmacologie-caen.github.io/vigicaen/reference/mp_.md)
for more details. Use
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)
to load the tables afterward.

## Usage

``` r
tb_who(path_who, force = FALSE)
```

## Arguments

- path_who:

  Character string, a directory containing whodrug txt tables. It is
  also the output directory.

- force:

  Logical, to be passed to
  [`cli::cli_progress_update()`](https://cli.r-lib.org/reference/cli_progress_bar.html).
  Used for internal purposes.

## Value

.parquet files into the `path_who` directory. Some columns are returned
as `integer` (all Id columns, including MedicinalProd_Id, with notable
exception of DrecNo), and some columns as `numeric` (Quantity from
ingredient table) All other columns are `character`.

## See also

[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md),
[`tb_subset()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_subset.md),
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)

## Examples

``` r
if (FALSE) { # interactive()

# Use the examples from tb_main if you want to see these functions in action.

path_who <- paste0(tempdir(), "/whodrug_directory/")
dir.create(path_who)
create_ex_who_txt(path_who)

tb_who(path_who = path_who)

# Clear temporary files when you're done
unlink(path_who, recursive = TRUE)
}
```
