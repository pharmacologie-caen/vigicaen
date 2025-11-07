# Create MedDRA tables

**\[stable\]** Transform MedDRA .ascii files to .parquet files

MedDRA is delivered as ascii files, that you should transform to a more
efficient format. Parquet format from arrow has many advantages: It
works with out-of-memory data, which makes it possible to process tables
on a computer with not-so-much RAM. It is also lightweighted and
standard across different langages. The function also creates variables
in each table. You should note that NOT all MedDRA tables are processed
with this function. Three tables are created: `meddra_hierarchy`, that
respects the System Organ Class hierarchic classification. `smq_list`
and `smq_content` for Standardized MedDRA Queries. **Caution** There
tends to be small variations in the MedDRA ascii files structure. Last
verified version on which this function is working is **26.1**. Use
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)
to load the tables afterward.

## Usage

``` r
tb_meddra(path_meddra)
```

## Arguments

- path_meddra:

  Character string, a directory containing MedDRA ascii tables. It is
  also the output directory.

## Value

.parquet files into the `path_meddra` directory. Three tables:
`meddra_hierarchy`, `smq_list`, and `smq_content`. Some columns are
returned as `integer` (all `*_code` columns). All other columns are
`character`.

## See also

[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
[`tb_who()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_who.md),
[`tb_subset()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_subset.md),
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)

## Examples

``` r
if (FALSE) { # interactive()

# Use the examples from tb_main if you want to see these functions in action.

path_meddra <- paste0(tempdir(), "/meddra_directory/")
dir.create(path_meddra)
create_ex_meddra_asc(path_meddra)

tb_meddra(path_meddra = path_meddra)

# Clear temporary files when you're done
unlink(path_meddra, recursive = TRUE)
}
```
