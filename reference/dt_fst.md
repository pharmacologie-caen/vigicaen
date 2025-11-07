# Read fst and convert to data.table

**\[deprecated\]** Short hand to `as.data.table(read_fst())`. File
extension can be omitted.

## Usage

``` r
dt_fst(path_base, name = NULL, ext = ".fst")
```

## Arguments

- path_base:

  A character string, providing the path to read from.

- name:

  A character string, the file name.

- ext:

  A character string, optional, specifying the file extension.

## Value

A data.table, read from `path_base/(name)`.

## Details

Output is a data.table. The function is deprecated, with the use of
parquet tables. Tables can now be loaded **IN**-memory or **OUT** of
memory with
[`dt_parquet`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md).

## See also

[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md),
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
[`tb_who()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_who.md),
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md)

## Examples

``` r
# dt_fst is deprecated and will generate an error
```
