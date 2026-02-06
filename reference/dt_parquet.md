# Read parquet and convert to data.table

**\[stable\]** Load data IN- our OUT- of memory. File extension can be
omitted.

## Usage

``` r
dt_parquet(path_base, name = NULL, ext = ".parquet", in_memory = TRUE)
```

## Arguments

- path_base:

  A character string, providing the path to read from.

- name:

  Optional. A character string. The file name (if absent from
  `path_base`).

- ext:

  Optional. A character string. The file extension.

- in_memory:

  Logical, should data be loaded in memory?

## Value

A data.table if `in_memory` is set to `TRUE`, a parquet Table if
`in_memory` is set to `FALSE`.

## Details

Output is a data.table. For meddra and whodrug tables, it is still a
good option to load data in-memory. This function is wrapping
[`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html),
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
and
[`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)
altogether. If you want to load **OUT** of memory, set arg `in_memory`
to FALSE. **Be careful that doing so will change the function output
format**. For this latter case, the output is not a data.table, so there
is no practical benefit as compared to using
[`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html)
directly, with `as_data_frame` = FALSE.

## See also

[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
[`tb_who()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_who.md),
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md)

## Examples

``` r
# Say you have a data.frame stored in a parquet format, such as this one
demo <-
  data.table::data.table(
    UMCReportId = c(1, 2, 3, 4),
    AgeGroup = c(1, 7, 7, 8)
  ) |>
  arrow::as_arrow_table()

tmp_folder <- paste0(tempdir(), "/dtparquetex")
dir.create(tmp_folder)
path_data <- paste0(tmp_folder, "/")

arrow::write_parquet(demo,
                     sink = paste0(path_data, "demo.parquet")
)

# Now you have a new session without demo
rm(demo)

# You may import the file directly to data.table format with dt_parquet
demo <-
  dt_parquet(path_data, "demo")

# Clean up (required for CRAN checks)
unlink(tmp_folder, recursive = TRUE)
```
