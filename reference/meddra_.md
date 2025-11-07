# Sample of MedDRA

Anonymized data from MedDRA, used to illustrate the package examples and
vignettes. You can find term codes related to colitis, pneumonitis,
hepatitis, a SMQ of embolisms. Compounds are `meddra_`, `smq_list_`,
`smq_content_` and `smq_list_content_`. Create dedicated .parquet files
using
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md).
See examples in
[`get_llt_soc`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
and
[`get_llt_smq`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)

## Usage

``` r
data(meddra_)

smq_list_content_

smq_list_

smq_content_
```

## Format

`meddra_` is a data.table with 15 variables and 677 rows.

- The `*_code` columns. Integers. MedDRA code for the given term.

- The `*_name` columns. Characters. The name of the term.

- `soc_abbrev` Character. The abbreviation of the SOC.

- `null_field` Logical. Empty column.

- `pt_soc_code` Integer. The preferred term code of the SOC itself.

- `primary_soc_fg` Character. Whether the SOC is primary for this code.
  "Y" or "N", Yes or No.

- `empty_col` Logical. Empty column.

`smq_list_` is a data.table with 9 variables and 11 rows. It is the list
of SMQ.

- `smq_code` Integer. The code of the SMQ.

- `smq_name` Character. The name of the SMQ.

- `smq_level` Integer. The hierarchical level of the SMQ.

- `smq_description` Character. The description of the SMQ.

- `smq_source` Character. The source of the SMQ.

- `smq_note` Character. Additional note on the SMQ.

- `MedDRA_version` Numeric. The version of MedDRA.

- `status` Character. The status of the SMQ (active or not)

- `smq_algorithm` Character. Whether the SMQ is algorithmic or not.

- `empty_col` Logical. Empty column.

`smq_content_` is a data.table with 9 variables and 3386 rows. It is the
content of each SMQ.

- `smq_code` Integer. The code of the SMQ.

- `term_code` Integer. The low-level term code.

- `term_level` Integer. The hierarchical level of the term.

- `term_scope` Integer. The scope of the term (narrow 2 or broad 1)

- `term_category` Character. In algorithmic SMQs, the category of the
  term.

- `term_weight` Integer. The weight of the term (algorithmic SMQs).

- `term_status` Integer. The status of the term (active or not)

- `term_addition_version` Numeric. The version of the term addition.

- `term_last_modified_version` Numeric. The last MedDRA version the term
  was modified.

- `empty_col` Logical. Empty column.

`smq_list_content_` is a data.table with 19 variables and 3386 rows. It
is a fusion of smq_list and smq_content, as created with
[`tb_meddra()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_meddra.md).

- `smq_code` Integer. The code of the SMQ.

- `smq_name` Character. The name of the SMQ.

- `smq_level` Integer. The hierarchical level of the SMQ.

- `smq_description` Character. The description of the SMQ.

- `smq_source` Character. The source of the SMQ.

- `smq_note` Character. Additional note on the SMQ.

- `MedDRA_version` Numeric. The version of MedDRA.

- `status` Character. The status of the SMQ (active or not)

- `smq_algorithm` Character. Whether the SMQ is algorithmic or not.

- `empty_col.x` Logical. Empty column.

- `term_code` Integer. The low-level term code.

- `term_level` Integer. The hierarchical level of the term.

- `term_scope` Integer. The scope of the term (narrow 2 or broad 1)

- `term_category` Character. In algorithmic SMQs, the category of the
  term.

- `term_weight` Integer. The weight of the term (algorithmic SMQs).

- `term_status` Integer. The status of the term (active or not)

- `term_addition_version` Numeric. The version of the term addition.

- `term_last_modified_version` Numeric. The last MedDRA version the term
  was modified.

- `empty_col.y` Logical. Empty column.

An object of class `data.table` (inherits from `data.frame`) with 3386
rows and 19 columns.

An object of class `data.table` (inherits from `data.frame`) with 11
rows and 9 columns.

An object of class `data.table` (inherits from `data.frame`) with 3386
rows and 9 columns.

## Source

None

## References

There is none

## Examples

``` r
data(meddra_)
```
