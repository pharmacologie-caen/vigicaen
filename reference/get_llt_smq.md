# Extract low level terms from SMQs

**\[stable\]** Collect llts from `smq_list` and `smq_content`
data.tables, given an SMQ.

## Usage

``` r
get_llt_smq(
  smq,
  smq_scope = c("narrow", "broad"),
  smq_list,
  smq_content,
  smq_list_content = deprecated()
)
```

## Arguments

- smq:

  A named list of character vector(s).

- smq_scope:

  A character vector. One of "narrow" or "broad".

- smq_list:

  A data.table. A list of SMQs.

- smq_content:

  A data.table. A list of SMQs content.

- smq_list_content:

  **\[deprecated\]**

## Value

A named list of integers. Low-level term codes.

## Details

`get_llt_smq()` is an *ID collector* function. SMQ stands for
Standardized MedDRA query. `get_llt_smq()`only works with
NON-algorithmic SMQs (this status is given in the smq_list table). See
[`smq_list_`](https://pharmacologie-caen.github.io/vigicaen/reference/meddra_.md)
and
[`smq_content_`](https://pharmacologie-caen.github.io/vigicaen/reference/meddra_.md).
You can choose between the narrow and the broad scope of the SMQ. If you
want to work with the SOC hierarchy, use
[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md).

## See also

[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)

## Examples

``` r
if (FALSE) { # interactive()
## Finding llt codes for Embolism (SMQ)

smq_sel <- rlang::list2(
  embolism = "Embolic and thrombotic events, venous (SMQ)"
 )
get_llt_smq(smq_sel,
                smq_scope = "narrow",
                smq_list = smq_list_,
                smq_content = smq_content_
                )

# You can query multiple SMQs in one item, and query high level SMQs
smq_sel2 <-
  rlang::list2(
    sepsis = c("Sepsis (SMQ)","Toxic-septic shock conditions (SMQ)"),
    ischemic_heart_disease = c("Ischaemic heart disease (SMQ)")
  )

get_llt_smq(smq_sel2,
            smq_scope = "narrow",
            smq_list = smq_list_,
            smq_content = smq_content_
            )
}
```
