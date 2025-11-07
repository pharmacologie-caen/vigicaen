# Get low level term codes from soc classification

**\[stable\]** Collect llt codes from a `meddra` data.table, given
another term of the MedDRA SOC Hierarchy.

## Usage

``` r
get_llt_soc(
  term_sel,
  term_level = c("soc", "hlgt", "hlt", "pt", "llt"),
  meddra,
  verbose = TRUE
)
```

## Arguments

- term_sel:

  A named list of character vector(s). The terms to extract llts codes
  from. See details.

- term_level:

  A character string. One of "soc", "hlgt", "hlt", "pt", or "llt"

- meddra:

  A data.table. Built from meddra_builders functions

- verbose:

  Logical. Allows you to see matching reactions in the console.

## Value

A named list of integers. Low-level term codes.

## Details

`get_llt_soc()` is an *ID collector* function. The function extracts low
level term codes. `get_llt_soc()` is **case-sensitive**, and MedDRA
terms always begin with a capital letter, in their native version. In
`term_sel`, all terms should come from the same hierarchical level, e.g.
all preferred terms, all high level terms, etc.

## See also

[`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)

## Examples

``` r
if (FALSE) { # interactive()

## Finding llt codes for colitis

pt_sel <- rlang::list2(
  colitis = c("Colitis",
              "Autoimmune colitis"),
  pneumonitis = c("Pneumonitis",
                  "Organising pneumonia")
  )

hlt_sel <- rlang::list2(
  colitis = c("Gastrointestinal inflammatory disorders NEC"),
  pneumonitis = c("Pulmonary thrombotic and embolic conditions")
  )

# Remember you can use more than one term to define each adverse reaction,
# but they should all be at the same hierarchical level in meddra.

# with preferred terms

get_llt_soc(
  term_sel = pt_sel,
  term_level = "pt",
  meddra = meddra_
  )

# with high level terms

get_llt_soc(
  term_sel = hlt_sel,
  term_level = "hlt",
  meddra = meddra_
  )
}
```
