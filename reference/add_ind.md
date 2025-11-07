# Add indication

**\[experimental\]** Creates indication columns. in vigibase datasets
(demo, link, adr, drug, or ind).

## Usage

``` r
add_ind(.data, i_list, i_names = names(i_list), drug_data, ind_data)
```

## Arguments

- .data:

  The dataset used to identify individual reports (usually, it is
  `demo`)

- i_list:

  A named list of indication terms. See Details.

- i_names:

  A character vector. Names for indication columns (must be the same
  length as i_list), default to `names(i_list)`

- drug_data:

  A data.frame containing the drug data (usually, it is `drug`)

- ind_data:

  A data.frame containing the indication data (usually, it is `ind`)

## Value

A dataset with the new indication columns. Each element of `i_names`
will add a column with the same name in `.data`. The value can be

- 0 The corresponding indication is absent.

- 1 The indication is present in the case if `.data` is `demo` or `adr`,
  or "this row correspond to this indication", if `.data` is `drug`,
  `link` or `ind`).

- NA There is no indication data for this case / drug.

## Details

Indication terms are issued from either MedDRA or International
Classification of Diseases (ICD) - you need to use *both* dictionaries,
should you wish to capture all terms related to a specific disease.
Indication terms are not translated into codes in VigiBase ECL, unlike
drug or adr terms. Therefore, there is no `get_*` step to collect such
codes. The terms are passed directly to `i_list`, which should still be
a *named* list containing indication terms.

## See also

[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)

## Examples

``` r
# Set up a list of indication terms

i_list <-
  list(
    melanoma = c("Malignant melanoma", "Metastatic malignant melanoma"),
    lung_cancer = c("Non-small cell lung cancer", "Lung adenocarcinoma")
    )

 demo <-
   demo_ |>
   add_ind(i_list,
           drug_data = drug_,
           ind_data  = ind_)
#> ℹ `.data` detected as `demo` table.

 demo |> desc_facvar(names(i_list))
#> # A tibble: 4 × 4
#>   var         level value         n_avail
#>   <chr>       <chr> <chr>           <int>
#> 1 melanoma    0     574/660 (87%)     660
#> 2 melanoma    1     86/660 (13%)      660
#> 3 lung_cancer 0     582/660 (88%)     660
#> 4 lung_cancer 1     78/660 (12%)      660
```
