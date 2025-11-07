# Data for the immune checkpoint inhibitors example

These are a set of data to provide examples on the package.

- `smq_sel` is a named list of smq names

- `pt_sel` is a named list of pt names

- `a_llt` is a named list of meddra llt codes related to adrs from
  `smq_sel` and `pt_sel`

- `d_drecno` is a named list of drecnos for immune checkpoint inhibitors
  (some of them)

- `d_groups` is a named list of ici classes according to icis

- `d_groups_drecno` is a named list of drecnos for drug groups

## Usage

``` r
data(ex_)
```

## Format

An object of class `list`.

## Source

VigiBase Extract Case Level

## References

There is none

## Examples

``` r
data(ex_)
ex_$pt_sel
#> $colitis
#>  [1] "Colitis"                    "Autoimmune colitis"        
#>  [3] "Colitis microscopic"        "Diarrhoea"                 
#>  [5] "Diarrhoea haemorrhagic"     "Duodenitis"                
#>  [7] "Enteritis"                  "Enterocolitis"             
#>  [9] "Enterocolitis haemorrhagic" "Ulcerative gastritis"      
#> 
#> $pneumonitis
#> [1] "Pneumonitis"          "Organising pneumonia"
#> 
```
