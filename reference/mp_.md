# Sample of WHODrug

A small part of WHODrug, used to illustrate the package examples and
vignettes. You can find DrecNo related to immune checkpoint inhibitors,
paracetamol, tramadol, tretinoin, anti-thrombin iii, and ATC classes
L03AA Colony stimulating factors, C09AA ACE inhibitors, plain, J01CA
Penicillins with extended spectrum. Compounds are `thg_` and `mp_`. See
examples in
[`get_drecno`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
and
[`get_atc_code`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)

## Usage

``` r
data(mp_)

thg_
```

## Format

`mp_` is a data.table with 8 variables and 14146 rows.

- `MedicinalProd_Id` Integer. The medicinalproduct identifier.

- `Sequence.number.1` and `2` Characters. Complement to DrecNo.

- `DrecNo` Character. Drug Record Number, pivotal to identify drugs with
  [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md).

- `drug_name_t` Character. The name of the drug. Compared to the
  original `drug_name` variable in `mp` table, this variable is trimmed
  for white spaces, and names are in lowercase.

- `Create.date` Character. The date the record was created.

- `Date.changed` Character. The date the record was last changed.

- `Country` Character. The country where the record was created.

`thg_` is a data.table with 5 variables and 4079 rows.

- `Therapgroup_Id` Integer. The identifier of the therapeutic group.

- `ATC.code` Character. The ATC code of the drug.

- `Create.date` Character. The date the record was created.

- `Official.ATC.code` Character. Whether the ATC code is official
  (Yes/No).

- `MedicinalProd_Id` Integer. The medicinalproduct identifier.

An object of class `data.table` (inherits from `data.frame`) with 4079
rows and 5 columns.

## Source

None

## References

There is none

## Examples

``` r
data(mp_)
```
