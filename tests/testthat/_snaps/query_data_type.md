# cli formatting

    Code
      vigicaen:::query_data_type(invalid_data, ".data")
    Condition
      Error:
      ! `invalid_data` must match an expected data type.
      ! Column names are not those of expected data types.
      > Supported types are demo, drug, adr, ind, and link. See ?demo_.

# cli formatting / format is ok [plain]

    Code
      vigicaen:::query_data_type(drug_valid, ".data")
    Message
      i `.data` detected as `drug` table.
    Output
      [1] "drug"

# cli formatting / format is ok [ansi]

    Code
      vigicaen:::query_data_type(drug_valid, ".data")
    Message
      [1m[22m[36mi[39m `.data` detected as `drug` table.
    Output
      [1] "drug"

# cli formatting / format is ok [unicode]

    Code
      vigicaen:::query_data_type(drug_valid, ".data")
    Message
      ℹ `.data` detected as `drug` table.
    Output
      [1] "drug"

# cli formatting / format is ok [fancy]

    Code
      vigicaen:::query_data_type(drug_valid, ".data")
    Message
      [1m[22m[36mℹ[39m `.data` detected as `drug` table.
    Output
      [1] "drug"

---

    Code
      dtype <- purrr::map(list(drug_valid, demo_valid, adr_valid, link_valid,
        ind_valid), function(data_) vigicaen:::query_data_type(data_, ".data"))
    Message
      i `.data` detected as `drug` table.
      i `.data` detected as `demo` table.
      i `.data` detected as `adr` table.
      i `.data` detected as `link` table.
      i `.data` detected as `ind` table.

