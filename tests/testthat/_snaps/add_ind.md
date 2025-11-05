# .data type is correctly detected and there is no other msg

    Code
      dtype <- purrr::map(list(drug_, demo_, adr_, link_, ind_), function(data_)
        add_ind(data_, i_list, drug_data = drug_, ind_data = ind_))
    Message
      i `.data` detected as `drug` table.
      i `.data` detected as `demo` table.
      i `.data` detected as `adr` table.
      i `.data` detected as `link` table.
      i `.data` detected as `ind` table.

# invalid data types to drug_data or ind_data raise error

    Code
      add_ind(demo_, i_list, drug_data = adr_, ind_data = ind_)
    Condition
      Error in `add_ind()`:
      ! `drug_data` must be a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `drug_data`. See ?drug_.

---

    Code
      add_ind(demo_, i_list, drug_data = drug_, ind_data = drug_)
    Condition
      Error in `add_ind()`:
      ! `ind_data` must be an `ind` table.
      x Missing columns: Indication
      > Supply an `ind` table to `ind_data`. See ?ind_.

