# gives proper counts

    Code
      adr_test <- adr_test %>% add_drug(d_code = d_drecno_test, drug_data = drug_test) %>%
        add_adr(a_code = adr_list_test, adr_data = adr_test)
    Message
      i `.data` detected as `adr` table.
      i `.data` detected as `adr` table.

