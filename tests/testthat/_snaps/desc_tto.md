# find proper ttos on a known dataset

    Code
      link_ <- link_ %>% add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# works with vectorization

    Code
      link_ <- link_ %>% add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# format is appropriately passed to desc_cont

    Code
      link_ <- link_ %>% add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# digits are appropriately passed to desc_cont

    Code
      link_ <- link_ %>% add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

