# find proper counts on a known dataset

    Code
      link_ <- link_ %>% add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# can be vectorized

    Code
      link_ <- link_ %>% add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# works with out of memory arrow Table

    Code
      link_ <- add_adr(add_drug(arrow::as_arrow_table(link_), d_code = ex_$
        d_groups_drecno, drug_data = arrow::as_arrow_table(drug_)), a_code = ex_$
      a_llt, adr_data = arrow::as_arrow_table(adr_))
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

