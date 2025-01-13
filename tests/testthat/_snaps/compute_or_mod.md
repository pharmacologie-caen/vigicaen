# accurate results

    Code
      demo <- demo_ %>% add_drug(d_code = ex_$d_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `demo` table.
      i `.data` detected as `demo` table.

# works with and without p_val arg

    Code
      demo <- demo_ %>% add_drug(d_code = ex_$d_drecno, drug_data = drug_) %>%
        add_adr(a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `demo` table.
      i `.data` detected as `demo` table.

