# returns proper counts

    Code
      demo <- demo_ %>% add_adr(a_code = ex_$a_llt, a_names = a_names, adr = adr_)
    Message
      i `.data` detected as `demo` table.

# works with out of memory tables

    Code
      demo <- add_adr(arrow::as_arrow_table(demo_), a_code = ex_$a_llt, a_names = a_names,
      adr = arrow::as_arrow_table(adr_))
    Message
      i `.data` detected as `demo` table.

