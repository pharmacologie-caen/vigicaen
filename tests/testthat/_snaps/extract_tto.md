# find proper ttos on a known dataset

    Code
      link_ <- add_adr(add_drug(link_, d_code = ex_$d_groups_drecno, drug_data = drug_),
      a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# works with vectorized adrs and drugs

    Code
      link_ <- add_adr(add_drug(link_, d_code = ex_$d_groups_drecno, drug_data = drug_),
      a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# output type is consistent in presence or absence of tto data

    Code
      link_ <- add_adr(add_drug(link_, d_code = ex_$d_groups_drecno, drug_data = drug_),
      a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# breaks if tto_mean or range are missing

    Code
      wrong_link <- add_adr(add_drug(dplyr::select(link_, -tto_mean), d_code = ex_$
        d_groups_drecno, drug_data = drug_), a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.
    Code
      expect_error(extract_tto(.data = wrong_link, adr_s = "a_colitis", drug_s = "pd1"),
      "`.data` is not a `link` table", fixed = TRUE)

---

    Code
      wrong_luda2 <- add_adr(add_drug(dplyr::select(link_, -range), d_code = ex_$
        d_groups_drecno, drug_data = drug_), a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

# works with link as Table (out of memory)

    Code
      link_ <- arrow::as_arrow_table(add_adr(add_drug(link_, d_code = ex_$
        d_groups_drecno, drug_data = drug_), a_code = ex_$a_llt, adr_data = adr_))
    Message
      i `.data` detected as `link` table.
      i `.data` detected as `link` table.

