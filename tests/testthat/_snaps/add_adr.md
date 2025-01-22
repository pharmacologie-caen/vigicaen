# works with regular names for demo and adr

    Code
      demo <- demo %>% add_adr(a_code = ex_$a_llt, a_names = a_names, adr_data = adr)
    Message
      i `.data` detected as `demo` table.

---

    Code
      demo_a <- dplyr::collect(add_adr(arrow::as_arrow_table(demo), a_code = ex_$
      a_llt, a_names = a_names, adr_data = arrow::as_arrow_table(adr)))
    Message
      i `.data` detected as `demo` table.

# works with irregular names for demo and adr

    Code
      dema <- dema %>% add_adr(a_code = ex_$a_llt, a_names = a_names, adr_data = adra)
    Message
      i `.data` detected as `demo` table.

---

    Code
      dema_a <- dplyr::collect(add_adr(arrow::as_arrow_table(dema), a_code = ex_$
      a_llt, a_names = a_names, adr_data = arrow::as_arrow_table(adra)))
    Message
      i `.data` detected as `demo` table.

# works with link data, adr identification is Adr_Id wise, not UMCReportId wise

    Code
      link_test <- data.table(Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1",
        "d5_ici1"), Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3), Dechallenge1 = NA, TimeToOnsetMin = NA) %>%
        add_adr(a_code = adr_list_test, adr_data = adr_test)
    Message
      i `.data` detected as `link` table.

---

    Code
      link_test_a <- dplyr::collect(add_adr(arrow::as_arrow_table(data.table(Drug_Id = c(
        "d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"), Adr_Id = c("a1_adr1",
        "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"), UMCReportId = c(1, 1, 2, 2, 3),
      Dechallenge1 = NA, TimeToOnsetMin = NA)), a_code = adr_list_test, adr_data = arrow::as_arrow_table(
        adr_test)))
    Message
      i `.data` detected as `link` table.

# works with adr data as the .data argument

    Code
      adr_try <- add_adr(adr_test, a_code = adr_list_test, adr_data = adr_test)
    Message
      i `.data` detected as `adr` table.

---

    Code
      adr_try_a <- dplyr::collect(add_adr(arrow::as_arrow_table(adr_test), a_code = adr_list_test,
      adr_data = arrow::as_arrow_table(adr_test)))
    Message
      i `.data` detected as `adr` table.

# works with drug data as the .data argument

    Code
      drug_try <- add_adr(drug_test, a_code = adr_list_test, adr_data = adr_test)
    Message
      i `.data` detected as `drug` table.

---

    Code
      drug_try_a <- dplyr::collect(add_adr(arrow::as_arrow_table(drug_test), a_code = adr_list_test,
      adr_data = arrow::as_arrow_table(adr_test)))
    Message
      i `.data` detected as `drug` table.

# handle ambiguous names in .data

    Code
      res <- demo_test %>% add_adr(a_code = adr_list_test, adr_data = adr_test)
    Message
      i `.data` detected as `demo` table.

---

    Code
      res_a <- dplyr::collect(add_adr(arrow::as_arrow_table(demo_test), a_code = adr_list_test,
      adr_data = arrow::as_arrow_table(adr_test)))
    Message
      i `.data` detected as `demo` table.

# Providing data_type arg raises deprecation warn

    Code
      r1 <- add_adr(demo_, a_code = ex_$a_llt, adr_data = adr_, data_type = "demo")
    Condition
      Warning:
      The `data_type` argument of `add_adr()` is deprecated as of vigicaen 0.14.1.
      i data_type is now internally detected
    Message
      i `.data` detected as `demo` table.

# adr_data should be a valid adr type data

    Code
      add_adr(demo_, a_code = ex_$a_llt, adr_data = drug_)
    Condition
      Error in `add_adr()`:
      ! `adr_data` is not an `adr` table.
      x Missing columns: Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `adr_data`. See ?adr_.

