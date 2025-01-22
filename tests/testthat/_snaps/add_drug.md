# works with drecnos, regular names for demo and drug

    Code
      demo_n <- add_drug(demo, d_code = d_drecno, method = "DrecNo", repbasis = "sci",
        drug_data = drug)
    Message
      i `.data` detected as `demo` table.

---

    Code
      demo_a <- dplyr::collect(add_drug(arrow::as_arrow_table(demo), d_code = d_drecno,
      method = "DrecNo", repbasis = "sci", drug_data = arrow::as_arrow_table(drug)))
    Message
      i `.data` detected as `demo` table.

# works with irregular names for demo and drug

    Code
      dema <- add_drug(dema, d_code = d_drecno, method = "DrecNo", repbasis = "sci",
        drug_data = druga)
    Message
      i `.data` detected as `demo` table.

# works with mpi_list

    Code
      demo <- add_drug(demo_, d_code = mpi, method = "MedicinalProd_Id", repbasis = "sci",
        drug_data = drug_)
    Message
      i `.data` detected as `demo` table.

---

    Code
      demo_a <- add_drug(arrow::as_arrow_table(demo_), d_code = mpi, method = "MedicinalProd_Id",
      repbasis = "sci", drug_data = arrow::as_arrow_table(drug_))
    Message
      i `.data` detected as `demo` table.

# selecting only s, c, i works and provide less cases than sci altogether

    Code
      res_each <- purrr::map(bas, function(repbasis_) add_drug(demo_, d_code = d_drecno,
        method = "DrecNo", repbasis = repbasis_, drug_data = drug_))
    Message
      i `.data` detected as `demo` table.
      i `.data` detected as `demo` table.
      i `.data` detected as `demo` table.

---

    Code
      res_each_a <- purrr::map(bas, function(repbasis_) dplyr::collect(add_drug(
        arrow::as_arrow_table(demo_), d_code = d_drecno, method = "DrecNo", repbasis = repbasis_,
        drug_data = arrow::as_arrow_table(drug_))))
    Message
      i `.data` detected as `demo` table.
      i `.data` detected as `demo` table.
      i `.data` detected as `demo` table.

---

    Code
      res_all <- add_drug(demo_, d_code = d_drecno, method = "DrecNo", repbasis = "sci",
        drug_data = drug_)
    Message
      i `.data` detected as `demo` table.

---

    Code
      res_all_a <- dplyr::collect(add_drug(arrow::as_arrow_table(demo_), d_code = d_drecno,
      method = "DrecNo", repbasis = "sci", drug_data = arrow::as_arrow_table(drug_)))
    Message
      i `.data` detected as `demo` table.

# works with link and drug data, drug identification is Drug_Id wise, not UMCReportId wise

    Code
      link_test <- add_drug(data.table(Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3",
        "d4_ici1", "d5_ici1"), Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4",
        "a5_adr2"), UMCReportId = c(1, 1, 2, 2, 3), Dechallenge1 = NA,
      TimeToOnsetMin = NA), d_code = d_drecno_test, drug_data = drug_test)
    Message
      i `.data` detected as `link` table.

---

    Code
      link_test_a <- dplyr::collect(add_drug(arrow::as_arrow_table(data.table(
        Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"), Adr_Id = c(
          "a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"), UMCReportId = c(1,
          1, 2, 2, 3), Dechallenge1 = NA, TimeToOnsetMin = NA)), d_code = d_drecno_test,
      drug_data = arrow::as_arrow_table(drug_test)))
    Message
      i `.data` detected as `link` table.

---

    Code
      drug_output_test <- add_drug(drug_test, d_code = d_drecno_test, drug_data = drug_test)
    Message
      i `.data` detected as `drug` table.

---

    Code
      drug_output_test_a <- dplyr::collect(add_drug(arrow::as_arrow_table(drug_test),
      d_code = d_drecno_test, drug_data = arrow::as_arrow_table(drug_test)))
    Message
      i `.data` detected as `drug` table.

# works with adr data, drug identification is UMCReportId wise

    Code
      adr_test <- add_drug(data.table(UMCReportId = c(1, 1, 2, 2, 3), Adr_Id = c(
        "a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"), MedDRA_Id = c(1e+05,
        20000, 30000, 40000, 50000), Outcome = c(1, 2, 3, 2, 2)), d_code = d_drecno_test,
      drug_data = drug_test)
    Message
      i `.data` detected as `adr` table.

---

    Code
      adr_test_a <- dplyr::collect(add_drug(arrow::as_arrow_table(data.table(
        UMCReportId = c(1, 1, 2, 2, 3), Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2",
          "a4_adr4", "a5_adr2"), MedDRA_Id = c(1e+05, 20000, 30000, 40000, 50000),
        Outcome = c(1, 2, 3, 2, 2))), d_code = d_drecno_test, drug_data = arrow::as_arrow_table(
        drug_test)))
    Message
      i `.data` detected as `adr` table.

# handle ambiguous names in .data

    Code
      res <- add_drug(demo_test, d_code = d_drecno_test, method = "DrecNo", repbasis = "sci",
        drug_data = drug_test)
    Message
      i `.data` detected as `demo` table.

---

    Code
      res_a <- dplyr::collect(add_drug(arrow::as_arrow_table(demo_test), d_code = d_drecno_test,
      method = "DrecNo", repbasis = "sci", drug_data = arrow::as_arrow_table(
        drug_test)))
    Message
      i `.data` detected as `demo` table.

# you can choose output column names with d_names

    Code
      res <- add_drug(demo_test, d_code = d_drecno_test, d_names = changed_names,
        method = "DrecNo", repbasis = "sci", drug_data = drug_test)
    Message
      i `.data` detected as `demo` table.

---

    Code
      res_a <- add_drug(arrow::as_arrow_table(demo_test), d_code = d_drecno_test,
      d_names = changed_names, method = "DrecNo", repbasis = "sci", drug_data = arrow::as_arrow_table(
        drug_test))
    Message
      i `.data` detected as `demo` table.

# you can use arrow/parquet format

    Code
      res <- add_drug(demo_parquet, d_code = d_drecno_test, method = "DrecNo",
        repbasis = "sci", drug_data = drug_parquet)
    Message
      i `.data` detected as `demo` table.

---

    Code
      res_a <- add_drug(demo_test, d_code = d_drecno_test, method = "DrecNo",
        repbasis = "sci", drug_data = drug_test)
    Message
      i `.data` detected as `demo` table.

# Providing data_type arg raises deprecation warn

    Code
      r1 <- add_drug(demo_, d_code = ex_$d_drecno, method = "DrecNo", drug_data = drug_,
      data_type = "demo")
    Condition
      Warning:
      The `data_type` argument of `add_drug()` is deprecated as of vigicaen 0.14.1.
      i data_type is now internally detected
    Message
      i `.data` detected as `demo` table.

# drug_data should be a valid drug type data

    Code
      add_drug(demo_, d_code = ex_$d_drecno, method = "DrecNo", drug_data = adr_,
      data_type = "demo")
    Condition
      Error in `add_drug()`:
      ! `drug_data` is not a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `drug_data`. See ?drug_.

