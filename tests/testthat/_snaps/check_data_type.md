# cli format and basic use work

    Code
      check_data_demo(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `demo` table.
      x Missing columns: Type, DateDatabase, and Region
      > Supply a `demo` table to `.data`. See ?demo_.

---

    Code
      check_data_demo(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `demo` table.
      x Missing columns: Type, DateDatabase, and Region
      > Supply a `demo` table to `.data`. See ?demo_.

---

    Code
      check_data_demo(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `demo` table.
      x Missing columns: Type, DateDatabase, and Region
      > Supply a `demo` table to `.data`. See ?demo_.

---

    Code
      check_data_demo(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `demo` table.
      x Missing columns: UMCReportId, Type, DateDatabase, and Region
      > Supply a `demo` table to `.data`. See ?demo_.

---

    Code
      check_data_link(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `link` table.
      x Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `.data`. See ?link_.

---

    Code
      check_data_link(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `link` table.
      x Missing columns: Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `.data`. See ?link_.

---

    Code
      check_data_link(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `link` table.
      x Missing columns: Drug_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `.data`. See ?link_.

---

    Code
      check_data_link(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be a `link` table.
      x Missing columns: Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `.data`. See ?link_.

---

    Code
      check_data_ind(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be an `ind` table.
      x Missing columns: Indication and Drug_Id
      > Supply an `ind` table to `.data`. See ?ind_.

---

    Code
      check_data_ind(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be an `ind` table.
      x Missing columns: Indication
      > Supply an `ind` table to `.data`. See ?ind_.

---

    Code
      check_data_ind(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be an `ind` table.
      x Missing columns: Indication and Drug_Id
      > Supply an `ind` table to `.data`. See ?ind_.

---

    Code
      check_data_ind(d_, ".data")
    Condition
      Error in `.f()`:
      ! `.data` must be an `ind` table.
      x Missing columns: Indication
      > Supply an `ind` table to `.data`. See ?ind_.

---

    Code
      check_data_meddra(demo_valid, ".data")
    Condition
      Error:
      ! `.data` must be a `meddra` table.
      x Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      > Supply a `meddra` table to `.data`. See ?meddra_.

# format is ok [plain]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be an `adr` table.
      x Missing columns: Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be a `link` table.
      x Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be a `meddra` table.
      x Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      > Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      Error:
      ! `x` must be an `smq_list` table.
      x Invalid/missing columns detected
      i Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      > See ?smq_list_.

# format is ok [ansi]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a `drug` table.
      [31mx[39m Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be an `adr` table.
      [31mx[39m Missing columns: Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a `link` table.
      [31mx[39m Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a `meddra` table.
      [31mx[39m Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      > Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be an `smq_list` table.
      [31mx[39m Invalid/missing columns detected
      [36mi[39m Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      > See ?smq_list_.

# format is ok [unicode]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be a `drug` table.
      ✖ Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      → Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be an `adr` table.
      ✖ Missing columns: Adr_Id, MedDRA_Id, and Outcome
      → Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be a `link` table.
      ✖ Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      → Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` must be a `meddra` table.
      ✖ Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      → Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      Error:
      ! `x` must be an `smq_list` table.
      ✖ Invalid/missing columns detected
      ℹ Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      → See ?smq_list_.

# format is ok [fancy]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a `drug` table.
      [31m✖[39m Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      → Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be an `adr` table.
      [31m✖[39m Missing columns: Adr_Id, MedDRA_Id, and Outcome
      → Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a `link` table.
      [31m✖[39m Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      → Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a `meddra` table.
      [31m✖[39m Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      → Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be an `smq_list` table.
      [31m✖[39m Invalid/missing columns detected
      [36mℹ[39m Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      → See ?smq_list_.

# smq_list is distinguished of smq_list_content

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      Error:
      ! `x` must be an `smq_list` table.
      x Invalid/missing columns detected
      i Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      > See ?smq_list_.

# works with arrow::Table

    Code
      check_data_demo(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error:
      ! Could not evaluate cli `{}` expression: `arg`.
      Caused by error:
      ! object 'name' not found

---

    Code
      check_data_demo(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error:
      ! Could not evaluate cli `{}` expression: `arg`.
      Caused by error:
      ! object 'name' not found

---

    Code
      check_data_demo(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error:
      ! Could not evaluate cli `{}` expression: `arg`.
      Caused by error:
      ! object 'name' not found

---

    Code
      check_data_demo(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error:
      ! Could not evaluate cli `{}` expression: `arg`.
      Caused by error:
      ! object 'name' not found

---

    Code
      check_data_demo(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error:
      ! Could not evaluate cli `{}` expression: `arg`.
      Caused by error:
      ! object 'name' not found

---

    Code
      check_data_drug(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `demo_data` must be a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `demo_data`. See ?drug_.

---

    Code
      check_data_drug(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `adr_data` must be a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `adr_data`. See ?drug_.

---

    Code
      check_data_drug(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `link_data` must be a `drug` table.
      x Missing columns: DrecNo and MedicinalProd_Id
      > Supply a `drug` table to `link_data`. See ?drug_.

---

    Code
      check_data_drug(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `meddra_data` must be a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, UMCReportId, and Drug_Id
      > Supply a `drug` table to `meddra_data`. See ?drug_.

---

    Code
      check_data_drug(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `smq_list_data` must be a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, UMCReportId, and Drug_Id
      > Supply a `drug` table to `smq_list_data`. See ?drug_.

---

    Code
      check_data_adr(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `demo_data` must be an `adr` table.
      x Missing columns: Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `demo_data`. See ?adr_.

---

    Code
      check_data_adr(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `drug_data` must be an `adr` table.
      x Missing columns: Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `drug_data`. See ?adr_.

---

    Code
      check_data_adr(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `link_data` must be an `adr` table.
      x Missing columns: MedDRA_Id and Outcome
      > Supply an `adr` table to `link_data`. See ?adr_.

---

    Code
      check_data_adr(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `meddra_data` must be an `adr` table.
      x Missing columns: UMCReportId, Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `meddra_data`. See ?adr_.

---

    Code
      check_data_adr(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `smq_list_data` must be an `adr` table.
      x Missing columns: UMCReportId, Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `smq_list_data`. See ?adr_.

---

    Code
      check_data_link(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `demo_data` must be a `link` table.
      x Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `demo_data`. See ?link_.

---

    Code
      check_data_link(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `adr_data` must be a `link` table.
      x Missing columns: Drug_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `adr_data`. See ?link_.

---

    Code
      check_data_link(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `drug_data` must be a `link` table.
      x Missing columns: Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `drug_data`. See ?link_.

---

    Code
      check_data_link(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `meddra_data` must be a `link` table.
      x Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `meddra_data`. See ?link_.

---

    Code
      check_data_link(arrow::as_arrow_table(d_), paste0(name, "data"))
    Condition
      Error in `.f()`:
      ! `smq_list_data` must be a `link` table.
      x Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `smq_list_data`. See ?link_.

