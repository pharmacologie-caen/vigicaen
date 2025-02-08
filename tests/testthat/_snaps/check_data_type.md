# cli format and basic use work

    Code
      check_data_link(drug_valid, ".data")
    Condition
      Error:
      ! `.data` is not a `link` table.
      x Missing columns: Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `.data`. See ?link_.

---

    Code
      check_data_link(adr_valid, ".data")
    Condition
      Error:
      ! `.data` is not a `link` table.
      x Missing columns: Drug_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `.data`. See ?link_.

---

    Code
      check_data_link(demo_valid, ".data")
    Condition
      Error:
      ! `.data` is not a `link` table.
      x Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `.data`. See ?link_.

---

    Code
      check_data_meddra(demo_valid, ".data")
    Condition
      Error:
      ! `.data` is not a `meddra` table.
      x Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      > Supply a `meddra` table to `.data`. See ?meddra_.

# format is ok [plain]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not a `drug` table.
      x Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not an `adr` table.
      x Missing columns: Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not a `link` table.
      x Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not a `meddra` table.
      x Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      > Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      Error:
      ! `x` is not an `smq_list` table.
      x Invalid/missing columns detected
      > Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      > See ?smq_list_.

# format is ok [ansi]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a `drug` table.
      [31mx[39m Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      > Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not an `adr` table.
      [31mx[39m Missing columns: Adr_Id, MedDRA_Id, and Outcome
      > Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a `link` table.
      [31mx[39m Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      > Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a `meddra` table.
      [31mx[39m Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      > Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not an `smq_list` table.
      [31mx[39m Invalid/missing columns detected
      > Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      > See ?smq_list_.

# format is ok [unicode]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not a `drug` table.
      ✖ Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      → Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not an `adr` table.
      ✖ Missing columns: Adr_Id, MedDRA_Id, and Outcome
      → Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not a `link` table.
      ✖ Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      → Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      Error:
      ! `x` is not a `meddra` table.
      ✖ Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      → Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      Error:
      ! `x` is not an `smq_list` table.
      ✖ Invalid/missing columns detected
      → Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      → See ?smq_list_.

# format is ok [fancy]

    Code
      check_data_drug(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a `drug` table.
      [31m✖[39m Missing columns: DrecNo, MedicinalProd_Id, and Drug_Id
      → Supply a `drug` table to `x`. See ?drug_.

---

    Code
      check_data_adr(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not an `adr` table.
      [31m✖[39m Missing columns: Adr_Id, MedDRA_Id, and Outcome
      → Supply an `adr` table to `x`. See ?adr_.

---

    Code
      check_data_link(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a `link` table.
      [31m✖[39m Missing columns: Drug_Id, Adr_Id, Dechallenge1, tto_mean, and range
      → Supply a `link` table to `x`. See ?link_.

---

    Code
      check_data_meddra(data_invalid, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a `meddra` table.
      [31m✖[39m Missing columns: llt_code, llt_name, pt_name, soc_name, and hlt_name
      → Supply a `meddra` table to `x`. See ?meddra_.

---

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not an `smq_list` table.
      [31m✖[39m Invalid/missing columns detected
      → Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      → See ?smq_list_.

# smq_list is distinguished of smq_list_content

    Code
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    Condition
      Error:
      ! `x` is not an `smq_list` table.
      x Invalid/missing columns detected
      > Did you provide an `smq_list_content`, instead of an `smq_list` dataset?.
      > See ?smq_list_.

