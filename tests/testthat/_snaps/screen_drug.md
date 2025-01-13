# error in mp_data [plain]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      Error in `screen_drug()`:
      ! `mp_data` is not an `mp` table.
      x Missing columns: drug_name_t
      > Supply an `mp` table to `mp_data`. See ?mp_.

# error in mp_data [ansi]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      [1m[33mError[39m in `screen_drug()`:[22m
      [1m[22m[33m![39m `mp_data` is not an `mp` table.
      [31mx[39m Missing columns: drug_name_t
      > Supply an `mp` table to `mp_data`. See ?mp_.

# error in mp_data [unicode]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      Error in `screen_drug()`:
      ! `mp_data` is not an `mp` table.
      ✖ Missing columns: drug_name_t
      → Supply an `mp` table to `mp_data`. See ?mp_.

# error in mp_data [fancy]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      [1m[33mError[39m in `screen_drug()`:[22m
      [1m[22m[33m![39m `mp_data` is not an `mp` table.
      [31m✖[39m Missing columns: drug_name_t
      → Supply an `mp` table to `mp_data`. See ?mp_.

---

    Code
      screen_drug(drug_true, mp_data = mp_false2)
    Condition
      Error in `screen_drug()`:
      ! `mp_data` is not an `mp` table.
      x Missing columns: DrecNo
      > Supply an `mp` table to `mp_data`. See ?mp_.

---

    Code
      screen_drug(drug_false, mp_data = mp_true)
    Condition
      Error in `screen_drug()`:
      ! `.data` is not a `drug` table.
      x Missing columns: MedicinalProd_Id and Drug_Id
      > Supply a `drug` table to `.data`. See ?drug_.

