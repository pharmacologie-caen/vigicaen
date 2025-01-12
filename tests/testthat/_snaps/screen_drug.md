# error in mp_data [plain]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      Error in `screen_drug()`:
      ! `mp_data` is invalid.
      ! Either `DrecNo` or `drug_name_t` columns are missing.
      > Supply an `mp` table to `mp_data`. See ?mp_.

# error in mp_data [ansi]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      [1m[33mError[39m in `screen_drug()`:[22m
      [1m[22m[33m![39m `mp_data` is invalid.
      [33m![39m Either `DrecNo` or `drug_name_t` columns are missing.
      > Supply an `mp` table to `mp_data`. See ?mp_.

# error in mp_data [unicode]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      Error in `screen_drug()`:
      ! `mp_data` is invalid.
      ! Either `DrecNo` or `drug_name_t` columns are missing.
      → Supply an `mp` table to `mp_data`. See ?mp_.

# error in mp_data [fancy]

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Condition
      [1m[33mError[39m in `screen_drug()`:[22m
      [1m[22m[33m![39m `mp_data` is invalid.
      [33m![39m Either `DrecNo` or `drug_name_t` columns are missing.
      → Supply an `mp` table to `mp_data`. See ?mp_.

---

    Code
      screen_drug(drug_true, mp_data = mp_false2)
    Condition
      Error in `screen_drug()`:
      ! `mp_data` is invalid.
      ! Either `DrecNo` or `drug_name_t` columns are missing.
      > Supply an `mp` table to `mp_data`. See ?mp_.

---

    Code
      screen_drug(drug_false, mp_data = mp_true)
    Condition
      Error in `screen_drug()`:
      ! `.data` is not a `drug` table.
      > Supply a `drug` table to `.data`. See ?drug_.

