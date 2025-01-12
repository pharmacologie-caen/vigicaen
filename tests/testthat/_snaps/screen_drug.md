# checkers of mp_data and .data are ok

    Code
      screen_drug(drug_true, mp_data = mp_false)
    Message
      x `mp_data` is invalid.
      ! Either `DrecNo` or `drug_name_t` columns are missing.
      > Supply an `mp` table to `mp_data`. See ?mp_.
    Condition
      Error in `screen_drug()`:
      ! cli-19644-10

---

    Code
      screen_drug(drug_true, mp_data = mp_false2)
    Message
      x `mp_data` is invalid.
      ! Either `DrecNo` or `drug_name_t` columns are missing.
      > Supply an `mp` table to `mp_data`. See ?mp_.
    Condition
      Error in `screen_drug()`:
      ! cli-19644-46

---

    Code
      screen_drug(drug_false, mp_data = mp_true)
    Message
      x `.data` is not a `drug` table.
      > Supply a `drug` table to `.data`. See ?drug_.
    Condition
      Error in `screen_drug()`:
      ! cli-19644-82

