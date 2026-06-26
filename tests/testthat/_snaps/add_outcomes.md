# check_data_out validates outcome data structure

    Code
      vigicaen:::check_data_out(data.frame(UMCReportId = 1L), ".data")
    Condition
      Error:
      ! `.data` must be an `out` table.
      x Missing columns: Seriousness and Serious
      > Supply an `out` table to `.data`. See ?out_.

---

    Code
      vigicaen:::check_data_out(demo_, ".data")
    Condition
      Error:
      ! `.data` must be an `out` table.
      x Missing columns: Seriousness and Serious
      > Supply an `out` table to `.data`. See ?out_.

# check_data_fup validates followup data structure

    Code
      vigicaen:::check_data_fup(data.frame(UMCReportId = 1L), ".data")
    Condition
      Error:
      ! `.data` must be a `followup` table.
      x Missing columns: ReplacedUMCReportId
      > Supply a `followup` table to `.data`. See ?followup_.

---

    Code
      vigicaen:::check_data_fup(out_, ".data")
    Condition
      Error:
      ! `.data` must be a `followup` table.
      x Missing columns: ReplacedUMCReportId
      > Supply a `followup` table to `.data`. See ?followup_.

# add_death raises error on wrong out_data

    Code
      add_death(demo_, out_data = adr_)
    Condition
      Error in `add_death()`:
      ! `out_data` must be an `out` table.
      x Missing columns: Seriousness and Serious
      > Supply an `out` table to `out_data`. See ?out_.

# add_death raises error on ind .data

    Code
      suppressMessages(add_death(ind_, out_data = out_))
    Condition
      Error in `add_death()`:
      ! `.data` must be one of `demo`, `drug`, `adr`, and `link`.
      x `ind` tables not supported in `add_death()`.

# add_serious raises error on wrong out_data

    Code
      add_serious(demo_, out_data = drug_)
    Condition
      Error in `add_serious()`:
      ! `out_data` must be an `out` table.
      x Missing columns: Seriousness and Serious
      > Supply an `out` table to `out_data`. See ?out_.

# add_serious raises error on ind .data

    Code
      suppressMessages(add_serious(ind_, out_data = out_))
    Condition
      Error in `add_serious()`:
      ! `.data` must be one of `demo`, `drug`, `adr`, and `link`.
      x `ind` tables not supported in `add_serious()`.

# add_fup raises error on wrong fup_data

    Code
      add_fup(demo_, fup_data = out_)
    Condition
      Error in `add_fup()`:
      ! `fup_data` must be a `followup` table.
      x Missing columns: ReplacedUMCReportId
      > Supply a `followup` table to `fup_data`. See ?followup_.

# add_fup raises error on ind .data

    Code
      suppressMessages(add_fup(ind_, fup_data = followup_))
    Condition
      Error in `add_fup()`:
      ! `.data` must be one of `demo`, `drug`, `adr`, and `link`.
      x `ind` tables not supported in `add_fup()`.

