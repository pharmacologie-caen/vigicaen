# checkers of d_code and a_code work

    Code
      vigi_routine(demo_data = demo, drug_data = drug, adr_data = adr, link_data = link,
        d_code = d_drecno_toolong, a_code = a_llt, vigibase_version = "September 2024")
    Condition <length_one>
      Error in `vigi_routine()`:
      ! `d_code` must have only one item in `vigi_routine()`.
      x `d_code` has 2 items.

---

    Code
      vigi_routine(demo_data = demo, drug_data = drug, adr_data = adr, link_data = link,
        d_code = d_drecno, a_code = a_llt_toolong, vigibase_version = "September 2024")
    Condition <length_one>
      Error in `vigi_routine()`:
      ! `a_code` must have only one item in `vigi_routine()`.
      x `a_code` has 2 items.

---

    Code
      vigi_routine(demo_data = demo, drug_data = drug, adr_data = adr, link_data = link,
        d_code = "nivolumab", a_code = a_llt, vigibase_version = "September 2024")
    Condition
      Error in `vigi_routine()`:
      ! `d_code` must be a named list.
      x `d_code` is of class character.
      > Supply a named list to `d_code`. See ?ex_.

---

    Code
      vigi_routine(demo_data = demo, drug_data = drug, adr_data = adr, link_data = link,
        d_code = d_drecno, a_code = "a_colitis", vigibase_version = "September 2024")
    Condition
      Error in `vigi_routine()`:
      ! `a_code` must be a named list.
      x `a_code` is of class character.
      > Supply a named list to `a_code`. See ?ex_.

# length one checker prints nicely [plain]

    Code
      error_length_one("d_code", "vigi_routine()", 2)
    Condition
      Error:
      ! `d_code` must have only one item in `vigi_routine()`.
      x `d_code` has 2 items.

# length one checker prints nicely [ansi]

    Code
      error_length_one("d_code", "vigi_routine()", 2)
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `d_code` must have only one item in `vigi_routine()`.
      [31mx[39m `d_code` has 2 items.

# length one checker prints nicely [unicode]

    Code
      error_length_one("d_code", "vigi_routine()", 2)
    Condition
      Error:
      ! `d_code` must have only one item in `vigi_routine()`.
      ✖ `d_code` has 2 items.

# length one checker prints nicely [fancy]

    Code
      error_length_one("d_code", "vigi_routine()", 2)
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `d_code` must have only one item in `vigi_routine()`.
      [31m✖[39m `d_code` has 2 items.

# export_to ends with proper extension and check svglite

    Code
      vigi_routine(demo_data = demo, drug_data = drug, adr_data = adr, link_data = link,
        d_code = d_drecno, a_code = a_llt, vigibase_version = "September 2024",
        export_to = "vigicaen_graph")
    Condition
      Error in `vigi_routine()`:
      ! `export_to` must end by '.bmp', '.eps', '.jpeg', '.pdf', '.png', '.ps''.svg', '.tex', '.tiff', or '.wmf' (windows only)

# data type checking prints nicely

    Code
      vigi_routine(demo_data = drug_, drug_data = drug_, adr_data = adr_, link_data = link_,
        d_code = d_drecno["nivolumab"], a_code = ex_$a_llt["a_colitis"], case_tto = 50,
        vigibase_version = "September 2024")
    Condition
      Error:
      ! object 'd_drecno' not found

---

    Code
      vigi_routine(demo_data = demo_, drug_data = demo_, adr_data = adr_, link_data = link_,
        d_code = d_drecno["nivolumab"], a_code = ex_$a_llt["a_colitis"], case_tto = 50,
        vigibase_version = "September 2024")
    Condition
      Error:
      ! object 'd_drecno' not found

---

    Code
      vigi_routine(demo_data = demo_, drug_data = drug_, adr_data = demo_, link_data = link_,
        d_code = d_drecno["nivolumab"], a_code = ex_$a_llt["a_colitis"], case_tto = 50,
        vigibase_version = "September 2024")
    Condition
      Error:
      ! object 'd_drecno' not found

---

    Code
      vigi_routine(demo_data = demo_, drug_data = drug_, adr_data = adr_, link_data = adr_,
        d_code = d_drecno["nivolumab"], a_code = ex_$a_llt["a_colitis"], case_tto = 50,
        vigibase_version = "September 2024")
    Condition
      Error:
      ! object 'd_drecno' not found

