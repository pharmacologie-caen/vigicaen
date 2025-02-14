# checkers of d_code and a_code work

    Code
      vigi_routine(demo_data = demo, drug_data = drug, adr_data = adr, link_data = link,
        d_code = "nivolumab", a_code = a_llt, vigibase_version = "September 2024")
    Condition
      Error in `vigi_routine()`:
      ! `d_code` is not a (named) list.
      i `data.frame`, `data.table`, `Array`, and `Table` are not allowed.
      > Supply a named list to `d_code`. See ?ex_.

---

    Code
      vigi_routine(demo_data = demo, drug_data = drug, adr_data = adr, link_data = link,
        d_code = d_drecno, a_code = "a_colitis", vigibase_version = "September 2024")
    Condition
      Error in `vigi_routine()`:
      ! `a_code` is not a (named) list.
      i `data.frame`, `data.table`, `Array`, and `Table` are not allowed.
      > Supply a named list to `a_code`. See ?ex_.

