# vectorization works inside and outside the function

    Code
      demo <- add_adr(add_drug(demo_, d_code = ex_$d_drecno, drug_data = drug_),
      a_code = ex_$a_llt, adr_data = adr_)
    Message
      i `.data` detected as `demo` table.
      i `.data` detected as `demo` table.

# factors with levels other than 0 and 1 are rejected

    Code
      compute_dispro(demo, y = "a_colitis", x = "nivolumab_fac")
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `purrr::map()`:
      i In index: 1.
      Caused by error in `c_or_abcd_core()`:
      ! `nivolumab_fac` must be a factor with levels 0 and 1.
      x Levels found: 0, 1, and 2
      > Supply character vector(s), or factor(s) with levels 0 and 1 to `x`.

