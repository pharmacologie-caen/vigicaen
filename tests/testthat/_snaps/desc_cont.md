# error columns not found prints nicely

    Code
      check_columns_in_data(d1, vf)
    Condition <columns_not_in_data>
      Error:
      ! `vf` columns must be in `.data`.
      x The followings were not found in `.data`: "v2".

---

    Code
      error_columns_in_data("vf", ".data", c("v2", "v3"))
    Condition <columns_not_in_data>
      Error:
      ! `vf` columns must be in `.data`.
      x The followings were not found in `.data`: "v2" and "v3".

# error columns not numeric/integer prints nicely

    Code
      check_columns_numeric_integer(d1, vf)
    Condition <columns_not_numeric_integer>
      Error:
      ! `vf` columns must be numeric or integer.
      x The following is not numeric/integer: "v2".

---

    Code
      check_columns_numeric_integer(d1, vf_cols)
    Condition <columns_not_numeric_integer>
      Error:
      ! `vf_cols` columns must be numeric or integer.
      x The following are not numeric/integer: "v2" and "v4".

---

    Code
      desc_cont(vc = c("v2"), .data = d1, format = "median (q1-q3)", dig = 0)
    Condition <columns_not_numeric_integer>
      Error in `desc_cont()`:
      ! `vc` columns must be numeric or integer.
      x The following is not numeric/integer: "v2".

