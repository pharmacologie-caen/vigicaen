# doesnt work with continuous columns

    Code
      desc_facvar(vf = c("age"), .data = df, format = "n_/N_ (pc_%)", dig = 0,
      pad_width = 0, ncat_max = 3)
    Condition
      Error in `desc_facvar()`:
      ! Too many levels detected in: age
      x Number of levels: 7 exceeded `ncat_max`(3)
      i Did you pass a continuous variable to `desc_facvar()`?
      > Set `ncat_max` to suppress this error.

