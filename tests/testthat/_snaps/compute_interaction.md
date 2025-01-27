# vectorization works

    Code
      res_int
    Output
      # A tibble: 8 x 8
        y             x            z             n_obs n_exp    ic ic_tail ci_level
        <chr>         <chr>        <chr>         <dbl> <dbl> <dbl>   <dbl> <chr>   
      1 nivolumab     ipilimumab   a_colitis        18  7.88  1.14   0.400 95%     
      2 pembrolizumab ipilimumab   a_colitis         0  3.46 -2.99 -13.0   95%     
      3 nivolumab     atezolizumab a_colitis         0  0     0     -9.99  95%     
      4 pembrolizumab atezolizumab a_colitis         1  0     1.58  -2.21  95%     
      5 nivolumab     ipilimumab   a_pneumonitis    12  0     4.64   3.71  95%     
      6 pembrolizumab ipilimumab   a_pneumonitis     0  3.13 -2.86 -12.9   95%     
      7 nivolumab     atezolizumab a_pneumonitis     0  0     0     -9.99  95%     
      8 pembrolizumab atezolizumab a_pneumonitis     0  0     0     -9.99  95%     

# formatting output works

    Code
      res_int_raw
    Output
      # A tibble: 2 x 17
        y       x     z     n_obs n_exp    ic ic_tail ci_level n_xyz  n_xy  n_xz   n_x
        <chr>   <chr> <chr> <dbl> <dbl> <dbl>   <dbl> <chr>    <dbl> <dbl> <dbl> <dbl>
      1 nivolu~ ipil~ a_co~    18  7.88  1.14   0.400 95%         18    41     2    25
      2 nivolu~ ipil~ a_pn~    12  0     4.64   3.71  95%         12    47     0    27
      # i 5 more variables: n_yz <dbl>, n_y <dbl>, n_z <dbl>, n_none <dbl>,
      #   signif_ic <dbl>

---

    Code
      res_int_minn
    Output
      # A tibble: 2 x 8
        y         x          z             n_obs n_exp    ic ic_tail ci_level
        <chr>     <chr>      <chr>         <dbl> <dbl> <dbl>   <dbl> <chr>   
      1 nivolumab ipilimumab a_colitis        18  7.88  1.14   0.400 95%     
      2 nivolumab ipilimumab a_pneumonitis    12 NA    NA     NA     <NA>    

