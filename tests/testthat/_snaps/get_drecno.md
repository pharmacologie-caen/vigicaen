# get drecno of a single drug, no combination allowed

    Code
      d_drecno_res <- get_drecno(d_sel = d_sel_names, mp = mp_, allow_combination = FALSE,
        method = "drug_name", show_all = FALSE)
    Message
      i Matching drugs in `d_sel`
      > `nivolumab`: nivolumab
      > `ipilimumab`: ipilimumab
      > `nivo_ipi`: nivolumab and ipilimumab
      i Set `verbose` to FALSE to suppress this message.

---

    Code
      d_drecno_res_comb <- get_drecno(d_sel = d_sel_names, mp = mp_,
        allow_combination = TRUE, method = "drug_name", show_all = FALSE)
    Message
      i Matching drugs in `d_sel`
      > `nivolumab`: nivolumab and ipilimumab;nivolumab
      > `ipilimumab`: ipilimumab and ipilimumab;nivolumab
      > `nivo_ipi`: nivolumab, ipilimumab;nivolumab, and ipilimumab
      i Set `verbose` to FALSE to suppress this message.

# verbose works

    Code
      r_verbose_1 <- get_drecno(d_one, mp = mp_, method = "drug_name",
        allow_combination = FALSE, verbose = TRUE)
    Message
      i Matching drugs in `d_sel`
      > `set1`: paracetamol
      i Set `verbose` to FALSE to suppress this message.

---

    Code
      r_verbose_2 <- get_drecno(d_one, mp = mp_, method = "drug_name",
        allow_combination = TRUE, verbose = TRUE)
    Message
      i Matching drugs in `d_sel`
      > `set1`: nivolumab and ipilimumab;nivolumab
      i Set `verbose` to FALSE to suppress this message.

---

    Code
      r_insp2 <- get_drecno(d_sel = d_min, mp = mp_, method = "drug_name",
        allow_combination = TRUE)
    Message
      i Matching drugs in `d_sel`
      > `set1`: ipilimumab, ipilimumab;nivolumab, and nivolumab
      i Set `verbose` to FALSE to suppress this message.

---

    Code
      r_insp3 <- get_drecno(mpi, mp_, method = "mpi_list", show_all = FALSE, verbose = TRUE,
        allow_combination = FALSE)
    Message
      i Matching drugs in `d_sel`
      > `set1`: paracetamol
      i Set `verbose` to FALSE to suppress this message.

# inspect is deprecated

    Code
      r_inspect <- expect_warning(get_drecno(d_one, mp = mp_, method = "drug_name",
        allow_combination = FALSE, inspect = TRUE, verbose = FALSE), "deprecated")

