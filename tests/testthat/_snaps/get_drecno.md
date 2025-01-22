# get drecno of single drug, or combination allowed

    Code
      d_drecno_res <- get_drecno(d_sel = d_sel_names, mp = mp_, allow_combination = FALSE,
        method = "drug_name", show_all = FALSE)
    Condition
      Warning:
      The `show_all` argument of `get_drecno()` is deprecated as of vigicaen 0.14.1.
      i Unmatching DrecNos or MedicinalProd_Ids will be shown by default.
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `nivolumab`: "nivolumab"
      > `ipilimumab`: "ipilimumab"
      > `nivo_ipi`: "nivolumab" and "ipilimumab"
      
      i Set `verbose` to FALSE to suppress this section.
      
      --------------------------------------------------------------------------------

---

    Code
      d_drecno_res_comb <- get_drecno(d_sel = d_sel_names, mp = mp_,
        allow_combination = TRUE, method = "drug_name", show_all = FALSE)
    Condition
      Warning:
      The `show_all` argument of `get_drecno()` is deprecated as of vigicaen 0.14.1.
      i Unmatching DrecNos or MedicinalProd_Ids will be shown by default.
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `nivolumab`: "nivolumab" and "ipilimumab;nivolumab"
      > `ipilimumab`: "ipilimumab" and "ipilimumab;nivolumab"
      > `nivo_ipi`: "nivolumab", "ipilimumab;nivolumab", and "ipilimumab"
      
      i Set `verbose` to FALSE to suppress this section.
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(list(a = "paracetamol"), allow_combination = TRUE, mp = mp_,
      verbose = TRUE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `a`: "chlorphenamine;guaifenesin;paracetamol;pseudoephedrine;tipepidine", "cinnamomum spp.;ephedra spp.;glycyrrhiza spp.;paeonia spp.;paracetamol;pueraria spp.;zingiber officinale;ziziphus spp.", "eperisone;paracetamol", "guaifenesin;paracetamol;pseudoephedrine", "caffeine;chlorphenamine;guaifenesin;methylephedrine;paracetamol;phenylephrine", "ascorbic acid;chlorphenamine;cloperastine;coptis spp.;lysozyme;methylephedrine;paracetamol;platycodon grandiflorus", "cetirizine;paracetamol;phenylephrine;zinc", "chlorphenamine;cloperastine;lysozyme;paracetamol", "allobarbital;codeine;paracetamol;salicylamide", "atropine;codeine;papaverine;paracetamol", "codeine;paracetamol;pentobarbital", "amobarbital;caffeine;codeine;paracetamol", "amobarbital;caffeine;codeine;octibenzonium bromide;paracetamol", "amobarbital;caffeine;codeine;dihydroergotamine;paracetamol", "caffeine;codeine;meprobamate;paracetamol", "codeine;meprobamate;paracetamol", "caffeine;codeine;paracetamol;phenazone;prothipendyl", "codeine;methaqualone;paracetamol", ..., "atropa bella-donna;caffeine;dihydrocodeine;diphenylpyraline;guaifenesin;methylephedrine;noscapine;paracetamol", and "ascorbic acid;caffeine;chlorphenamine;dextromethorphan;guaifenesin;herbal nos;paracetamol;pseudoephedrine"
      
      i Set `verbose` to FALSE to suppress this section.
      
      --------------------------------------------------------------------------------

# d_sel has inappropriate structure

    Code
      get_drecno(drug1, mp_, verbose = FALSE)
    Condition
      Error in `get_drecno()`:
      ! `d_sel` is not a list of character or numeric vectors.
      i Nested `lists` are not allowed.
      > Supply a named list of character or numeric vectors to `d_sel`. See ?ex_.

# show_all is deprecated

    Code
      r1 <- expect_warning(get_drecno(drug1, mp_, show_all = TRUE, verbose = FALSE),
      "deprecated")

# non WHO names raise appropriate warnings

    Code
      expect_warning(r1 <- get_drecno(drug1, mp_, verbose = FALSE),
      "Switch to .* WHO names.")
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs are not WHO name 
      
      ! Not WHO name | i WHO name
      
      * In `atra`:
        * ! "all-trans retinoic acid" | i tretinoin
      
      --------------------------------------------------------------------------------

---

    Code
      expect_warning(r1 <- get_drecno(drug2, mp_, verbose = FALSE),
      "Switch to .* WHO names.")
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs are not WHO name 
      
      ! Not WHO name | i WHO name
      
      * In `doli`:
        * ! "doliprane" | i paracetamol
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(drug3, mp_, verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs were not found 
      
      * In `medi`: x "medicament"
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(drug4, mp_, verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs were not found 
      
      * In `doli`: x "paracetam"
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(drug5, mp_, verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs were not found 
      
      * In `att`: x "antithrombin"
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(drug6, mp_, verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs were not found 
      
      * In `medi`: x "medicament" and "medicament2"
      * In `medi2`: x "autremedic"
      
      --------------------------------------------------------------------------------

---

    Code
      expect_warning(r1 <- get_drecno(drug7, mp_, allow_combination = TRUE, verbose = FALSE),
      "Switch to .* WHO names.")
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs are not WHO name 
      
      ! Not WHO name | i WHO name
      
      * In `notwho`:
        * ! "all-trans retinoic acid" | i tretinoin
        * ! "doliprane" | i paracetamol
      * In `notwho2`:
        * ! "doliprane" | i paracetamol
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(drug7, mp_, allow_combination = FALSE, verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs are not WHO name 
      
      ! Not WHO name | i WHO name
      
      * In `notwho`:
        * ! "all-trans retinoic acid" | i tretinoin
        * ! "doliprane" | i paracetamol
      * In `notwho2`:
        * ! "doliprane" | i paracetamol
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(drug8, mp_, verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Checking names --
      
      ! names of `d_sel` were lowered and trimed
      
      * `Medi` > `medi`
      
      -- `d_sel`: Matching drugs --
      
      -- ! Some drugs were not found 
      
      * In `medi`: x "medicament" and "medicament2"
      
      -- ! Some drugs are not WHO name 
      
      ! Not WHO name | i WHO name
      
      * In `mix`:
        * ! "all-trans retinoic acid" | i tretinoin
        * ! "renitec" | i enalaprilat
      * In `notwho2`:
        * ! "doliprane" | i paracetamol
      
    Condition
      Warning:
      You might have missed combinations.
      > Switch to i WHO names.
    Message
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(drug8, mp_, allow_combination = FALSE, verbose = TRUE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Checking names --
      
      ! names of `d_sel` were lowered and trimed
      
      * `Medi` > `medi`
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `medi`: "enalapril"
      > `mix`: "enalaprilat" and "enalapril"
      > `notwho2`: x No match
      > `who`: "nivolumab"
      
      i Set `verbose` to FALSE to suppress this section.
      
      -- ! Some drugs were not found 
      
      * In `medi`: x "medicament" and "medicament2"
      
      -- ! Some drugs are not WHO name 
      
      ! Not WHO name | i WHO name
      
      * In `mix`:
        * ! "all-trans retinoic acid" | i tretinoin
        * ! "renitec" | i enalaprilat
      * In `notwho2`:
        * ! "doliprane" | i paracetamol
      
      --------------------------------------------------------------------------------

---

    Code
      expect_warning(r1 <- get_drecno(drug8, mp_, allow_combination = TRUE, verbose = TRUE),
      "Switch to .* WHO names.")
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Checking names --
      
      ! names of `d_sel` were lowered and trimed
      
      * `Medi` > `medi`
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `medi`: "enalapril", "enalapril;folic acid", and "captopril;enalapril"
      > `mix`: "enalaprilat", "enalapril", "enalapril;folic acid", and "captopril;enalapril"
      > `notwho2`: x No match
      > `who`: "nivolumab" and "ipilimumab;nivolumab"
      
      i Set `verbose` to FALSE to suppress this section.
      
      -- ! Some drugs were not found 
      
      * In `medi`: x "medicament" and "medicament2"
      
      -- ! Some drugs are not WHO name 
      
      ! Not WHO name | i WHO name
      
      * In `mix`:
        * ! "all-trans retinoic acid" | i tretinoin
        * ! "renitec" | i enalaprilat
      * In `notwho2`:
        * ! "doliprane" | i paracetamol
      
      --------------------------------------------------------------------------------

# works for mpi_list as well

    Code
      r1 <- get_drecno(mpi, mp_, method = "mpi_list", allow_combination = FALSE,
        verbose = TRUE)
    Message
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `para`: "paracetamol"
      
      i Set `verbose` to FALSE to suppress this section.
      

# verbose works

    Code
      r_verbose_1 <- get_drecno(d_one, mp = mp_, method = "drug_name",
        allow_combination = FALSE, verbose = TRUE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `set1`: "enalapril"
      
      i Set `verbose` to FALSE to suppress this section.
      
      --------------------------------------------------------------------------------

---

    Code
      r_verbose_2 <- get_drecno(d_one, mp = mp_, method = "drug_name",
        allow_combination = TRUE, verbose = TRUE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `set1`: "nivolumab" and "ipilimumab;nivolumab"
      
      i Set `verbose` to FALSE to suppress this section.
      
      --------------------------------------------------------------------------------

---

    Code
      r_insp2 <- get_drecno(d_sel = d_min, mp = mp_, method = "drug_name",
        allow_combination = TRUE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `set1`: "ipilimumab", "ipilimumab;nivolumab", and "nivolumab"
      
      i Set `verbose` to FALSE to suppress this section.
      
      --------------------------------------------------------------------------------

---

    Code
      r_insp3 <- get_drecno(mpi, mp_, method = "mpi_list", verbose = TRUE,
        allow_combination = FALSE)
    Message
      
      -- `d_sel`: Matching drugs --
      
      -- v Matched drugs 
      
      > `set1`: "enalapril"
      
      i Set `verbose` to FALSE to suppress this section.
      

# inspect and show_all are deprecated

    Code
      r_inspect <- expect_warning(get_drecno(d_one, mp = mp_, method = "drug_name",
        allow_combination = FALSE, inspect = TRUE, verbose = FALSE), "deprecated")

---

    Code
      r_inspect <- expect_warning(get_drecno(d_one, mp = mp_, method = "drug_name",
        allow_combination = FALSE, show_all = TRUE, verbose = FALSE), "deprecated")

# names of d_sel were tolower-ed and trimed warning

    Code
      r1 <- get_drecno(d_sel = d_sel_names, mp = mp_, allow_combination = TRUE,
        method = "drug_name", verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Checking names --
      
      ! names of `d_sel` were lowered and trimed
      
      * `Nivolumab` > `nivolumab`
      * `Ipilimumab` > `ipilimumab`
      * `Nivo_ipi` > `nivo_ipi`
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(d_sel = d_sel_long, mp = mp_, allow_combination = TRUE,
        method = "drug_name", verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Checking names --
      
      ! names of `d_sel` were lowered and trimed
      
      * `Drug1` > `drug1`
      * `Drug2` > `drug2`
      * `Drug3` > `drug3`
      * `Drug4` > `drug4`
      * `Drug5` > `drug5`
      * ...
      * i Showing first 5 only
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_drecno(d_sel = d_sel_mix, mp = mp_, allow_combination = TRUE, method = "drug_name",
        verbose = FALSE)
    Message
      
      -- get_drecno() ----------------------------------------------------------------
      
      -- `d_sel`: Checking names --
      
      ! names of `d_sel` were lowered and trimed
      
      * `Drug2` > `drug2`
      
      --------------------------------------------------------------------------------

