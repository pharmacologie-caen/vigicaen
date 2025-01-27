## code to prepare `my_pkg_data` dataset goes here

path_ici_data <-
  "C:/Users/dolladille-c/vigibase_2023_sep/main/ici/"

demo <-
  dt_fst(path_ici_data, "demo")

drug <-     dt_fst(path_ici_data, "drug")
adr  <-     dt_fst(path_ici_data, "adr")
out  <-     dt_fst(path_ici_data, "out")
srce <-     dt_fst(path_ici_data, "srce")
link <-     dt_fst(path_ici_data, "link")
ind  <-     dt_fst(path_ici_data, "ind")
followup <- dt_fst(path_ici_data, "followup")

mp <- dt_fst("C:/Users/dolladille-c/vigibase_2023_sep/who/",
                   "mp")

path_meddra <-
  "C:/Users/dolladille-c/meddra_26_1/MedAscii/"

meddra <-
  dt_fst(
    path_meddra, "meddra_hierarchy"
  )

smq_list <- dt_fst(path_meddra, "meddra_smq_list")


smq_content <- dt_fst(path_meddra, "meddra_smq_content.fst")


smq_list_content <-
  smq_list %>%
  left_join(smq_content, by = "smq_code") %>%
  data.table

# #### change ex_ a_llt names #### ####

names(ex_$a_llt) <-
  c("a_embolism", "a_colitis", "a_pneumonitis")

# restrain meddra to a_colitis, a_pneumonitis and a_embolism case ids ---- ####

# and some smqs !

some_smqs <-
  list(hep = "Hepatitis, non-infectious (SMQ)",
       embol = "Embolic and thrombotic events, venous (SMQ)",
       sepsis1 = "Sepsis (SMQ)",
       sepsis2 = "Toxic-septic shock conditions (SMQ)",
       idm = "Myocardial infarction (SMQ)",
       cmi = "Other ischaemic heart disease (SMQ)",

       # those are for future use/debugging, since the 1st is a master smq of the 3 lasts
       cnsh_main = "Central nervous system haemorrhages and cerebrovascular conditions",

       cnsh_sub1 = "Conditions associated with central nervous system haemorrhages and cerebrovascular accidents (SMQ)",
       cnsh_sub1 = "Haemorrhagic central nervous system vascular conditions (SMQ)",
       cnsh_sub1 = "Ischaemic central nervous system vascular conditions (SMQ)",

       isch = "Ischaemic heart disease (SMQ)"
  )

smq_codes <-
  get_llt_smq(
    smq = some_smqs,
    smq_list_content = smq_list_content
  )

other_llt_codes <-
  get_llt_soc(
    term_sel = c(hep = "Hepatitis"),
    meddra = meddra,
    term_level = "pt"
  )

# reduce smq_list_content

smq_list_content_small <-
  smq_list_content %>%
  filter(smq_name %in%
           c(unlist(some_smqs),
             "Neuroleptic malignant syndrome (SMQ)"
           )
  ) %>%
  data.table() # had an internal error otherwise, problem with smq_name as an index...

meddra_small <-
  meddra %>%
  filter(
    llt_code %in%
      c(unlist(ex_$a_llt),
        unlist(some_smqs),
        # another term for test
        unlist(other_llt_codes))
  ) %>%
  mutate(across(where(is.factor),
                ~ factor(.x))) %>%
  data.table()

# restrain mp to ici drugs, and L03AA class and C01BB, paracetamol, and tramadol

ex_$d_drecno


ex_$d_groups <-
  rlang::list2(

    # Cosibelimab, # 2023-09-22 no cases, not found, same with drug code CK-301
    # Envafolimab, # 2023-09-22 no cases, not found, same with drug code kn035
    # Zalifrelimab # 2023-09-22 no cases, not found, same with drug code AGEN1884
    # prolgolimab, # 2023-10-05 no cases, not found. same with drug code L01FF08

    pd1 = c(
      "cemiplimab",
      "nivolumab",
      "pembrolizumab"

    ),
    pdl1 = c(
      "atezolizumab",
      "avelumab",

      "durvalumab"


    ),
    ctla4 = c(
      "ipilimumab",
      "tremelimumab"
    )

  )

ex_$d_groups_drecno <-
  get_drecno(
    ex_$d_groups,
    mp = mp
  )

# some drecno used for tests
other_drecno <-
  get_drecno(
    d_sel = list(para = "paracetamol",
                 tram = "tramadol",
                 atra = "tretinoin",
                 at3  = "antithrombin iii",
                 pc = "protein c (coagulation inhibitor)"
                 ),
    mp = mp
  )

atc_drecno <-
  get_atc_code(
    atc_sel = list(l03aa = "L03AA", c09aa = "C09AA", j01ca = "J01CA"),
    mp = mp,
    thg_data = thg_
  )

# L03AA Colony stimulating factors
# C09AA ACE inhibitors, plain
# J01CA Penicillins with extended spectrum

# also just a small distractor with class J01CA   ---- ####
atc_mpi <-
  get_atc_code(
    atc_sel = list(l03aa = "L03AA", c09aa = "C09AA", j01ca = "J01CA"),
    mp = mp,
    thg_data = thg_,
    vigilyze = FALSE
  )

mp_small <-
  mp %>%
  filter(
    DrecNo %in%
      c(unlist(ex_$d_drecno), unlist(atc_drecno), unlist(other_drecno))
  )


# restrain thg data  ---- ####

thg_small <-
  thg_ %>%
  filter(
    MedicinalProd_Id %in%
      unlist(atc_mpi)
  )

# identify a_colitis, a_pneumonitis and a_embolism cases  ---- ####
demo_test <-
  demo %>%
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr
  ) %>%
  add_drug(
    ex_$d_groups_drecno,
    drug_data = drug,
    data_type = "demo"
  ) %>%
  add_drug(
    atc_drecno,
    drug_data = drug,
    data_type = "demo"
  ) %>%
  add_drug(
    d_code = ex_$d_drecno %>%
      rlang::set_names(~ paste0("i_", .x)),
    drug_data = drug,
    repbasis = "i"
  ) %>%

  add_drug(
    ex_$d_drecno,
    drug_data = drug,
    data_type = "demo"
  )

# build a luda  ---- ####

tmp_d <-
  drug[, .(Drug_Id, UMCReportId, DrecNo)]

# adrs in link, step 1

tmp_a <-
  adr[, .(Adr_Id, MedDRA_Id)]

tmp_u <-
  demo[, .(UMCReportId, AgeGroup, Gender)]

luda_minimal <- # see #3
  link %>%
  left_join(tmp_d, by = "Drug_Id") %>%
  left_join(tmp_a, by = "Adr_Id") %>%
  left_join(tmp_u, by = "UMCReportId") %>%
  mutate(
    tto_mean = (TimeToOnsetMax + TimeToOnsetMin) / 2,
    range =    (TimeToOnsetMax + TimeToOnsetMin) / 2 - TimeToOnsetMin
  )

luda <-
  luda_minimal %>%
  add_drug(
    ex_$d_drecno,
    drug_data = drug,
    data_type = "link"
  ) %>%
  add_drug(
    atc_drecno,
    drug_data = drug,
    data_type = "link"
  ) %>%
  add_drug(
    ex_$d_groups_drecno,
    drug_data = drug,
    data_type = "link"
  ) %>%
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr,
    data_type = "link"
  )

# find some interesting cases for rechallenge  ---- ####

rch_pd1_colitis_r21 <-
  luda %>%
  filter(
    pd1 == 1 &
      a_colitis == 1 &
      Rechallenge2 %in% c("1")
  )

rch_pd1_colitis_r22 <-
  luda %>%
  filter(
    pd1 == 1 &
      a_colitis == 1 &
      Rechallenge2 %in% c("2")
  )

rch_pd1_pneumonitis_r21 <-
  luda %>%
  filter(
    pd1 == 1 &
      a_pneumonitis == 1 &
      Rechallenge2 %in% c("1")
  )

rch_pd1_pneumonitis_r22 <-
  luda %>%
  filter(
    pd1 == 1 &
      a_pneumonitis == 1 &
      Rechallenge2 %in% c("2")
  )

rch_pd1_colitis_tto <-
  extract_tto(
    luda_data = luda,
    adr_s = "a_colitis",
    drug_s = "pd1"
  )

rch_pd1_pneumonitis_tto <-
  extract_tto(
    luda_data = luda,
    adr_s = "a_pneumonitis",
    drug_s = "pd1"
  )

rch_umc <-
  list(
  d_ = list(rch_pd1_colitis_r21,
            rch_pd1_colitis_r22,
            rch_pd1_pneumonitis_r21,
            rch_pd1_pneumonitis_r22
            ),
  # those counts will help in tests
  n_ = c(25, 50, 33, 66)
) %>%
  purrr::pmap(
    function(d_, n_)
      d_ %>%
      pull(UMCReportId) %>%
      head(n = n_)
  ) %>%
  unlist()



# some cases with a dechallenge data  ---- ####

dch_pd1_colitis_d1 <-
  c("1", "2", "3", "4") %>%
  rlang::set_names(
    ~ paste0("d1_", .x)
  ) %>%
  purrr::map(
     function(d1)
      luda %>%
      filter(
        pd1 == 1 &
          a_colitis == 1 &
          Dechallenge1 %in% d1
      )
  )


dch_pd1_colitis_d2 <-
  c("1", "2", "3", "4") %>%
  rlang::set_names(
    ~ paste0("d2_", .x)
  ) %>%
  purrr::map(
    function(d2)
      luda %>%
      filter(
        pd1 == 1 &
          a_colitis == 1 &
          Dechallenge2 %in% d2
      )
  )

dch_pd1_pneumonitis_d1 <-
  c("1", "2", "3", "4") %>%
  rlang::set_names(
    ~ paste0("d1_", .x)
  ) %>%
  purrr::map(
    function(d1)
      luda %>%
      filter(
        pd1 == 1 &
          a_pneumonitis == 1 &
          Dechallenge1 %in% d1
      )
  )


dch_pd1_pneumonitis_d2 <-
  c("1", "2", "3", "4") %>%
  rlang::set_names(
    ~ paste0("d2_", .x)
  ) %>%
  purrr::map(
    function(d2)
      luda %>%
      filter(
        pd1 == 1 &
          a_pneumonitis == 1 &
          Dechallenge2 %in% d2
      )
  )

# ---- picking some of these cases  ---- ####

dch_umc <-
  list(
    d_ = list(dch_pd1_colitis_d1[[1]],
              dch_pd1_colitis_d1[[2]],
              dch_pd1_colitis_d1[[3]],
              dch_pd1_colitis_d1[[4]],
              dch_pd1_colitis_d2[[1]],
              dch_pd1_colitis_d2[[2]],
              dch_pd1_colitis_d2[[3]],
              dch_pd1_colitis_d2[[4]],

              dch_pd1_pneumonitis_d1[[1]],
              # dch_pd1_pneumonitis_d1[[2]], # mostly empty
              # dch_pd1_pneumonitis_d1[[3]],
              dch_pd1_pneumonitis_d1[[4]],

              dch_pd1_pneumonitis_d2[[1]],
              dch_pd1_pneumonitis_d2[[2]],
              dch_pd1_pneumonitis_d2[[3]]
              # dch_pd1_pneumonitis_d2[[4]]

    ),
    # those counts wont help in the end for tests... too bad
    n_ = c(10, 15, 5, 1,
           7, 3, 4, 6,

           2, # 9, 11,
           12,

           8, 13, 14
           # 15
           )
  ) %>%
  purrr::pmap(
    function(d_, n_)
      d_ %>%
      pull(UMCReportId) %>%
      head(n = n_)
  ) %>%
  unlist()

# ---- picking a few cases for other icis  ---- ####

demo_d <-
  list(
    d_ = names(ex_$d_drecno),
    n_ = c(51, 68, 43, 159, 220, 43, 26, 27)
    ) %>%
  purrr::pmap(
    function(d_, n_)
      demo_test %>%
      filter(.data[[d_]] == 1) %>%
      pull(UMCReportId) %>%
      head(n = n_)
  )

# ---- picking cases with an "interacting nivolumab" ---- ####

check_dm(demo_test, paste0("i_", names(ex_$d_drecno)))

i_umc <-
  c(
    demo_test %>%
      filter(i_nivolumab == 1) %>%
      pull(UMCReportId) %>%
      head(5),
    demo_test %>%
      filter(i_pembrolizumab == 1) %>%
      pull(UMCReportId) %>%
      head(5)
  )


# ---- filtering out those cases (overall) ---- ####
demo_small <-
  demo_test %>%
  filter(
    UMCReportId %in% c(rch_umc, dch_umc, unlist(demo_d), i_umc)
  )


demo_small %>% check_dm(cols = c(names(ex_$a_llt),
                                 names(ex_$d_drecno),
                                 names(atc_drecno)))


demo_small %>%
  compute_dispro(
    y = "a_colitis",
    x = "pd1"
  )

luda_small <-
  luda %>%
  filter(UMCReportId %in%
           demo_small[["UMCReportId"]])

luda_small %>%
  desc_dch(
    adr_s = c("a_colitis", "a_pneumonitis"),
    drug_s = c("pd1", "pdl1")
  )

luda_small %>%
  desc_rch(
    demo_data = demo_small,
    adr_s = c("a_colitis", "a_pneumonitis"),
    drug_s = c("pd1", "pdl1")
  )

luda_small %>%
  desc_tto(
    adr_s = c("a_colitis", "a_pneumonitis"),
    drug_s = c("pd1", "pdl1")
  )

# ---- filter other tables ---- ####

# umc filtered
drug_small      <-  drug     %>%
  filter(UMCReportId %in%
           demo_small[["UMCReportId"]])
adr_small       <-  adr      %>%
  filter(UMCReportId %in%
           demo_small[["UMCReportId"]])
out_small       <-  out      %>%
  filter(UMCReportId %in%
           demo_small[["UMCReportId"]])
srce_small      <-  srce     %>%
  filter(UMCReportId %in%
           demo_small[["UMCReportId"]])
followup_small  <-  followup %>%
  filter(UMCReportId %in%
           demo_small[["UMCReportId"]])

# drug_id filterd
link_small      <- link %>%
  filter(Drug_Id %in%
           drug_small[["Drug_Id"]])
ind_small       <- ind  %>%
  filter(Drug_Id %in%
           drug_small[["Drug_Id"]])

# ---- Age, sex
demo_small[, `:=`(
  age = cut(
    as.integer(AgeGroup),
    breaks = c(0, 5, 6, 7, 8),
    include.lowest = T,
    right = T,
    labels = c("<45", "45-64", "65-74", "75+")
  ),
  sexe = ifelse(Gender %in% c("-", "0", "9"), NA, Gender)
)]
# table(demo$age, demo$AgeGroup, useNA = "always")
# table(demo$Gender, demo$sexe,useNA = "always")

# ---- Report year
demo_small[, `:=`(year = as.integer(substr(DateDatabase, start=1, stop=4)))]

# ---- Death + disponibilite de l'outcome
demo_small[, `:=` (death = ifelse(UMCReportId %in%
                                    out_small[out_small$Seriousness == "1",UMCReportId],
                                  1,
                                  ifelse(UMCReportId %in%
                                           out_small[,UMCReportId],
                                         0,
                                         NA)))]

# ---- type of reporter ---- ####
demo_small %<>% left_join(srce_small, by = "UMCReportId") %>% data.table


demo_small %>%
  desc_facvar(
    c("age", "sexe", names(ex_$a_llt),
      names(ex_$d_drecno),
      names(atc_drecno))
  ) %>% print(n = 40)


demo_small %>%
  desc_facvar(
    c("year", "death", "Type.y"),
    ncat_max = 20
  )



# exporting to .rda files

demo_             <- demo %>%
  filter(UMCReportId %in% demo_small[["UMCReportId"]])
drug_             <- drug_small
adr_              <- adr_small
out_              <- out_small
srce_             <- srce_small
link_             <- link_small
ind_              <- ind_small
followup_         <- followup_small
thg_              <- thg_small
meddra_           <- meddra_small
mp_         <- mp_small
smq_list_content_ <- smq_list_content_small
luda_             <- luda_minimal %>%
  filter(UMCReportId %in% luda_small[["UMCReportId"]])

ex_$meddra   <- NULL
ex_$mp <- NULL
ex_$smq_list_content <- NULL
ex_$d_sel_names <- NULL

usethis::use_data(demo_            , compress = "xz", overwrite = TRUE)
usethis::use_data(drug_            , compress = "xz", overwrite = TRUE)
usethis::use_data(adr_             , compress = "xz", overwrite = TRUE)
usethis::use_data(out_             , compress = "xz", overwrite = TRUE)
usethis::use_data(srce_            , compress = "xz", overwrite = TRUE)
usethis::use_data(link_            , compress = "xz", overwrite = TRUE)
usethis::use_data(ind_             , compress = "xz", overwrite = TRUE)
usethis::use_data(followup_        , compress = "xz", overwrite = TRUE)
usethis::use_data(thg_             , compress = "xz", overwrite = TRUE)
usethis::use_data(meddra_          , compress = "xz", overwrite = TRUE)
usethis::use_data(mp_        , compress = "xz", overwrite = TRUE)
usethis::use_data(smq_list_content_, compress = "xz", overwrite = TRUE)
usethis::use_data(ex_              , compress = "xz", overwrite = TRUE)
usethis::use_data(luda_            , compress = "xz", overwrite = TRUE)
