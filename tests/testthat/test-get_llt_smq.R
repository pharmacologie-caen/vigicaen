test_that("basic use works", {
  llt_extraction_smq <-
    get_llt_smq(
      list(a = "Embolic and thrombotic events, venous (SMQ)"),
      smq_list = smq_list_,
      smq_content = smq_content_)

  expect_equal(
    sum(llt_extraction_smq[[1]]
    ),
    26895551000
  )
})

test_that("there is no duplicates in extracted llts", {
  # Intended for the rechallenge data management

  llt_extraction_smq <-
    get_llt_smq(
      list(a = "Embolic and thrombotic events, venous (SMQ)"),
      smq_list = smq_list_,
      smq_content = smq_content_)

  expect_equal(length(llt_extraction_smq),
               length(unique(llt_extraction_smq)))
})

test_that("find the appropriate number of codes", {
  smq_sel <- rlang::list2(
      embolism = "Embolic and thrombotic events, venous (SMQ)"
     )


  smq_res_length <- rlang::list2(embolism = 348)

  adr_llt <-
    get_llt_smq(smq_sel,
                smq_scope = "narrow",
                smq_list = smq_list_,
                smq_content = smq_content_)

  purrr::iwalk(adr_llt,
               function(p, p_n)
                 expect_equal(length(p), smq_res_length[[p_n]]))
})


test_that("works with broad definitions", {

  # an smq with no broad terms (only narrow ones)
   smq_sel <- rlang::list2(
    embolism = "Embolic and thrombotic events, venous (SMQ)"
  )
  # an smq with broad terms
   smq_sel_2 <- rlang::list2(
     hepato = "Hepatitis, non-infectious (SMQ)"
   )

  smq_res_length <- rlang::list2(embolism = 348)

  smq_res2_length <- rlang::list2(hepato_narrow = 61,
                                  hepato_broad = 69)

  adr_llt <-
    get_llt_smq(smq_sel,
                smq_scope = "broad",
                smq_list = smq_list_,
                smq_content = smq_content_)

  purrr::iwalk(adr_llt,
               function(p, p_n)
                 expect_equal(length(p), smq_res_length[[p_n]]))

  adr_llt2_b <-
    get_llt_smq(smq_sel_2,
                smq_scope = "broad",
                smq_list = smq_list_,
                smq_content = smq_content_)

  adr_llt2_n <-
    get_llt_smq(smq_sel_2,
                smq_scope = "narrow",
                smq_list = smq_list_,
                smq_content = smq_content_)

  purrr::iwalk(adr_llt2_b,
               function(p, p_n)
                 expect_equal(length(p), smq_res2_length[["hepato_broad"]]))

  purrr::iwalk(adr_llt2_n,
               function(p, p_n)
                 expect_equal(length(p), smq_res2_length[["hepato_narrow"]]))
})


test_that("omitting ' (SMQ)' is corrected", {
  smq_sel <- rlang::list2(
    embolism = "Embolic and thrombotic events, venous (SMQ)",
    embolism2 = "Embolic and thrombotic events, venous"
  )

  r1 <-
    get_llt_smq(smq_sel,
                smq_scope = "narrow",
                smq_list = smq_list_,
                smq_content = smq_content_)

  expect_equal(r1[["embolism"]], r1[["embolism2"]])
})

test_that("errors and warnings pop as needed", {

  smq_sel <- rlang::list2(
    embolism = "Embolic and thrombotic events, venous"
  )

  s1 <- rlang::list2(
    high_level_smq = "Ischaemic heart disease (SMQ)"
  )

  s_algorithmic <- rlang::list2(
    algo = "Neuroleptic malignant syndrome (SMQ)"
  )

  s2 <- rlang::list2(
    wrong_alone = "ouuioui (SMQ)"
  )

  s3 <- rlang::list2(
    wrong_inassociationwithagoodone = "ouuioui (SMQ)",
    good = "Embolic and thrombotic events, venous (SMQ)"
  )

  expect_snapshot(
    r1 <- get_llt_smq(s1,
                smq_list = smq_list_,
                smq_content = smq_content_)
  )

  expect_error(
    get_llt_smq(s_algorithmic,
                smq_list = smq_list_,
                smq_content = smq_content_),
    "algorithmic"
  )

  expect_snapshot(
    r1 <- get_llt_smq(s2,
                smq_list = smq_list_,
                smq_content = smq_content_)
  )

  expect_snapshot(
   r2 <- get_llt_smq(s3,
                smq_list = smq_list_,
                smq_content = smq_content_)
  )

  expect_equal(
    r2$good,
    get_llt_smq(smq_sel,
                smq_list = smq_list_,
                smq_content = smq_content_)[[1]]
  )

})

test_that("works with multiple smqs in a single item", {
  smq_sel <-
    rlang::list2(
      sepsis = c("Sepsis (SMQ)","Toxic-septic shock conditions (SMQ)"),
      ischemic_heart_disease = c("Myocardial infarction (SMQ)", "Other ischaemic heart disease (SMQ)"),
    )

  smq_sel2 <-
    rlang::list2(
      sepsis = c("Sepsis (SMQ)","Toxic-septic shock conditions (SMQ)"),
      ischemic_heart_disease = c("Myocardial infarction (SMQ)"),
    )

  smq_sel_low <-
    rlang::list2(
      ischemic_heart_disease = c("Myocardial infarction (SMQ)")
    )

  smq_sel3 <-
    rlang::list2(
      sepsis = c("Sepsis (SMQ)","Toxic-septic shock conditions (SMQ)"),
      ischemic_heart_disease = c("Ischaemic heart disease (SMQ)"),
    )

  smq_sel4 <-
    rlang::list2(
      sepsis = c("Sepsis (SMQ)","Toxic-septic shock conditions (SMQ)"),
      ischemic_heart_disease = c("Ischaemic heart disease (SMQ)"),
      smq_failure = c("Not an SMQ")
    )

  adr_llt <-
    get_llt_smq(smq_sel,
                smq_scope =  "narrow",
                smq_list_,
                smq_content_)


  # no higher level smq found when querying lower ones

  expect_no_message(
    r1 <- get_llt_smq(smq_sel_low,
                smq_scope =  "narrow",
                smq_list_,
                smq_content_)
  )

  adr_llt2 <-
    get_llt_smq(smq_sel2,
                  smq_scope =  "narrow",
                  smq_list_,
                  smq_content_)

  # complex behaviors

  # mix of exact and sub smqs
  expect_snapshot(
    adr_llt3 <-
    get_llt_smq(smq_sel3,
                  smq_scope =  "narrow",
                  smq_list_,
                smq_content_)
  )

  # same with a non matching smq
  expect_snapshot(
    adr_llt4 <-
    get_llt_smq(smq_sel4,
                smq_scope =  "narrow",
                smq_list_,
                smq_content_)
  )

  true_ihd_length <-
    c(404, # for both Myocardial infarction (SMQ) and Other ischaemic heart disease (SMQ)
      188) # only for Myocardial infarction (SMQ)

  expect_gt(
    length(adr_llt$ischemic_heart_disease),
    length(adr_llt2$ischemic_heart_disease)
  )

  expect_equal(
    c(length(adr_llt$ischemic_heart_disease),
      length(adr_llt2$ischemic_heart_disease)),
    true_ihd_length
  )

  expect_equal(
    # no duplicates
    length(adr_llt$ischemic_heart_disease),
    length(adr_llt$ischemic_heart_disease |> unique())
  )


})

test_that("works with smq_list and smq_content as Table (out of memory)", {

  llt_extraction_smq <-
    get_llt_smq(list(a = "Embolic and thrombotic events, venous (SMQ)"),
                smq_list = smq_list_ |>
                  arrow::as_arrow_table(),
                smq_content = smq_content_ |>
                  arrow::as_arrow_table())

  expect_equal(sum(llt_extraction_smq[[1]]), 26895551000)

})

test_that("smq_list_content is deprecated", {
  expect_warning(
    r1 <- get_llt_smq(list(a = "Embolic and thrombotic events, venous (SMQ)"),
                smq_list = smq_list_,
                smq_content = smq_content_,
                smq_list_content = smq_list_content_),
    "deprecated"
  )

})

test_that("find_smq works", {


  # exact match
  smq_sel <- rlang::list2(
    embolism = "Embolic and thrombotic events, venous (SMQ)"
  )

  smq_true_code <-
    101948271 |>
    rlang::set_names("Embolic and thrombotic events, venous (SMQ)")

  smq_code <- find_smq(smq_sel[[1]], smq_list_)

  expect_equal(smq_code$match_exact, smq_true_code)

  # sub smqs match

  smq_sel2 <- rlang::list2(
    ihd = "Ischaemic heart disease (SMQ)"
  )

  smq_true_code2 <-
    list(match_exact = 32885138 |>
           rlang::set_names(
             "Ischaemic heart disease (SMQ)"
           ),
         match_sub =  c(70272710, 146302418) |>
           rlang::set_names(
             "Myocardial infarction (SMQ)",
             "Other ischaemic heart disease (SMQ)"
           ),
         match_failed = NULL
    )

  smq_code2 <- find_smq(smq_sel2[[1]], smq_list_)

  expect_equal(smq_code2, smq_true_code2)

  # algorithmic detection

  s_algorithmic <- rlang::list2(
    algo = "Neuroleptic malignant syndrome (SMQ)"
  )

  expect_error(
    find_smq(s_algorithmic[[1]], smq_list_),
    "algorithmic"
  )

  # failed match

  smq_nomatch <-
    find_smq("Not an SMQ", smq_list_)

  expect_equal(
    smq_nomatch$match_failed,
    "Not an SMQ"
  )

  expect_equal(
    smq_nomatch$match_exact,
    integer(0) |> rlang::set_names()
  )

  # invalid structure

  smq_invalid <-
    c("Embolic and thrombotic events, venous (SMQ)",
      "Embolic and thrombotic events, venous (SMQ)")

  expect_error(
    find_smq(smq_invalid, smq_list_),
    "smq.*structure is probably incorrect"
  )
})
