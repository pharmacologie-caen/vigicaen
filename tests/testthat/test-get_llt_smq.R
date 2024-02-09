test_that("there is no duplicate in extracted llts", {
  # Intended for the rechallenge data management

  llt_extraction_smq <-
    get_llt_smq(
      "Embolic and thrombotic events, venous (SMQ)",
      smq_list_content = smq_list_content_)

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
                smq_list_content = smq_list_content_)

  purrr::iwalk(adr_llt,
               function(p, p_n)
                 expect_equal(length(p), smq_res_length[[p_n]]))
})

test_that("omitting ' (SMQ)' is corrected", {
  smq_sel <- rlang::list2(
    embolism = "Embolic and thrombotic events, venous (SMQ)",
    embolism2 = "Embolic and thrombotic events, venous"
  )

  r1 <-
    get_llt_smq(smq_sel,
                smq_scope = "narrow",
                smq_list_content = smq_list_content_)

  expect_equal(r1[["embolism"]], r1[["embolism2"]])
})

test_that("errors and warnings pop as needed", {

  smq_sel <- rlang::list2(
    embolism = "Embolic and thrombotic events, venous"
  )

  s1 <- rlang::list2(
    wrong2 = "Ischaemic heart disease (SMQ)"
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

  expect_warning(
    get_llt_smq(s1,
                smq_list_content = smq_list_content_),
    "high level smq"
  )

  expect_error(
    get_llt_smq(s_algorithmic,
                smq_list_content = smq_list_content_),
    "algorithmic"
  )

  expect_warning(
    get_llt_smq(s2,
                smq_list_content = smq_list_content_),
    "the following elements were not found: ouuioui"
  )

  expect_warning(
    get_llt_smq(s3,
                smq_list_content = smq_list_content_),
    "the following elements were not found: ouuioui"
  )

})
