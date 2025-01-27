test_that("cli format and basic use work", {

  drug_valid <- data.frame(
    DrecNo = 1, UMCReportId = 1, MedicinalProd_Id = 1, Drug_Id = 1)

  demo_valid <- data.frame(
    UMCReportId = 1, Type = 1, Region = 1, DateDatabase = 1
  )

  adr_valid <- data.frame(
    UMCReportId = 1, Adr_Id = 1, MedDRA_Id = 1, Outcome = 1
  )

  link_valid <- data.frame(
    UMCReportId = 1, Drug_Id = 1, Adr_Id = 1,
    Dechallenge1 = 1, TimeToOnsetMin = 1,
    tto_mean = 1,
    range = 1
  )

  data_invalid <-
    data.frame(
      UMCReportId = 1
    )

  r1 <-
    check_data_drug(drug_valid, ".data")

  r2 <-
    check_data_adr(adr_valid, ".data")

  expect_null(r1)
  expect_null(r2)

  expect_invisible(
    check_data_link(link_valid, ".data")
  )

  expect_snapshot(
    error = TRUE, {
      check_data_link(drug_valid, ".data")
    }
  )

  expect_snapshot(
    error = TRUE, {
      check_data_link(adr_valid, ".data")
    }
  )

  expect_snapshot(
    error = TRUE, {
      check_data_link(demo_valid, ".data")
    }
  )


  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      check_data_drug(data_invalid, arg = "x")
    })
  })

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      check_data_adr(data_invalid, arg = "x")
    })
  })

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      check_data_link(data_invalid, arg = "x")
    })
  })


})

test_that("smq_list is distinguished of smq_list_content", {

  smq_list <- list(
    smq_name = 1,
    smq_level = 1,
    smq_description = 1,
    smq_source = 1,
    smq_note = 1,
    MedDRA_version = 1,
    status = 1,
    smq_algorithm = 1
  )

  smq_list_content <- list(
    smq_name = 1,
    smq_level = 1,
    smq_description = 1,
    smq_source = 1,
    smq_note = 1,
    MedDRA_version = 1,
    status = 1,
    smq_algorithm = 1,
    term_code = 1,
    term_level = 1,
    term_scope = 1,
    term_category = 1,
    term_weight = 1,
    term_status = 1,
    term_addition_version = 1,
    term_last_modified_version = 1
  )

  r1 <-
    vigicaen:::check_data_smqlist(smq_list, ".data")



  expect_null(r1)
  expect_snapshot(error = TRUE, {
    vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
  })

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      vigicaen:::check_data_smqlist(smq_list_content, arg = "x")
    })
  })
})
