test_that("cli formatting", {
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
    Dechallenge1 = 1, TimeToOnsetMin = 1
  )

  invalid_data <- data.frame(
    UMCReportId = 1
  )

  expect_snapshot(error = TRUE, {
    vigicaen:::query_data_type(invalid_data, ".data")
  })

  cli::test_that_cli("format is ok", {
    dtype <- expect_snapshot(vigicaen:::query_data_type(drug_valid, ".data"))
  })

  expect_snapshot({
    dtype <-
      purrr::map(list(drug_valid, demo_valid, adr_valid, link_valid),
          function(data_)
      vigicaen:::query_data_type(data_, ".data")
      )
    })

  expect_equal(dtype, list("drug", "demo", "adr", "link"))
})
