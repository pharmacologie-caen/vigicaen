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
    Dechallenge1 = 1, TimeToOnsetMin = 1
  )

  data_invalid <-
    data.frame(
      UMCReportId = 1
    )

  r1 <-
    vigicaen:::check_data_drug(drug_valid, ".data")

  r2 <-
    vigicaen:::check_data_adr(adr_valid, ".data")

  expect_null(r1)
  expect_null(r2)


  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      vigicaen:::check_data_drug(data_invalid, arg = "x")
    })
  })

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      vigicaen:::check_data_adr(data_invalid, arg = "x")
    })
  })

})
