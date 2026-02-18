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

  ind_valid <- data.frame(
    Drug_Id = 1, Indication = ""
  )

  meddra_valid <- data.frame(
    llt_code = 1, llt_name = 1, pt_name = 1,
    hlt_name = 1, hlgt_name = 1, soc_name = 1
  )

  data_invalid <-
    data.frame(
      UMCReportId = 1
    )

  r1 <-
    check_data_drug(drug_valid, ".data")

  r2 <-
    check_data_adr(adr_valid, ".data")

  r3 <-
    check_data_demo(demo_valid, ".data")

  r4 <-
    check_data_ind(ind_valid, ".data")

  expect_null(r1)
  expect_null(r2)
  expect_null(r3)
  expect_null(r4)

  expect_invisible(
    check_data_link(link_valid, ".data")
  )

  expect_invisible(
    check_data_meddra(meddra_valid, ".data")
  )

  purrr::map(
    list(drug_valid, link_valid, adr_valid, ind_valid),
    function(d_)
      expect_snapshot(
        error = TRUE, {
          check_data_demo(d_, ".data")
        }
      )
  )

  purrr::map(
    list(demo_valid, drug_valid, adr_valid, ind_valid),
    function(d_)
      expect_snapshot(
        error = TRUE, {
          check_data_link(d_, ".data")
        }
      )
  )

  purrr::map(
    list(demo_valid, drug_valid, adr_valid, link_valid),
    function(d_)
      expect_snapshot(
        error = TRUE, {
          check_data_ind(d_, ".data")
        }
      )
  )

  expect_snapshot(
    error = TRUE, {
      check_data_meddra(demo_valid, ".data")
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

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      check_data_meddra(data_invalid, arg = "x")
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

test_that("works with arrow::Table", {

  data_invalid <-
    data.frame(
      UMCReportId = 1
    )

 list(demo_, adr_, drug_, link_, meddra_, smq_list_) |>
    rlang::set_names("demo_", "adr_", "drug_", "link_", "meddra_", "smq_list_") |>
    purrr::map(arrow::as_arrow_table) |>
    purrr::discard_at("demo_") |>
    purrr::imap(
      function(d_, name)
      expect_snapshot(
        error = TRUE, {
          # arg paste0(name, "data") is deviated to make snapshot reading easier
          check_data_demo(d_ |> arrow::as_arrow_table(), paste0(name, "data"))
        }
      )
    )

  list(demo_, adr_, drug_, link_, meddra_, smq_list_) |>
    rlang::set_names("demo_", "adr_", "drug_", "link_", "meddra_", "smq_list_") |>
    purrr::map(arrow::as_arrow_table) |>
    purrr::discard_at("drug_") |>
    purrr::imap(
      function(d_, name)
        expect_snapshot(
          error = TRUE, {
            check_data_drug(d_ |> arrow::as_arrow_table(), paste0(name, "data"))
          }
        )
    )

  list(demo_, adr_, drug_, link_, meddra_, smq_list_) |>
    rlang::set_names("demo_", "adr_", "drug_", "link_", "meddra_", "smq_list_") |>
    purrr::map(arrow::as_arrow_table) |>
    purrr::discard_at("adr_") |>
    purrr::imap(
      function(d_, name)
        expect_snapshot(
          error = TRUE, {
            check_data_adr(d_ |> arrow::as_arrow_table(), paste0(name, "data"))
          }
        )
    )

  list(demo_, adr_, drug_, link_, meddra_, smq_list_) |>
    rlang::set_names("demo_", "adr_", "drug_", "link_", "meddra_", "smq_list_") |>
    purrr::map(arrow::as_arrow_table) |>
    purrr::discard_at("link_") |>
    purrr::imap(
      function(d_, name)
        expect_snapshot(
          error = TRUE, {
            check_data_link(d_ |> arrow::as_arrow_table(), paste0(name, "data"))
          }
        )
    )

  expect_invisible(check_data_demo(demo_ |> arrow::as_arrow_table(), ".data"))
  expect_invisible(check_data_drug(drug_ |> arrow::as_arrow_table(), ".data"))
  expect_invisible(check_data_adr(adr_ |> arrow::as_arrow_table(), ".data"))
  expect_invisible(check_data_link(link_ |> arrow::as_arrow_table(), ".data"))

  expect_invisible(check_data_meddra(meddra_ |> arrow::as_arrow_table(), ".data"))
  expect_invisible(check_data_smqlist(smq_list_ |> arrow::as_arrow_table(), ".data"))

})
