# By default, if vdiffr is not installed, all visual tests are skipped unless
# VDIFFR_RUN_TESTS is explicitly set to "true", which should be the case only on
# a GitHub Actions CI runner with stable version of R.

if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion('testthat') >= '3.0.3' && utils::packageVersion('base') >= '4.4.1') {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # If vdiffr is not available and visual tests are explicitly required, raise error.
  if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {
    abort("vdiffr is not installed")
  }

  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}

# from https://github.com/tidyverse/ggplot2/blob/ddd207e926cc1c1847dc661d9a099b8ec19c4010/tests/testthat/helper-vdiffr.R#L1-L15

test_that("standard use works", {
  d_drecno <-
    ex_$d_drecno["nivolumab"]

  a_llt <-
    ex_$a_llt["a_colitis"]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_

  # run routine

  expect_doppelganger(
    "Base graphic",
   vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
  )

 expect_doppelganger(
    "Case time to onset",
    vigi_routine(
     case_tto = 150,
     demo_data = demo,
     drug_data = drug,
     adr_data  = adr,
     link_data = link,
     d_code = d_drecno,
     a_code = a_llt,
     vigibase_version = "September 2024"
   )
  )

# Additional customization with d_name and a_name args

  expect_doppelganger(
    "Custom drug and adr labels",
   vigi_routine(
     case_tto = 150,
     demo_data = demo,
     drug_data = drug,
     adr_data  = adr,
     link_data = link,
     d_code = d_drecno,
     a_code = a_llt,
     vigibase_version = "September 2024",
     d_label = "Nivolumab",
     a_label = "Colitis"
   )
  )

  tmp_folder <- tempdir()

  path_vigiroutine_test <-
    paste0(tmp_folder, "/", "vigiroutine_test", "/")

  if (!dir.exists(path_vigiroutine_test))
  {
    dir.create(path_vigiroutine_test)
  }

  expect_message(
    expect_doppelganger(
      "Exporting",

      vigi_routine(
       case_tto = 150,
       demo_data = demo,
       drug_data = drug,
       adr_data  = adr,
       link_data = link,
       d_code = d_drecno,
       a_code = a_llt,
       vigibase_version = "September 2024",
       d_label = "Nivolumab",
       a_label = "Colitis",
       export_to = paste0(path_vigiroutine_test,
                          "vigicaen_graph.png"
                          )
       )
      ),
    "Plot exported to .*vigicaen_graph.png"
    )

  expect_equal(
    file.exists(paste0(path_vigiroutine_test, "vigicaen_graph.png")),
    TRUE
  )

  expect_message(
    expect_doppelganger(
      "Exporting",

      vigi_routine(
        case_tto = 150,
        demo_data = arrow::as_arrow_table(demo),
        drug_data = arrow::as_arrow_table(drug),
        adr_data  = arrow::as_arrow_table(adr),
        link_data = arrow::as_arrow_table(link),
        d_code = d_drecno,
        a_code = a_llt,
        vigibase_version = "September 2024",
        d_label = "Nivolumab",
        a_label = "Colitis",
        export_to = paste0(path_vigiroutine_test,
                           "vigicaen_graph2.png"
        )
      )
    ),
    "Plot exported to .*vigicaen_graph2.png"
  )

  expect_equal(
    file.exists(paste0(path_vigiroutine_test, "vigicaen_graph2.png")),
    TRUE
  )

  unlink(path_vigiroutine_test, recursive = TRUE)
})

test_that("checkers of d_code and a_code work", {
  cnd <- rlang::catch_cnd(error_length_one("d_code", "vigi_routine()", 2))

  expect_s3_class(
    cnd, "length_one"
  )

  expect_equal(cnd$arg, "d_code")
  expect_equal(cnd$fn, "vigi_routine()")
  expect_equal(cnd$wrong_length, 2)

  d_drecno_toolong <-
    ex_$d_drecno[c("nivolumab", "ipilimumab")]

  d_drecno <-
    ex_$d_drecno[c("nivolumab")]


  a_llt <-
    ex_$a_llt["a_colitis"]

  a_llt_toolong <-
    ex_$a_llt[c("a_colitis", "a_pneumonitis")]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_

  expect_snapshot(error = TRUE, cnd_class = TRUE,
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno_toolong,
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
  )

  cnd_vr1 <- rlang::catch_cnd(
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno_toolong,
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
    )

  expect_s3_class(cnd_vr1, "length_one")
  expect_equal(cnd_vr1$arg, "d_code")
  expect_equal(cnd_vr1$fn, "vigi_routine()")

  expect_snapshot(error = TRUE, cnd_class = TRUE,
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt_toolong,
      vigibase_version = "September 2024"
    )
  )

  cnd_vr2 <- rlang::catch_cnd(
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt_toolong,
      vigibase_version = "September 2024"
    )
  )

  expect_s3_class(cnd_vr2, "length_one")
  expect_equal(cnd_vr2$arg, "a_code")
  expect_equal(cnd_vr2$fn, "vigi_routine()")

  # not lists

  expect_snapshot(
    error = TRUE,
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = "nivolumab",
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
  )

  expect_snapshot(
    error = TRUE,
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = "a_colitis",
      vigibase_version = "September 2024"
    )
  )

})

cli::test_that_cli("length one checker prints nicely",{
  expect_snapshot(
    error = TRUE,
    error_length_one("d_code", "vigi_routine()", 2)
  )
})

test_that("export_to ends with proper extension and check svglite", {
  d_drecno <-
    ex_$d_drecno["nivolumab"]

  a_llt <-
    ex_$a_llt["a_colitis"]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_

  expect_error(
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt,
      vigibase_version = "September 2024",
      export_to = "vigicaen_graph"
      ),
    regexp = "export_to.*must end by"
  )

  expect_snapshot(
    error = TRUE,
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt,
      vigibase_version = "September 2024",
      export_to = "vigicaen_graph"
    )
  )
})

test_that("formatting IC025 with out of bound value works", {
  d_drecno <-
    ex_$d_drecno["pembrolizumab"]

  a_llt <-
    ex_$a_llt["a_colitis"]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_

  # run routine

  expect_doppelganger(
    "ic025 below 0",
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
  )
})

test_that("patient label is left or right justified, depending on 90 days threshold", {
  d_drecno <-
    ex_$d_drecno["nivolumab"]

  a_llt <-
    ex_$a_llt["a_colitis"]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_

  # run routine

  expect_doppelganger(
    "case_tto below 90 days",
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt,
      case_tto = 30,
      vigibase_version = "September 2024"
    )
  )

  expect_doppelganger(
    "case_tto above 90 days",
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt,
      case_tto = 150,
      vigibase_version = "September 2024"
    )
  )
})

test_that("too few time to onset prevents graph drawing", {
  d_drecno <-
    ex_$d_drecno["ipilimumab"]

  a_llt <-
    ex_$a_llt["a_embolism"]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_

  # run routine

  expect_message(
    expect_doppelganger(
      "no time to onset",
      vigi_routine(
        demo_data = demo,
        drug_data = drug,
        adr_data  = adr,
        link_data = link,
        d_code = d_drecno,
        a_code = a_llt,
        vigibase_version = "September 2024"
        )
      ),
      "Not enough data to plot time to onset"
  )

  expect_message(
    expect_doppelganger(
      "no time to onset",
      vigi_routine(
        demo_data = arrow::as_arrow_table(demo),
        drug_data = arrow::as_arrow_table(drug),
        adr_data  = arrow::as_arrow_table(adr),
        link_data = arrow::as_arrow_table(link),
        d_code = d_drecno,
        a_code = a_llt,
        vigibase_version = "September 2024"
      )
    ),
    "Not enough data to plot time to onset"
  )

  # export is smaller

  tmp_folder <- tempdir()

  path_vigiroutine_test2 <-
    paste0(tmp_folder, "/", "vigiroutine_test2", "/")

  if (!dir.exists(path_vigiroutine_test2))
  {
    dir.create(path_vigiroutine_test2)
  }


  expect_message(expect_message(
    expect_doppelganger(
      "no time to onset export",
      vigi_routine(
        case_tto = 150,
        demo_data = demo,
        drug_data = drug,
        adr_data  = adr,
        link_data = link,
        d_code = d_drecno,
        a_code = a_llt,
        vigibase_version = "September 2024",
        export_to = paste0(path_vigiroutine_test2,
                           "vigicaen_graph.png"
        )
      )
      ),
    "Plot exported to .*vigicaen_graph.png"
    ),
    "Not enough data to plot time to onset"
    )

  expect_equal(
    file.exists(paste0(path_vigiroutine_test2, "vigicaen_graph.png")),
    TRUE
  )

  suppressMessages(link_colitis <-
    link |>
    add_adr(
      a_code = ex_$a_llt,
      adr_data = adr_
    ) |>
    add_drug(
      d_code = ex_$d_drecno,
      drug_data = drug_
    )
  )

  one_colitis <-
    link_colitis |>
    dplyr::filter(a_colitis == 1 & atezolizumab == 1 & !is.na(tto_mean)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::select(UMCReportId, Drug_Id, Adr_Id)

  demo <- demo_ |>
    dplyr::filter(UMCReportId %in% one_colitis$UMCReportId)
  adr  <- adr_  |>
    dplyr::filter(Adr_Id %in% one_colitis$Adr_Id)
  drug <- drug_ |>
    dplyr::filter(Drug_Id %in% one_colitis$Drug_Id)
  link <- link_ |>
    dplyr::filter(UMCReportId %in% one_colitis$UMCReportId)

  d_drecno <-
    ex_$d_drecno["atezolizumab"]

  # not enough cases

  expect_message(
    expect_doppelganger(
      "no cases",
      vigi_routine(
        demo_data = demo,
        drug_data = drug,
        adr_data  = adr,
        link_data = link,
        d_code = ex_$d_drecno["atezolizumab"],
        a_code = ex_$a_llt["a_colitis"],
        vigibase_version = "September 2024"
      )
    ),
    "Not enough data to plot time to onset"
  )
})

test_that("error if no adr or drug cases found", {
  # zero drug cases, zero adr cases

  d_drecno_empty <-
    list(d1 = integer())

  a_llt_empty <-
    list(a1 = integer())

  err <- rlang::catch_cnd(vigi_routine(
    demo_data = demo_,
    drug_data = drug_,
    adr_data  = adr_,
    link_data = link_,
    d_code = d_drecno_empty,
    a_code = ex_$a_llt["a_colitis"],
    case_tto = 50,
    vigibase_version = "September 2024"
  ))

  err_arrow <- rlang::catch_cnd(vigi_routine(
    demo_data = arrow::as_arrow_table(demo_),
    drug_data = arrow::as_arrow_table(drug_),
    adr_data  = arrow::as_arrow_table(adr_),
    link_data = arrow::as_arrow_table(link_),
    d_code = d_drecno_empty,
    a_code = ex_$a_llt["a_colitis"],
    case_tto = 50,
    vigibase_version = "September 2024"
  ))

  expect_s3_class(err, "no_cases")
  expect_equal(err$arg, "d1")
  expect_equal(err$arg_type, "drug")
  expect_equal(err$dataset, "demo_data")

  expect_equal(err[c("arg", "message", "class", "arg_type", "dataset")],
               err_arrow[c("arg", "message", "class", "arg_type", "dataset")])

  expect_error(
    vigi_routine(
      demo_data = demo_,
      drug_data = drug_,
      adr_data  = adr_,
      link_data = link_,
      d_code = d_drecno_empty,
      a_code = ex_$a_llt["a_colitis"],
      case_tto = 50,
      vigibase_version = "September 2024"
    ),
    class = "no_cases"
  )

  err2 <- rlang::catch_cnd(vigi_routine(
    demo_data = demo_,
    drug_data = drug_,
    adr_data  = adr_,
    link_data = link_,
    d_code = ex_$d_drecno["nivolumab"],
    a_code = a_llt_empty,
    case_tto = 50,
    vigibase_version = "September 2024"
  ))

  err2_arrow <- rlang::catch_cnd(vigi_routine(
    demo_data = arrow::as_arrow_table(demo_),
    drug_data = arrow::as_arrow_table(drug_),
    adr_data  = arrow::as_arrow_table(adr_),
    link_data = arrow::as_arrow_table(link_),
    d_code = ex_$d_drecno["nivolumab"],
    a_code = a_llt_empty,
    case_tto = 50,
    vigibase_version = "September 2024"
  ))

  expect_s3_class(err2, "no_cases")
  expect_equal(err2$arg, "a1")
  expect_equal(err2$arg_type, "adr")
  expect_equal(err2$dataset, "demo_data")

  expect_equal(err2[c("arg", "message", "class", "arg_type", "dataset")],
               err2_arrow[c("arg", "message", "class", "arg_type", "dataset")])

  expect_error(
    vigi_routine(
      demo_data = demo_,
      drug_data = drug_,
      adr_data  = adr_,
      link_data = link_,
      d_code = ex_$d_drecno["nivolumab"],
      a_code = a_llt_empty,
      case_tto = 50,
      vigibase_version = "September 2024"
    ),
    class = "no_cases"
  )

})

test_that("works with arrow tables", {
  d_drecno <-
    ex_$d_drecno["nivolumab"]

  a_llt <-
    ex_$a_llt["a_colitis"]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_

  # run routine

  expect_doppelganger(
    "arrow table",
    vigi_routine(
      demo_data = arrow::as_arrow_table(demo),
      drug_data = arrow::as_arrow_table(drug),
      adr_data  = arrow::as_arrow_table(adr),
      link_data = arrow::as_arrow_table(link),
      d_code = d_drecno,
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
  )
})

test_that("data type checking prints nicely", {
  expect_snapshot(
    error = TRUE,
    vigi_routine(
      demo_data = drug_, # wrong
      drug_data = drug_,
      adr_data  = adr_,
      link_data = link_,
      d_code = d_drecno["nivolumab"],
      a_code = ex_$a_llt["a_colitis"],
      case_tto = 50,
      vigibase_version = "September 2024"
    )
  )

  expect_snapshot(
    error = TRUE,
    vigi_routine(
      demo_data = demo_,
      drug_data = demo_, # wrong
      adr_data  = adr_,
      link_data = link_,
      d_code = d_drecno["nivolumab"],
      a_code = ex_$a_llt["a_colitis"],
      case_tto = 50,
      vigibase_version = "September 2024"
    )
  )

  expect_snapshot(
    error = TRUE,
    vigi_routine(
      demo_data = demo_,
      drug_data = drug_,
      adr_data  = demo_, # wrong
      link_data = link_,
      d_code = d_drecno["nivolumab"],
      a_code = ex_$a_llt["a_colitis"],
      case_tto = 50,
      vigibase_version = "September 2024"
    )
  )

  expect_snapshot(
    error = TRUE,
    vigi_routine(
      demo_data = demo_,
      drug_data = drug_,
      adr_data  = adr_,
      link_data = adr_, # wrong
      d_code = d_drecno["nivolumab"],
      a_code = ex_$a_llt["a_colitis"],
      case_tto = 50,
      vigibase_version = "September 2024"
    )
  )


})

test_that("absence of rechallenge data correctly displayed", {
  d_drecno <-
    ex_$d_drecno["nivolumab"]

  a_llt <-
    ex_$a_llt["a_colitis"]

  demo <- demo_
  adr  <- adr_
  drug <- drug_
  link <- link_ |>
    dplyr::filter(!Rechallenge2 %in% c("1", "2"))

  expect_doppelganger(
    "no rechallenge",
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
  )

  expect_doppelganger(
    "no rechallenge",
    vigi_routine(
      demo_data = arrow::as_arrow_table(demo),
      drug_data = arrow::as_arrow_table(drug),
      adr_data  = arrow::as_arrow_table(adr),
      link_data = arrow::as_arrow_table(link),
      d_code = d_drecno,
      a_code = a_llt,
      vigibase_version = "September 2024"
    )
  )

})
