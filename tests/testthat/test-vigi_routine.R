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
})

test_that("checkers of d_code and a_code work", {
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

  expect_error(
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno_toolong,
      a_code = a_llt,
      vigibase_version = "September 2024"
    ),
    "d_code must have only one item for this function."
  )

  expect_error(
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = a_llt_toolong,
      vigibase_version = "September 2024"
    ),
    "a_code must have only one item for this function."
  )

  # not lists

  expect_error(
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = "nivolumab",
      a_code = a_llt,
      vigibase_version = "September 2024"
    ),
    "d_code must be a named list"
  )

  expect_error(
    vigi_routine(
      demo_data = demo,
      drug_data = drug,
      adr_data  = adr,
      link_data = link,
      d_code = d_drecno,
      a_code = "a_colitis",
      vigibase_version = "September 2024"
    ),
    "a_code must be a named list"
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
    "export_to must end by"
  )

  # insufficient checker

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

test_that("patient label is left or right justified, depending on tto median", {
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
    "case_tto below median",
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

})
