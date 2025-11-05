test_that(".data type is correctly detected and there is no other msg", {
  i_list <-
    list(
      melanoma = c("Malignant melanoma", "Metastatic malignant melanoma"),
      lung_cancer = c("Non-small cell lung cancer", "Lung adenocarcinoma")
    )

  expect_snapshot({
    dtype <-
      purrr::map(list(drug_, demo_, adr_, link_, ind_),
                 function(data_)
                   data_ |> add_ind(i_list,
                                    drug_data = drug_,
                                    ind_data  = ind_)
      )
  })

})

test_that("modal case works", {
  i_list <-
    list(
      melanoma = c("Malignant melanoma", "Metastatic malignant melanoma"),
      lung_cancer = c("Non-small cell lung cancer", "Lung adenocarcinoma")
      )

  n_ind <- length(i_list)


  i_count <- rlang::list2(
    melanoma    = 86,
    lung_cancer = 78
  )

  suppressMessages(
    demo_n <-
     demo_ |>
     add_ind(i_list,
             drug_data = drug_,
             ind_data  = ind_)
  )

  suppressMessages(
    demo_a <-
      demo_ |>
      arrow::as_arrow_table() |>
      add_ind(i_list,
              drug_data = drug_ |> arrow::as_arrow_table(),
              ind_data  = ind_ |> arrow::as_arrow_table()
              ) |>
      dplyr::collect()
  )

  expect_equal(ncol(demo_n),
               ncol(demo_) + n_ind)

  expect_equal(ncol(demo_a),
               ncol(demo_) + n_ind)



  purrr::iwalk(
    i_count,
    function(d_n, id_n){
      expect_equal(sum(demo_n[[id_n]], na.rm = TRUE),
                   expected = d_n)
    }
  )

  # note, there could not be any missing data across
  # new columns if updating ind

  suppressMessages(
    ind_n <-
      ind_ |>
      add_ind(i_list, drug_data = drug_, ind_data  = ind_)
  )

  purrr::iwalk(
    i_count,
    function(d_n, id_n){
      expect_equal(sum(is.na(ind_n[[id_n]])),
                   0)
    }
  )

})


test_that("missing data from ind are handled", {
  i_list <-
    list(diabetes = c("Diabetes mellitus", "Type 2 diabetes mellitus"))

  ind_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3"),
      # no ind for d4_ici3 and so on
      Indication = c("Diabetes mellitus",
                     "Type 2 diabetes mellitus",
                     "Another indication")
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA
    )

  suppressMessages(
    demo_res <-
      demo_test |>
      add_ind(
        i_list,
        drug_data = drug_test,
        ind_data  = ind_test
      )
  )

  demo_true <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,
      diabetes = c(1, 0, NA, NA, NA)
    )

  expect_equal(demo_res,
               demo_true)
})

test_that("works with drug level tables", {
  i_list <-
    list(diabetes = c("Diabetes mellitus", "Type 2 diabetes mellitus"))

  ind_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3"),
      # no ind for d4_ici3 and so on
      Indication = c("Diabetes mellitus",
                     "Type 2 diabetes mellitus",
                     "Another indication")
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  link_test <-
    data.table(
      Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3),
      Dechallenge1 = NA,
      TimeToOnsetMin = NA
    )

  suppressMessages(
    drug_res <-
      drug_test |>
      add_ind(
        i_list,
        drug_data = drug_test,
        ind_data  = ind_test
      )
  )

  drug_true <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA,
      diabetes = c(1, 1, 0, NA, NA)
    )

  expect_equal(drug_res, drug_true)

  suppressMessages(
    ind_res <-
      ind_test |>
      add_ind(i_list, drug_data = drug_test, ind_data  = ind_test))

  ind_true <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2",
                  "d3_ici3"),
      Indication = c("Diabetes mellitus",
                     "Type 2 diabetes mellitus",
                     "Another indication"),
      diabetes = c(1, 1, 0)
    )

  expect_equal(ind_res, ind_true)

  suppressMessages(
    link_res <-
      link_test |>
      add_ind(i_list, drug_data = drug_test, ind_data  = ind_test)
  )

  link_true <-
    data.table(
      Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3),
      Dechallenge1 = NA,
      TimeToOnsetMin = NA,
      diabetes = c(1, 1, 0, NA, NA)
    )

  expect_equal(link_res, link_true)

})

test_that("works with case level tables adr", {
  i_list <-
    list(diabetes = c("Diabetes mellitus", "Type 2 diabetes mellitus"))

  ind_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3"),
      # no ind for d4_ici3 and so on
      Indication = c("Diabetes mellitus",
                     "Type 2 diabetes mellitus",
                     "Another indication")
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  # demo already tested previously

  adr_test <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(100000, 20000, 30000, 40000, 50000),
      Outcome = c(1, 2, 3, 2, 2)
    )

  suppressMessages(
    adr_res <-
      adr_test |>
      add_ind(
        i_list,
        drug_data = drug_test,
        ind_data  = ind_test
      )
  )

  adr_true <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(100000, 20000, 30000, 40000, 50000),
      Outcome = c(1, 2, 3, 2, 2),
      diabetes = c(1, 1, 0, 0, NA)
    )

  expect_equal(adr_res, adr_true)

})

test_that("handle ambiguous names in .data", {

  i_list <-
    list(diabetes = c("Diabetes mellitus", "Type 2 diabetes mellitus"))

  ind_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3"),
      # no ind for d4_ici3 and so on
      Indication = c("Diabetes mellitus",
                     "Type 2 diabetes mellitus",
                     "Another indication"),

      # ambiguous column name
      ind_test = NA
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,

      # ambiguous column name
      drug_test = c(0, 0, 0, 0, 1),
      ind_test = NA
    )

  suppressMessages(
    demo_res <-
      demo_test |>
      add_ind(i_list,
              drug_data = drug_test,
              ind_data  = ind_test)
    )

  demo_true <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,
      drug_test = c(0, 0, 0, 0, 1),
      ind_test = NA,
      diabetes = c(1, 0, NA, NA, NA)
    )

  expect_equal(demo_res, demo_true)

})

test_that("you can choose output column names with i_names", {
  i_list <-
    list(diabetes = c("Diabetes mellitus", "Type 2 diabetes mellitus"))

  ind_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3"),
      Indication = c("Diabetes mellitus",
                     "Type 2 diabetes mellitus",
                     "Another indication")
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA
    )

  suppressMessages(
    demo_res <-
      demo_test |>
      add_ind(
        i_list,
        i_names = c("i1"),
        drug_data = drug_test,
        ind_data  = ind_test
      )
  )

  demo_true <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,
      i1 = c(1, 0, NA, NA, NA)
    )

  expect_equal(demo_res, demo_true)
})

test_that("invalid data types to drug_data or ind_data raise error", {
# check each arg once - the idea is to see whether the error is raised
  # not precisely what the error msg says (which depends on the wrong table
  # itself)

  i_list <-
    list(
      melanoma = c("Malignant melanoma", "Metastatic malignant melanoma")
    )

  expect_snapshot(error = TRUE,
                  {
                    demo_ |>
                      add_ind(
                        i_list,
                        drug_data = adr_, # wrong drug_data arg
                        ind_data = ind_
                      )
                  })

  expect_snapshot(error = TRUE,
                  {
                    demo_ |>
                      add_ind(
                        i_list,
                        drug_data = drug_,
                        ind_data = drug_ # wrong ind_data arg
                      )
                  })
})

test_that("you can use arrow/parquet format", {
  i_list <-
    list(diabetes = c("Diabetes mellitus", "Type 2 diabetes mellitus"))

  ind_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3"),
      Indication = c("Diabetes mellitus",
                     "Type 2 diabetes mellitus",
                     "Another indication")
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA
    )

  tmp_folder <- paste0(tempdir(), "/", "add_ind_t1")

  dir.create(path = tmp_folder)

  arrow::write_parquet(demo_test,
                       sink = paste0(tmp_folder, "\\demo.parquet"))
  arrow::write_parquet(drug_test,
                       sink = paste0(tmp_folder, "\\drug.parquet"))
  arrow::write_parquet(ind_test,
                       sink = paste0(tmp_folder, "\\ind.parquet"))


  demo_parquet <- arrow::read_parquet(paste0(tmp_folder, "\\demo.parquet"))
  drug_parquet <- arrow::read_parquet(paste0(tmp_folder, "\\drug.parquet"))
  ind_parquet  <- arrow::read_parquet(paste0(tmp_folder, "\\ind.parquet"))

  suppressMessages(
    res <-
      demo_parquet |>
      add_ind(
        i_list = i_list,
        drug_data = drug_parquet,
        ind_data  = ind_parquet
      )
  )

  suppressMessages(
    res_a <-
      demo_test |>
      add_ind(
        i_list = i_list,
        drug_data = drug_test,
        ind_data  = ind_test
      )
  )

  expect_equal(res, res_a)

  unlink(tmp_folder, recursive = TRUE)

  if(dir.exists(tmp_folder) & Sys.info()[["sysname"]] != "Windows")
    file.remove(tmp_folder)
})
