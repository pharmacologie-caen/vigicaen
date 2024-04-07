test_that("basic use and here package works", {
  f_sets <-
   list(
     DEMO.txt = data.frame(f0 = c("96548661   32194501051119460820")),
     DRUG.txt = data.frame(f0 = c("70548965   8          4901354    064392080055011    31- 806")
                           ),
     LINK.txt = data.frame(f0 = c("2          654654     51---0.78991   0.98745    ",
                                  "2          456456     51---6.98789   -          ")),
     FOLLOWUP.txt = data.frame(f0 = c("0548978    0254687    ",
                                      "7568798    4565321    ")),
     ADR.txt = data.frame(f0 = c("96570161   14         100474561",
                                 "70578465   17         145078144")),
     OUT.txt = data.frame(f0 = c("70547815   - N",
                                 "96575661   - Y")),
     SRCE.txt = data.frame(f0 = c("4898765    1 ",
                                  "9804562    1 ")),
     IND.txt = data.frame(# 266 length
       f0 = "780954     Cutaneous diseases due to other mycobacteria                                                                                                                                                                                                                "
       ),
     SUSPECTEDDUPLICATES.txt = data.frame(f0 = c("789054     789542     ",
                                                 "780546     654352     "))
   )

  tmp_folder <- tempdir()

  path_base <- paste0(tmp_folder, "/", "main", "/")

  if(!dir.exists(path_base))
    dir.create(path_base)

  path_sub  <- paste0(tmp_folder, "/", "sub",  "/")

  if(!dir.exists(path_sub))
    dir.create(path_sub)

  purrr::iwalk(f_sets, function(d_, name_){
    if(name_ == "SUSPECTEDDUPLICATES.txt"){
      write.table(d_, file = paste0(path_sub, name_), row.names = FALSE, quote = FALSE, col.names = FALSE)
    } else {
      write.table(d_, file = paste0(path_base, name_), row.names = FALSE, quote = FALSE, col.names = FALSE)
    }
  })

   tb_main(path_base = path_base,
           path_sub  = path_sub)

   demo_res <- arrow::read_parquet(paste0(path_base, "demo.parquet"))
   ind_res  <- arrow::read_parquet(paste0(path_base, "ind.parquet"))

   demo_true <-
     dplyr::tibble(
       UMCReportId = 96548661,
       AgeGroup = "3",
       Gender = "2",
       DateDatabase = "19450105",
       Type = "1",
       Region = "1",
       FirstDateDatabase = "19460820")

   ind_true <-
     dplyr::tibble(
       Drug_Id = 780954,
       Indication = "Cutaneous diseases due to other mycobacteria")

   expect_equal(demo_res, demo_true)

   expect_equal(ind_res, ind_true)

   # here syntax

   here_path_base <- here::here(tmp_folder, "main")

   here_path_sub <- here::here(tmp_folder, "sub")

   tb_main(path_base = here_path_base,
           path_sub  = here_path_sub)

   demo_res_here <- arrow::read_parquet(here::here(here_path_base, "demo.parquet"))

   expect_equal(demo_res_here, demo_true)
})
