

mp_ <-
  mp_ |>
  dplyr::rename(
    Record_Id = MedicinalProd_Id
  )

drug_ <-
  drug_ |>
  dplyr::rename(
    Record_Id = MedicinalProd_Id
  )


thg_ <-
  thg_ |>
  dplyr::rename(
    Record_Id = MedicinalProd_Id
  )

usethis::use_data(mp_        , compress = "xz", overwrite = TRUE)
usethis::use_data(drug_      , compress = "xz", overwrite = TRUE)
usethis::use_data(thg_       , compress = "xz", overwrite = TRUE)
