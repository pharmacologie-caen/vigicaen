

mp_ <-
  mp_ |>
  dplyr::add_row(
    MedicinalProd_Id = 123456787,
    Sequence.number.1 = "01",
    Sequence.number.2 = "001",
    DrecNo = 10116627,
    drug_name_t = "ramipril;bis"
  )

usethis::use_data(mp_        , compress = "xz", overwrite = TRUE)
