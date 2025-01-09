## code to convert some variables into integer

# DrecNo, MedicinalProd_Id


drug     <- drug_
mp <- mp_
thg      <- thg_


drug_cvt <- drug |>
  dplyr::mutate(dplyr::across(c(DrecNo, MedicinalProd_Id), as.integer))

mp_cvt <- mp |>
  dplyr::mutate(dplyr::across(c(DrecNo, MedicinalProd_Id), as.integer))

thg_cvt <- thg |>
  dplyr::mutate(dplyr::across(c(Therapgroup_Id, MedicinalProd_Id), as.integer))

# exporting to .rda files

drug_     <- drug_cvt
mp_ <- mp_cvt
thg_      <- thg_cvt

usethis::use_data(drug_            , compress = "xz", overwrite = TRUE)
usethis::use_data(thg_             , compress = "xz", overwrite = TRUE)
usethis::use_data(mp_        , compress = "xz", overwrite = TRUE)


## built in luda_ becomes link_

link_ <- luda_ |>
  dplyr::select(
    Drug_Id,
    Adr_Id,
    Dechallenge1,
    Dechallenge2,
    Rechallenge1,
    Rechallenge2,
    TimeToOnsetMin,
    TimeToOnsetMax,
    tto_mean,
    range,
    UMCReportId
  )

usethis::use_data(link_        , compress = "xz", overwrite = TRUE)
