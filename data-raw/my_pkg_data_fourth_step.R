


  <-
  smq_list_content_ |>
  dplyr::distinct(
    smq_code,
    smq_name,
    smq_level,
    smq_description,
    smq_source,
    smq_note,
    MedDRA_version,
    status,
    smq_algorithm
  )


smq_content_ <-
  smq_list_content_ |>
  dplyr::select(
    smq_code,
    term_code,
    term_level,
    term_scope,
    term_category,
    term_weight,
    term_status,
    term_addition_version,
    term_last_modified_version
  ) |>
  dplyr::distinct()


usethis::use_data(smq_list_        , compress = "xz", overwrite = TRUE)
usethis::use_data(smq_content_     , compress = "xz", overwrite = TRUE)
