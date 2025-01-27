
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

# set ex_$d_drecno to integer

ex_$d_drecno <-
  ex_$d_drecno |>
  purrr::map(
    function(x)
      as.integer(x)
  )

ex_$d_groups_drecno <-
  ex_$d_groups_drecno |>
  purrr::map(
    function(x)
      as.integer(x)
  )

usethis::use_data(ex_     , compress = "xz", overwrite = TRUE)
