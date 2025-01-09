library(tidyverse)

anonymizer <-
  function(value){

    value_to_replace <-
        as.character(value)

    pattern <-
      sample(0:9, 10, replace = FALSE) |>
      as.character()

    intermediate <- letters[1:10]

        replace <- rev(pattern)

    # replace <- pattern

    for(i in seq_along(pattern)){
      value_to_replace <-
        value_to_replace |>
        stringr::str_replace_all(
          pattern[i],
          intermediate[i]
        )
    }

    for(i in seq_along(pattern)){
      value_to_replace <-
        value_to_replace |>
        stringr::str_replace_all(
          intermediate[i],
          replace[i]
        )
    }

    vtr_1 <-
      value_to_replace |>
      stringr::str_sub(start = 1L, end = 1L)

    vtr_3 <-
      value_to_replace |>
      stringr::str_sub(start = 3L, end = 3L)

    vtr_5 <-
      value_to_replace |>
      stringr::str_sub(start = 5L, end = 5L)

    vtr_7 <-
      value_to_replace |>
      stringr::str_sub(start = 7L, end = 7L)

    vtr_2 <-
      value_to_replace |>
      stringr::str_sub(start = 2L, end = 2L)

    vtr_4 <-
      value_to_replace |>
      stringr::str_sub(start = 4L, end = 4L)

    vtr_6 <-
      value_to_replace |>
      stringr::str_sub(start = 6L, end = 6L)

    vtr_8 <-
      value_to_replace |>
      stringr::str_sub(start = 8L, end = 8L)

    vtr_end <-
      value_to_replace |>
      stringr::str_sub(start = 9L)


    vtr_end <-
      value_to_replace |>
      stringr::str_sub(start = 9L)

    value_to_replace <-
      paste0(vtr_7, vtr_end,
             vtr_1, vtr_8,
             vtr_6, vtr_4,
             vtr_2, vtr_5,
             vtr_3
             )

    # output according to initial type

    if(class(value) == "numeric"){
      as.numeric(value_to_replace)
    } else if(class(value) == "integer"){
      as.integer(value_to_replace)
    } else if(class(value) == "character"){
      as.character(value_to_replace)
    }
  }

anonymizer2 <-
  function(value, replacement){

    value_to_replace <-
      as.numeric(value)

    # use value input as indices to replacement

    output <-
      replacement[value_to_replace]

    # output according to initial type

    if(class(value) == "numeric"){
      as.numeric(output)
    } else if(class(value) == "integer"){
      as.integer(output)
    } else if(class(value) == "character"){
      as.character(output)
    }
  }




testthat::test_that("the same number always produces the same replacement", {
  n <- rep(6508068, 1000)

  replacement <-
    anonymizer(n)

  expect_equal(
    length(unique(replacement)),
    1
  )
  })

testthat::test_that("input class is preserved", {
  n <- as.numeric(65080)

  replacement <-
    anonymizer(n)

  n_char <- as.character(65080)

  replacement_char <-
    anonymizer(n_char)

  n_int <- as.integer(65080)

  replacement_int <-
    anonymizer(n_int)

  expect_equal(
    c(class(n),
      class(n_char),
      class(n_int)),
    c(class(replacement),
      class(replacement_char),
      class(replacement_int))
  )
})

load(here::here("data-raw", "nums_shuffled.RData"))

# check the tests for ano_2
testthat::test_that("the same number always produces the same replacement", {
  n <- rep(6508068, 1000)

  replaced <-
    anonymizer2(n, nums_shuffled)

  expect_equal(
    length(unique(replaced)),
    1
  )
})

testthat::test_that("input class is preserved", {
  n <- as.numeric(65080)

  replacement <-
    anonymizer2(n, nums_shuffled)

  n_char <- as.character(65080)

  replacement_char <-
    anonymizer2(n_char, nums_shuffled)

  n_int <- as.integer(65080)

  replacement_int <-
    anonymizer2(n_int, nums_shuffled)

  expect_equal(
    c(class(n),
      class(n_char),
      class(n_int)),
    c(class(replacement),
      class(replacement_char),
      class(replacement_int))
  )

  expect_false(
    any(is.na(replacement_char))
  )
  expect_false(
    any(is.na(replacement_int))
  )
  expect_false(
    any(is.na(replacement))
  )
})

testthat::test_that("there can be several times the same input", {
  n <- c(12, 12, 24, 24)

  replaced <-
    anonymizer2(n, nums_shuffled)

  expect_equal(replaced[1], replaced[2])
  expect_equal(replaced[3], replaced[4])
})

# ---- anonymized data ---- ####

demo_ano <-
  demo_ |>
  mutate(across(c(UMCReportId),
                ~ anonymizer2(.x, nums_shuffled)))

drug_ano <-
  drug_ |>
  mutate(across(c(UMCReportId, Drug_Id, MedicinalProd_Id, DrecNo),
                ~ anonymizer2(.x, nums_shuffled)))

adr_ano <-
  adr_ |>
  mutate(across(c(UMCReportId, Adr_Id, MedDRA_Id),
                ~ anonymizer2(.x, nums_shuffled)))

link_ano <-
  link_ |>
  mutate(across(c(Drug_Id, Adr_Id),
                ~ anonymizer2(.x, nums_shuffled)))

ind_ano <-
  ind_ |>
  mutate(across(c(Drug_Id),
                ~ anonymizer2(.x, nums_shuffled)))
luda_ano <-
  luda_ |>
  mutate(across(c(UMCReportId, Drug_Id, Adr_Id),
                ~ anonymizer2(.x, nums_shuffled)))

luda_ano$DrecNo <- NULL
luda_ano$MedDRA_Id <- NULL

out_ano <-
  out_ |>
  mutate(across(c(UMCReportId),
                ~ anonymizer2(.x, nums_shuffled)))

srce_ano <-
  srce_ |>
  mutate(across(c(UMCReportId),
                ~ anonymizer2(.x, nums_shuffled)))

followup_ano <-
  followup_ |>
  mutate(across(c(UMCReportId, ReplacedUMCReportId),
                ~ anonymizer2(.x, nums_shuffled)))
thg_ano <-
  thg_ |>
  mutate(across(c(Therapgroup_Id, MedicinalProd_Id),
                ~ anonymizer2(.x, nums_shuffled)))
meddra_ano <-
  meddra_ |>
  mutate(across(c(llt_code, pt_code, hlt_code, hlgt_code, soc_code, pt_soc_code),
                ~ anonymizer2(.x, nums_shuffled)))
mp_ano <-
  mp_ |>
  mutate(across(c(MedicinalProd_Id, DrecNo, Create.date, Date.changed),
                ~ anonymizer2(.x, nums_shuffled)))

smq_list_content_ano <-
  smq_list_content_ |>
  mutate(across(c(smq_code, term_code),
                ~ anonymizer2(.x, nums_shuffled)))

ex_ano <- list()

ex_ano$a_llt <-
  ex_$a_llt |>
  purrr::map(anonymizer2, replacement = nums_shuffled)

ex_ano$d_drecno <-
  ex_$d_drecno |>
  purrr::map(anonymizer2, replacement = nums_shuffled)

ex_ano$d_groups_drecno <-
  ex_$d_groups_drecno |>
  purrr::map(anonymizer2, replacement = nums_shuffled)

ex_ano$d_groups <-
  ex_$d_groups

ex_ano$pt_sel <-
  ex_$pt_sel

ex_ano$smq_sel <-
  ex_$smq_sel



# --- check anonymization ---- ####

all.equal(demo_ano$UMCReportId, demo_$UMCReportId)

all.equal(ex_ano$a_llt, ex_$a_llt)
all.equal(ex_ano$d_drecno, ex_$d_drecno)
all.equal(ex_ano$d_groups_drecno, ex_$d_groups_drecno)

# ---- check that results are identical ---- ####

# same drecnos extracted and stored

drec_ano <-get_drecno(ex_ano$d_groups,
           mp = mp_ano,
           allow_combination = TRUE)

# had more drecnos before... don't know why.
ex_ano$d_groups_drecno

ex_ano$d_groups_drecno <- drec_ano

get_drecno(ex_$d_groups,
           mp = mp_,
           allow_combination = TRUE)

demo <-
  demo_ |>
  add_drug(
    d_code = ex_$d_drecno,
    drug_data = drug_
  ) |>
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr_
  )

demo_2 <-
  demo_ano |>
  add_drug(
    d_code = ex_ano$d_drecno,
    drug_data = drug_ano
  )|>
  add_adr(
    a_code = ex_ano$a_llt,
    adr_data = adr_ano
  )

all.equal(demo |> select(all_of(names(ex_$d_drecno))),
          demo_2 |> select(all_of(names(ex_$d_drecno)))
)

check_dm(demo, names(ex_$d_drecno))
check_dm(demo_2, names(ex_$d_drecno))
# counts are different, my guess is that new ano numbers can match
# other cases in the base
# say colitis id switched from 100 to 200, and 200 is actually a hepatitis code.
# that makes it possible to find inappropriate events
# well, that's a pretty good news for anonymization, but that's bad news to
# be sure everything is still in place...
# PLUS, it will mess up many tests...

check_dm(demo, names(ex_$a_llt))
check_dm(demo_2, names(ex_$a_llt))

# Ok, now lets try to replace existing data by the ano one and run tests...



# exporting to .rda files

demo_             <- demo_ano
drug_             <- drug_ano
adr_              <- adr_ano
out_              <- out_ano
srce_             <- srce_ano
link_             <- link_ano
ind_              <- ind_ano
followup_         <- followup_ano
thg_              <- thg_ano
meddra_           <- meddra_ano
mp_         <- mp_ano
smq_list_content_ <- smq_list_content_ano
luda_             <- luda_ano

ex_               <- ex_ano


usethis::use_data(demo_            , compress = "xz", overwrite = TRUE)
usethis::use_data(drug_            , compress = "xz", overwrite = TRUE)
usethis::use_data(adr_             , compress = "xz", overwrite = TRUE)
usethis::use_data(out_             , compress = "xz", overwrite = TRUE)
usethis::use_data(srce_            , compress = "xz", overwrite = TRUE)
usethis::use_data(link_            , compress = "xz", overwrite = TRUE)
usethis::use_data(ind_             , compress = "xz", overwrite = TRUE)
usethis::use_data(followup_        , compress = "xz", overwrite = TRUE)
usethis::use_data(thg_             , compress = "xz", overwrite = TRUE)
usethis::use_data(meddra_          , compress = "xz", overwrite = TRUE)
usethis::use_data(mp_        , compress = "xz", overwrite = TRUE)
usethis::use_data(smq_list_content_, compress = "xz", overwrite = TRUE)
usethis::use_data(ex_              , compress = "xz", overwrite = TRUE)
usethis::use_data(luda_            , compress = "xz", overwrite = TRUE)

