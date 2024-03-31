anonymizer <-
  function(value){

    value_to_replace <-
        as.character(value)

    set.seed(123)  # Set seed for reproducibility
    pattern <-
      sample(10:99, 100, replace = TRUE) |>
      as.character()

    set.seed(321)
    replace <-
      sample(10:99, 100, replace = TRUE) |>
      as.character()

    for(i in seq_along(pattern)){
      value_to_replace <-
        value_to_replace |>
        stringr::str_replace(
          pattern[i],
          replace[i]
        )
    }

    # output according to initial type

    if(class(value) == "numeric"){
      as.numeric(value_to_replace)
    } else if(class(value) == "integer"){
      as.integer(value_to_replace)
    } else if(class(value) == "character"){
      as.character(value_to_replace)
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

# ---- anonymized data ---- ####

demo_ano <-
  demo_ |>
  mutate(across(c(UMCReportId),
                ~ anonymizer(.x)))

drug_ano <-
  drug_ |>
  mutate(across(c(UMCReportId, Drug_Id, MedicinalProd_Id, DrecNo),
                ~ anonymizer(.x)))

adr_ano <-
  adr_ |>
  mutate(across(c(UMCReportId, Adr_Id, MedDRA_Id),
                ~ anonymizer(.x)))

link_ano <-
  link_ |>
  mutate(across(c(Drug_Id, Adr_Id),
                ~ anonymizer(.x)))

ind_ano <-
  ind_ |>
  mutate(across(c(Drug_Id),
                ~ anonymizer(.x)))
luda_ano <-
  luda_ |>
  mutate(across(c(UMCReportId, Drug_Id, Adr_Id),
                ~ anonymizer(.x)))

luda_ano$DrecNo <- NULL
luda_ano$MedDRA_Id <- NULL

out_ano <-
  out_ |>
  mutate(across(c(UMCReportId),
                ~ anonymizer(.x)))

srce_ano <-
  srce_ |>
  mutate(across(c(UMCReportId),
                ~ anonymizer(.x)))

followup_ano <-
  followup_ |>
  mutate(across(c(UMCReportId, ReplacedUMCReportId),
                ~ anonymizer(.x)))
thg_ano <-
  thg_ |>
  mutate(across(c(Therapgroup_Id, MedicinalProd_Id),
                ~ anonymizer(.x)))
meddra_ano <-
  meddra_ |>
  mutate(across(c(llt_code, pt_code, hlt_code, hlgt_code, soc_code, pt_soc_code),
                ~ anonymizer(.x)))
mp_short_ano <-
  mp_short_ |>
  mutate(across(c(MedicinalProd_Id, DrecNo, Create.date, Date.changed),
                ~ anonymizer(.x)))

smq_list_content_ano <-
  smq_list_content_ |>
  mutate(across(c(smq_code, term_code),
                ~ anonymizer(.x)))

ex_ano <- list()

ex_ano$a_llt <-
  ex_$a_llt |>
  purrr::map(anonymizer)

ex_ano$d_drecno <-
  ex_$d_drecno |>
  purrr::map(anonymizer)

ex_ano$d_groups_drecno <-
  ex_$d_groups_drecno |>
  purrr::map(anonymizer)

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

get_drecno(ex_ano$d_groups,
           mp_short = mp_short_ano,
           allow_combination = TRUE)

ex_ano$d_groups_drecno

get_drecno(ex_$d_groups,
           mp_short = mp_short_,
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
