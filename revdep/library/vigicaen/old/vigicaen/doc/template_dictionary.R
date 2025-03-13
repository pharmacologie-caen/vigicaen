## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, warning = FALSE, message = FALSE------------------------------
library(vigicaen)
library(rlang)
library(dplyr)

## ----real_data, eval = FALSE--------------------------------------------------
# # #### PATHS #### ####
# 
# path_who    <- "../vigibase_ecl/who" # adapt to your own
# 
# path_meddra <- "../meddra"
# 
# # #### IMPORT #### ####
# 
# # ---- Datasets ---- ####
# 
# mp     <- dt_parquet(path_who, "mp")
# 
# thg    <- dt_parquet(path_who, "thg")
# 
# meddra <- dt_parquet(path_meddra, "meddra_hierarchy")
# 
# smq_list <- dt_parquet(path_meddra, "meddra_smq_list")
# 
# smq_content <- dt_parquet(path_meddra, "meddra_smq_content")

## ----test_data, echo = FALSE, include = FALSE---------------------------------
meddra <- meddra_
mp <- mp_
smq_list <- smq_list_
smq_content <- smq_content_
thg <- thg_

## ----drugs_adrs, echo = TRUE, eval = TRUE, message = FALSE--------------------
# #### Drug definitions #### ####

d_sel <-
  list(
    nivolumab = "nivolumab", # use lower case
    ici = c("nivolumab", "pembrolizumab")
  )

# ---- ID collector

d_drecno <-
  d_sel |>
  get_drecno(
    mp = mp
  )

# #### ATC definitions #### ####

atc_sel <-
  list(l03 = "L03") # ATC code

# ---- ID collector

atc_drecno <-
  atc_sel |>
  get_atc_code(
    mp = mp,
    thg_data = thg
  )

# #### ADR definitions #### ####

# adr selection, matching meddra case

# ---- SOC definition

pt_sel <-
  list(
    hepatitis = c("Hepatitis", "Budd-Chiari syndrome"),
    hyponatremia = c("Pneumonitis")
  )

hlt_sel <-
  list(
    conduction = "Diarrhoea (excl infective)"
  )

# ---- SMQ definition

smq_sel <-
  list(
    hta = "Ischaemic heart disease (SMQ)"
  )

# ---- ID collectors

a_llt <-
  c(
    get_llt_soc(pt_sel,
                term_level = "pt",
                meddra = meddra),
    get_llt_soc(hlt_sel,
                term_level = "hlt",
                meddra),
    get_llt_smq(smq_sel,
                smq_scope =  "narrow",
                smq_list,
                smq_content)
  )

