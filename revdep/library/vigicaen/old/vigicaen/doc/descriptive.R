## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----library, warning=FALSE, message=FALSE------------------------------------
library(vigicaen)
library(rlang)
library(dplyr)

## ----test_datasets------------------------------------------------------------
demo     <- demo_
adr      <- adr_
drug     <- drug_
link     <- link_
out      <- out_
followup <- followup_

srce     <- srce_

thg      <- thg_
mp       <- mp_
meddra   <- meddra_
smq_list <- smq_list_
smq_content <- smq_content_

suspdup <- 
  data.table::data.table(
    UMCReportId = 1,
    SuspectedduplicateReportId = NA
  )

## ----codes, eval=TRUE---------------------------------------------------------
d_drecno <- ex_$d_drecno

a_llt <- ex_$a_llt

## ----demo_dm------------------------------------------------------------------
demo <-
  demo |>
  add_drug(
    d_code = d_drecno,
    drug_data = drug
  )

demo <-
  demo |>
  add_adr(
    a_code = a_llt,
    adr_data = adr
  )


## ----demo_dm_other_cols-------------------------------------------------------
# Age, sex

demo <-
  demo |>
  mutate(
    age = cut(as.integer(AgeGroup),
              breaks = c(0,4,5,6,7,8),
              include.lowest = TRUE, right = TRUE,
              labels = c("<18", "18-45","45-64", "65-74", "75+")),

    sex = case_when(Gender == "1" ~ 1,
                    Gender == "2" ~ 2,
                    Gender %in% c("-","0","9") ~ NA_real_,
                    TRUE ~ NA_real_)
  )

# Death + outcome availability

demo <- 
  demo |> 
  mutate(death = 
           ifelse(UMCReportId %in% out$UMCReportId,
                  UMCReportId %in% 
                    (out |> 
                    filter(Seriousness == "1") |> 
                    pull(UMCReportId)
                    ),
                  NA)
         )

# follow-up, seriousness

demo <-
  demo |>
  mutate(
    fup = if_else(UMCReportId %in% followup$UMCReportId, 1, 0),
    serious = 
      ifelse(
        UMCReportId %in% out$UMCReportId,
        UMCReportId %in% 
          (out |> 
          filter(Serious == "Y") |> 
          pull(UMCReportId)
          ),
        NA)
  )

# year

demo <- 
  demo |> 
  mutate(
    year = as.numeric(substr(FirstDateDatabase, start = 1, stop = 4))
    )

# type of reporter

demo <-
  demo |>
  left_join(
    srce |> transmute(UMCReportId, type_reporter = Type),
    by = "UMCReportId")

## ----desc_fv_age--------------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "age"
)

## ----desc_fv_drug-------------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "nivolumab"
)

## ----desc_fv_serious----------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "serious"
)

## ----desc_fv_raw_values-------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "nivolumab",
  export_raw_values = TRUE
)

## ----demo_re_dm_age-----------------------------------------------------------
demo <-
  demo |>
  mutate(
    age2 = cut(as.integer(AgeGroup),
              breaks = c(0, 6, 7, 8),
              include.lowest = TRUE, right = TRUE,
              labels = c("<64", "65-74", "75+"))
  )


desc_facvar(
  demo,
  vf = "age2"
)

## ----desc_fv_year, error=TRUE-------------------------------------------------
try({
desc_facvar(
  .data = demo,
  vf = "year"
)
})

## ----desc_fv_year_ncat--------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "year",
  ncat_max = 20
)

## ----desc_fv_region-----------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "Region"
)

## ----factor_region------------------------------------------------------------
demo <-
  demo |> 
  mutate(
    Region = factor(Region, levels = c("1", "2", "3", "4", "5", "6"))
  )

levels(demo$Region) <-
  c("African Region",                                    
    "Region of the Americas",                            
    "South-East Asia Region",                            
    "European Region",                                   
    "Eastern Mediterranean Region",                      
    "Western Pacific Region"  
  )

## ----desc_fv_region_factor----------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "Region"
)

## ----desc_fv_format-----------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "nivolumab",
  format = "n_/N_ [pc_%]"
)

## ----desc_fv_digits-----------------------------------------------------------
desc_facvar(
  .data = demo,
  vf = "nivolumab",
  digits = 1
)

## ----screen_drug--------------------------------------------------------------
screen_drug(drug, mp_data = mp, top_n = 5)

## ----screen_drug_add----------------------------------------------------------
drug |> 
  add_adr(
    a_llt,
    adr_data = adr
  ) |> 
  filter(a_colitis == 1) |> 
  screen_drug(
    mp_data = mp, top_n = 5
  )


## ----screen_adr---------------------------------------------------------------
screen_adr(adr_, meddra = meddra_)

## ----adr_dm-------------------------------------------------------------------

adr <-
  adr |>
  add_drug(
    d_code = d_drecno,
    drug_data = drug
  )

adr <-
  adr |>
  add_adr(
    a_code = a_llt,
    adr_data = adr
  )

## ----desc_outcome-------------------------------------------------------------
adr |> 
  desc_outcome(
    drug_s = "nivolumab",
    adr_s = "a_colitis"
  )

## ----create_link--------------------------------------------------------------
link <- 
  link_

## ----link_dm------------------------------------------------------------------
link <-
  link |> 
   add_drug(
    d_code = d_drecno,
    drug_data = drug
  )

link <-
  link |>
  add_adr(
    a_code = a_llt,
    adr_data = adr
  )

## ----link_check_dm------------------------------------------------------------
link |> 
   check_dm(
     cols = c(names(d_drecno), names(a_llt))
     )

## ----tto_mean_range, eval = FALSE---------------------------------------------
# tto_mean = (TimeToOnsetMax + TimeToOnsetMin) / 2
# 
# range = (TimeToOnsetMax + TimeToOnsetMin) / 2 - TimeToOnsetMin

## ----extract_tto_example------------------------------------------------------
extract_tto(
  .data = link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)

## ----desc_tto_example---------------------------------------------------------
desc_tto(
  .data = link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)

## ----desc_tto_many_to_many----------------------------------------------------
desc_tto(
  .data = link,
  drug_s = c("nivolumab", "pembrolizumab"),
  adr_s  = c("a_colitis", "a_pneumonitis")
)

## ----desc_dch_example---------------------------------------------------------
desc_dch(
  link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)

## ----desc_dch_many_to_many----------------------------------------------------
desc_dch(
  link,
  drug_s = c("nivolumab", "pembrolizumab"),
  adr_s  = c("a_colitis", "a_pneumonitis")
)

## ----desc_rch_example---------------------------------------------------------
desc_rch(
  link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)

## ----desc_rch_dm_hack---------------------------------------------------------
link <-
  link |> 
  mutate(
    all_cases = 1
  )

## ----desc_rch_hack------------------------------------------------------------
desc_rch(
  link,
  drug_s = "all_cases",
  adr_s  = "all_cases"
)

