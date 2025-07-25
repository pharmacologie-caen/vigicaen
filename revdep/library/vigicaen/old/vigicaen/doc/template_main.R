## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE,
  echo = TRUE,
  warning = FALSE, 
  message = FALSE
)

## ----libraries----------------------------------------------------------------
library(vigicaen)
library(rlang)
library(here) # if you like the here syntax for file paths
library(dplyr)

## ----path_classic, eval = FALSE-----------------------------------------------
# # #### PATHS #### ####
# 
# path_base   <- "../vigibase_ecl/main"
# 
# # #### IMPORT #### ####
# 
# demo     <- dt_parquet(path_base, "demo", in_memory = FALSE)
# adr      <- dt_parquet(path_base, "adr",  in_memory = FALSE)
# drug     <- dt_parquet(path_base, "drug", in_memory = FALSE)
# link     <- dt_parquet(path_base, "link", in_memory = FALSE)
# out      <- dt_parquet(path_base, "out",  in_memory = FALSE)
# srce     <- dt_parquet(path_base, "srce", in_memory = FALSE)
# followup <- dt_parquet(path_base, "followup", in_memory = FALSE)
# 
# suspdup  <- dt_parquet(path_base, "suspdup", in_memory = FALSE)
# 
# source("00_dict.R") # from template_dictionary

## ----test_datasets, include = FALSE-------------------------------------------
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

## ----codes, include = FALSE, eval=TRUE, message = FALSE-----------------------
d_drecno <- ex_$d_drecno

atc_drecno <-
  get_atc_code(atc_sel =  rlang::list2(l03 = c("L03AA")),
               mp = mp_,
               thg_data = thg_,
               vigilyze = TRUE)

a_llt <- ex_$a_llt

## ----demo_dm, eval = TRUE-----------------------------------------------------
# ---- Deduplicating ---- ####

demo <- demo |> 
  filter(!(UMCReportId %in% suspdup$SuspectedduplicateReportId))

# ---- Drugs ---- ####

# From a list of drugs

demo <-
  demo |>
  add_drug(
    d_code = d_drecno,
    drug_data = drug
  )

# From ATC

demo <-
  demo |>
  add_drug(
    d_code = atc_drecno,
    drug_data = drug
  )

# ---- Reactions ---- ####

demo <-
  demo |>
  add_adr(
    a_code = a_llt,
    adr_data = adr
  )

# ---- Demographics ---- ####

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

# explicit multi-level vars

demo <-
  demo |> 
  mutate(
    Type = factor(Type, levels = c("1", "2", "3", "4", "5")),
    type_reporter = factor(type_reporter,
                           levels = c("1", "2", "3", "4", "5")),
    Region = factor(Region, levels = c("1", "2", "3", "4", "5", "6"))
  )

levels(demo$Type) <-
  c("Spontaneous",                     
     "Report from study",                        
     "Other",   
     "Not available to sender (unknown)",   
     "PMS/Special monitoring")

levels(demo$type_reporter) <-
  c("Physician",
    "Pharmacist",
    "Other Health Professional",
    "Lawyer",
    "Consumer or other non health professional")

levels(demo$Region) <-
  c("African Region",                                    
    "Region of the Americas",                            
    "South-East Asia Region",                            
    "European Region",                                   
    "Eastern Mediterranean Region",                      
    "Western Pacific Region"  
  )

# ---- Check ---- ####

demo |>
  check_dm(cols = c(names(d_drecno), names(a_llt), "fup"))

## ----link_drug_adr------------------------------------------------------------
link <-
  link |> 
    add_drug(
      d_code = d_drecno,
      drug_data = drug
    ) |> 
    add_adr(
      a_code = a_llt,
      adr_data = adr
    )

## ----mods---------------------------------------------------------------------
# ---- Bivariate ---- ####

rb <- 
  demo |> 
  compute_dispro(
    y = names(a_llt),
    x = names(d_drecno)
  )

# remove the hashes to save your results
## write.csv2(rb, here("outputs", "rb.csv"), row.names = FALSE)

# ---- Multivariate ---- ####

mod <-
  glm(a_colitis ~ nivolumab + age + sex,
      family = "binomial",
      data = demo)

rm <-
  summary(mod)$coefficients |>
  compute_or_mod(
    estimate = Estimate,
    std_er = Std..Error
    )

# remove the hashes to save your results
## write.csv2(rm, here("outputs", "rm.csv"), row.names = FALSE)

## ----desc_demo----------------------------------------------------------------
# ---- General description ---- ####

r_desc <- 
  demo |> 
  desc_facvar(
    vf = c("age", "sex",
           "type_reporter",
           "Type",
           "year",
           names(d_drecno),
           names(a_llt),
           "serious", "death"),
    ncat_max = 20
  )

# ---- Time to onset ---- ####

r_tto <-
  desc_tto(
    link,
    adr_s = names(a_llt),
    drug_s = names(d_drecno)
    ) 

# ---- Dechallenge ---- ####

r_dch <-
  desc_dch(
    link,
    adr_s = names(a_llt),
    drug_s = names(d_drecno)
    )

# ---- Rechallenge ---- ####

r_rch <-
  desc_rch(
    link,
    adr_s = names(a_llt),
    drug_s = names(d_drecno)
  )

