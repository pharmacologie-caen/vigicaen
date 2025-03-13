## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(vigicaen)
library(rlang)
library(dplyr)

## ----interaction_dm-----------------------------------------------------------

# ---- Tables ---- ####

demo <- demo_
drug <- drug_

# ---- Dictionary step ---- ####

d_drecno <- ex_$d_drecno
a_llt <- ex_$a_llt

# #### Data management #### ####

# ---- Drugs ---- ####

demo <-
    demo |>
    add_drug(
      d_code = d_drecno,
      drug_data = drug_
    ) 

# ---- Adrs ---- ####

demo <- 
  demo |>
    add_adr(
      a_code = a_llt,
      adr_data = adr_
    )

# ---- Sex ---- ####

demo <- 
  demo |> 
  mutate(
    sex = case_when(Gender == "1" ~ 1,
                    Gender == "2" ~ 2,
                    TRUE ~ NA_real_)
    )

## ----mod_inter----------------------------------------------------------------
mod3 <- glm(a_colitis ~ ipilimumab + sex,
            data = demo,
            family = "binomial")

mod_or <- 
  compute_or_mod(
  summary(mod3)$coefficients,
      estimate = Estimate,
    std_er = Std..Error
  ) |> select(rn, orl, ci, up_ci)

mod_or

## ----echo=FALSE---------------------------------------------------------------
ror_ipi <- mod_or[rn == "ipilimumab", orl]
ror_sex <- mod_or[rn == "sex", orl]

## -----------------------------------------------------------------------------
demo |> 
  filter(nivolumab == 1) |> 
  compute_dispro(
    y = "a_colitis",
    x = "ipilimumab"
    )

## -----------------------------------------------------------------------------
mod4 <- glm(a_colitis ~ ipilimumab + sex + ipilimumab * sex,
            data = demo,
            family = "binomial")

compute_or_mod(
  summary(mod4)$coefficients,
  estimate = Estimate,
  std_er = Std..Error
)

## ----compute_int--------------------------------------------------------------

demo |>
  compute_interaction(
    y = "a_colitis",
    x = "ipilimumab",
    z = "nivolumab"
  )


