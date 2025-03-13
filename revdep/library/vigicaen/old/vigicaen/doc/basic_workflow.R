## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----library, warning=FALSE, message=FALSE------------------------------------
library(vigicaen)
library(rlang)
library(dplyr)

## ----named_lists--------------------------------------------------------------
# drug selection
d_sel <-
  list(
    nivolumab   = "nivolumab",
    ipilimumab  = "ipilimumab",
    nivo_or_ipi = c("nivolumab", "ipilimumab")
  )

# adverse drug reaction selection

a_sel <- 
  list(
    colitis     = "Colitis",
    pneumonitis = "Pneumonitis"
  )

## ----table_loading, warning=FALSE, message=FALSE------------------------------
demo <- demo_
drug <- drug_

mp <- mp_

## ----drug_sel-----------------------------------------------------------------
d_sel <- # drug selection
  list2(
    nivolumab = "nivolumab"
  )

d_sel

## ----verbose_with_get_drecno--------------------------------------------------
get_drecno(
  d_sel = d_sel,
  mp = mp_,
  verbose = TRUE
  )

## ----d_drecno-----------------------------------------------------------------
d_drecno <-
  get_drecno(
    d_sel = d_sel,
    mp = mp_,
    verbose = FALSE
    )

## ----add_drug-----------------------------------------------------------------
demo <- 
  add_drug(
    .data = demo,
    d_code = d_drecno,
    drug_data = drug)
demo

## ----results='hide'-----------------------------------------------------------
demo <- 
  demo |> 
  add_drug(
    d_code = d_drecno,
    drug_data = drug
  )

## ----check_dm-----------------------------------------------------------------
check_dm(demo, "nivolumab")

## ----atc_drecno---------------------------------------------------------------
atc_sel <-
  list2(l03 = "L03")

atc_drecno <- 
  get_atc_code(
    atc_sel = atc_sel,
    mp = mp,
    thg_data = thg_
    )

## ----str_atc_drecno-----------------------------------------------------------
str(atc_drecno)

## ----add_atc------------------------------------------------------------------
demo |> 
  add_drug(
    d_code = atc_drecno,
    drug_data = drug
  )

## ----add_drug_repbasis--------------------------------------------------------
demo |> 
  add_drug(
    d_code = d_drecno,
    drug_data = drug,
    repbasis = "sci"
  ) |> 
  check_dm("nivolumab")

# suspected only

demo |> 
  add_drug(
    d_code = d_drecno,
    drug_data = drug,
    repbasis = "s"
  ) |> 
  check_dm("nivolumab")

## ----many_drugs---------------------------------------------------------------
d_sel <- 
  list2(
    nivolumab = "nivolumab",
    pembrolizumab = "pembrolizumab"
  )

d_drecno <-
  d_sel |> 
  get_drecno(mp = mp)

demo <- 
  demo |> 
  add_drug(
    d_drecno,
    drug_data = drug
  )

demo |> 
  check_dm(c("nivolumab", "pembrolizumab"))

## ----d_groups-----------------------------------------------------------------
d_sel <- 
  list2(
    analgesics = c("paracetamol", "tramadol"),
    ici = c("nivolumab", "pembrolizumab")
  )

d_drecno <-
  d_sel |> 
  get_drecno(mp = mp,
             allow_combination = FALSE)

demo <- 
  demo |> 
  add_drug(
    d_drecno,
    drug_data = drug
  )

demo |> 
  check_dm(names(d_sel))

## ----load_adr_table-----------------------------------------------------------
adr <- adr_
meddra <- meddra_

## ----a_sel_pt-----------------------------------------------------------------
a_sel_pt <-
  list2(
    a_colitis = c(
      "Colitis",
      "Autoimmune colitis",
      "Colitis microscopic",
      "Diarrhoea",
      "Diarrhoea haemorrhagic",
      "Duodenitis",
      "Enteritis",
      "Enterocolitis",
      "Enterocolitis haemorrhagic",
      "Ulcerative gastritis"
    )
  )

## ----get_llt_soc--------------------------------------------------------------
a_llt <- 
  get_llt_soc(
    term_sel = a_sel_pt,
    term_level = "pt",
    meddra = meddra_
    )

a_llt

## ----add_adr------------------------------------------------------------------
demo <- 
  add_adr(
    .data = demo,
    a_code = a_llt,
    adr_data = adr)

## ----check_dm_adr-------------------------------------------------------------
demo |> 
  check_dm("a_colitis")

## ----age----------------------------------------------------------------------
demo <-
  demo |>
  mutate(
    age = cut(as.integer(AgeGroup),
              breaks = c(0,4,5,6,7,8),
              include.lowest = TRUE, right = TRUE,
              labels = c("<18", "18-45","45-64", "65-74", "75+"))
  )

## ----sex----------------------------------------------------------------------
demo <-
  demo |> 
  mutate(
    sex = ifelse(Gender == "1", 1,
                 ifelse(Gender == "2", 2, NA_real_)
                 )
    )

## ----sex_casewhen-------------------------------------------------------------
demo <- 
  demo |> 
  mutate(
    sex = case_when(Gender == "1" ~ 1,
                    Gender == "2" ~ 2,
                    TRUE ~ NA_real_)
    )

## ----serious_death------------------------------------------------------------
# ---- Serious ---- ####

out <- out_

demo <- 
  demo |> 
  mutate(
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

# ---- Death + outcome availability ---- ####

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


## ----compute_dispro-----------------------------------------------------------
demo |> 
  compute_dispro(
    y = "a_colitis",
    x = "nivolumab"
    )

## ----mod----------------------------------------------------------------------
mod <- glm(a_colitis ~ nivolumab, 
           data = demo, family = "binomial")

summary(mod)

## -----------------------------------------------------------------------------
summary(mod)$coefficients

exp(summary(mod)$coefficients[2, 1])

## ----mod_covar----------------------------------------------------------------
mod2 <- glm(a_colitis ~ nivolumab + sex + age,
            data = demo,
            family = "binomial")

summary(mod2)

## ----compute_or_mod-----------------------------------------------------------
mod_or <- 
  compute_or_mod(
    summary(mod2)$coefficients,
    estimate = Estimate,
    std_er = Std..Error
    )

mod_or

