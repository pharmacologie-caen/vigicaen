## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(vigicaen)

## ----paths--------------------------------------------------------------------
path_base   <- "~/vigibase/main/"
path_who    <- "~/vigibase/who/"
path_meddra <- "~/meddra/"

## ----load_tables, eval=FALSE--------------------------------------------------
# demo <- dt_parquet(path_base, "demo", in_memory = FALSE)
# drug <- dt_parquet(path_base, "drug", in_memory = FALSE)
# adr  <- dt_parquet(path_base, "adr", in_memory = FALSE)
# link <- dt_parquet(path_base, "link", in_memory = FALSE)
# 
# mp <- dt_parquet(path_who,  "mp")
# meddra <- dt_parquet(path_meddra, "meddra_hierarchy")

## ----load_example_tables------------------------------------------------------
demo     <- demo_
adr      <- adr_
drug     <- drug_
link     <- link_

mp <- mp_
meddra   <- meddra_

## ----a_sel_d_sel--------------------------------------------------------------
d_sel <- list(
  ipilimumab = "ipilimumab"
)

a_sel <- list(
  # this is a High Level Term
  colitis = "Colitis (excl infective)"
)

## ----get_drecno_get_llt_soc---------------------------------------------------
d_code <- 
  get_drecno(d_sel, mp = mp)

a_code <-
  get_llt_soc(a_sel, term_level = "hlt", meddra = meddra)

## ----vigi_routine, fig.height=7.6, fig.width=4--------------------------------
vigi_routine(
  demo_data = demo,
  drug_data = drug,
  adr_data  = adr,
  link_data = link,
  d_code    = d_code,
  a_code    = a_code,
  vigibase_version = "September 2024"
)

## ----vigi_routine_case, fig.height=7.6, fig.width=4---------------------------
vigi_routine(
  case_tto  = 150,
  demo_data = demo,
  drug_data = drug,
  adr_data  = adr,
  link_data = link,
  d_code    = d_code,
  a_code    = a_code,
  vigibase_version = "September 2024"
)

## ----vigi_routine_case_export, eval=FALSE-------------------------------------
# vigi_routine(
#   case_tto  = 150,
#   demo_data = demo,
#   drug_data = drug,
#   adr_data  = adr,
#   link_data = link,
#   d_code    = d_code,
#   a_code    = a_code,
#   d_label   = "Ipilimumab",
#   a_label   = "Colitis (HLT)",
#   vigibase_version = "September 2024",
#   export_to = "~/vigicaen_graph.png"
# )

## ----dual_drug, fig.height=7.6, fig.width=4-----------------------------------
d1 <- ex_$d_drecno["nivolumab"]
d2 <- ex_$d_drecno["ipilimumab"]
a_llt <- ex_$a_llt["a_colitis"]

demo <- demo_
adr  <- adr_
drug <- drug_
link <- link_

vigi_routine(
  demo_data = demo,
  drug_data = drug,
  adr_data  = adr,
  link_data = link,
  d_code = d1,
  d_code_2 = d2,
  a_code = a_llt,
  vigibase_version = "September 2024"
)

