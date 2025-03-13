## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic, eval = FALSE------------------------------------------------------
# library(vigicaen)
# 
# # change for the correct paths on your computer
# path_base   <- "../vigibase_ecl/main"
# path_sub    <- "../vigibase_ecl/sub"
# path_who    <- "../vigibase_ecl/who"
# path_meddra <- "../meddra"
# 
# tb_vigibase(
#   path_base = path_base,
#   path_sub  = path_sub
#   )
# 
# tb_who(
#   path_who  = path_who
#   )
# 
# tb_meddra(
#   path_meddra = path_meddra
#   )

## ----load_tables, eval = FALSE------------------------------------------------
# 
# demo   <- dt_parquet(path_main, "demo")
# drug   <- dt_parquet(path_main, "drug")
# adr    <- dt_parquet(path_main, "adr")
# link   <- dt_parquet(path_main, "link")
# # etc.
# 
# mp     <- dt_parquet(path_who, "mp")
# 
# meddra <- dt_parquet(path_meddra, "meddra_hierarchy")
# 

## ----load_tables_out_memory, eval = FALSE-------------------------------------
# demo   <- dt_parquet(path_main, "demo", in_memory = FALSE)

## ----tb_subset, eval = FALSE--------------------------------------------------
# 
# sv_selection <-
#     c(7, #  65 - 74 years group
#       8) #  >= 75 years group
# 
# wd_in <- "some/place/on/your/computer/containing/vigibase_ecl/main"
# 
# wd_out <- paste0(wd_in, "/", "more_than_65_subset", "/")
# 
# tb_subset(wd_in, wd_out,
#           subset_var = "age",
#           sv_selection = sv_selection)

