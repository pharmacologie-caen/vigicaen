# you can subset on drecno, age, meddra_id

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/",
        "subset_drecno", "/"), subset_var = "drecno", sv_selection = sv_selection_drecno)
    Message
      
      -- tb_subset() -----------------------------------------------------------------
      i Subsetting VigiBase tables.
      ====>-------------------------- percent, seconds | Reading source tables 
      =========>--------------------- percent, seconds | Picking subset 
      ==============>---------------- percent, seconds | Collect case list 
      ===============>--------------- percent, seconds | Collect drug list 
      ==================>------------ percent, seconds | Apply case level subset 
      ========================>------ percent, seconds | Apply drug level subset 
      ==============================> percent, seconds | Done 
      v Subset successful
      
      i percent, seconds | Number of rows in subset
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      

---

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/",
        "subset_drecno_item", "/"), subset_var = "drecno", sv_selection = sv_selection_drecno_item)
    Message
      
      -- tb_subset() -----------------------------------------------------------------
      i Subsetting VigiBase tables.
      ====>-------------------------- percent, seconds | Reading source tables 
      =========>--------------------- percent, seconds | Picking subset 
      ==============>---------------- percent, seconds | Collect case list 
      ===============>--------------- percent, seconds | Collect drug list 
      ==================>------------ percent, seconds | Apply case level subset 
      ========================>------ percent, seconds | Apply drug level subset 
      ==============================> percent, seconds | Done 
      v Subset successful
      
      i percent, seconds | Number of rows in subset
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      

---

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/", "subset_age",
        "/"), subset_var = "age", sv_selection = c(7, 8))
    Message
      
      -- tb_subset() -----------------------------------------------------------------
      i Subsetting VigiBase tables.
      ====>-------------------------- percent, seconds | Reading source tables 
      =========>--------------------- percent, seconds | Picking subset 
      ==============>---------------- percent, seconds | Collect case list 
      ===============>--------------- percent, seconds | Collect drug list 
      ==================>------------ percent, seconds | Apply case level subset 
      ========================>------ percent, seconds | Apply drug level subset 
      ==============================> percent, seconds | Done 
      v Subset successful
      
      i percent, seconds | Number of rows in subset
      > percent, seconds | 1
      > percent, seconds | 1
      > percent, seconds | 1
      > percent, seconds | 1
      > percent, seconds | 1
      > percent, seconds | 1
      > percent, seconds | 1
      > percent, seconds | 1
      

---

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/",
        "subset_meddraid", "/"), subset_var = "meddra_id", sv_selection = sv_selection_mid)
    Message
      
      -- tb_subset() -----------------------------------------------------------------
      i Subsetting VigiBase tables.
      ====>-------------------------- percent, seconds | Reading source tables 
      =========>--------------------- percent, seconds | Picking subset 
      ==============>---------------- percent, seconds | Collect case list 
      ===============>--------------- percent, seconds | Collect drug list 
      ==================>------------ percent, seconds | Apply case level subset 
      ========================>------ percent, seconds | Apply drug level subset 
      ==============================> percent, seconds | Done 
      v Subset successful
      
      i percent, seconds | Number of rows in subset
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      

# you can keep suspdup

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/",
        "subset_age_suspdup", "/"), subset_var = "age", sv_selection = c(7, 8),
      rm_suspdup = FALSE)
    Message
      
      -- tb_subset() -----------------------------------------------------------------
      i Subsetting VigiBase tables.
      ====>-------------------------- percent, seconds | Reading source tables 
      =========>--------------------- percent, seconds | Picking subset 
      ==============>---------------- percent, seconds | Collect case list 
      ===============>--------------- percent, seconds | Collect drug list 
      ==================>------------ percent, seconds | Apply case level subset 
      ========================>------ percent, seconds | Apply drug level subset 
      ==============================> percent, seconds | Done 
      v Subset successful
      
      i percent, seconds | Number of rows in subset
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 1
      > percent, seconds | 3
      > percent, seconds | 3
      > percent, seconds | 3
      

---

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/", "subset_age",
        "/"), subset_var = "age", sv_selection = c(7, 8), rm_suspdup = TRUE)
    Message
      
      -- tb_subset() -----------------------------------------------------------------
      i Subsetting VigiBase tables.
      ====>-------------------------- percent, seconds | Reading source tables 
      =========>--------------------- percent, seconds | Picking subset 
      ==============>---------------- percent, seconds | Collect case list 
      ===============>--------------- percent, seconds | Collect drug list 
      ==================>------------ percent, seconds | Apply case level subset 
      ========================>------ percent, seconds | Apply drug level subset 
      ==============================> percent, seconds | Done 
      v Subset successful
      
      i percent, seconds | Number of rows in subset
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      

# alternative syntaxes work

    Code
      tb_subset(wd_in = wd_in, wd_out = paste0(wd_in, "/", "subset_age"), subset_var = "age",
      sv_selection = c(7, 8))
    Message
      
      -- tb_subset() -----------------------------------------------------------------
      i Subsetting VigiBase tables.
      ====>-------------------------- percent, seconds | Reading source tables 
      =========>--------------------- percent, seconds | Picking subset 
      ==============>---------------- percent, seconds | Collect case list 
      ===============>--------------- percent, seconds | Collect drug list 
      ==================>------------ percent, seconds | Apply case level subset 
      ========================>------ percent, seconds | Apply drug level subset 
      ==============================> percent, seconds | Done 
      v Subset successful
      
      i percent, seconds | Number of rows in subset
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      > percent, seconds | 2
      

