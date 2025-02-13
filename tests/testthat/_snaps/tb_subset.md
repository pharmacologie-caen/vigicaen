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
      

# error if no wd_in

    Code
      tb_subset(wd_in = "that_dir_doesnt_exist", sv_selection = list(a = c(1, 2)))
    Condition <no_dir>
      Error in `tb_subset()`:
      ! `wd_in` must exist.
      x "that_dir_doesnt_exist" does not exist.

# printing error no dir [plain]

    Code
      error_dir_exists("wd_in", "/wrong_path/")
    Condition
      Error:
      ! `wd_in` must exist.
      x "/wrong_path/" does not exist.

# printing error no dir [ansi]

    Code
      error_dir_exists("wd_in", "/wrong_path/")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `wd_in` must exist.
      [31mx[39m [34m"/wrong_path/"[39m does not exist.

# printing error no dir [unicode]

    Code
      error_dir_exists("wd_in", "/wrong_path/")
    Condition
      Error:
      ! `wd_in` must exist.
      ✖ "/wrong_path/" does not exist.

# printing error no dir [fancy]

    Code
      error_dir_exists("wd_in", "/wrong_path/")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `wd_in` must exist.
      [31m✖[39m [34m"/wrong_path/"[39m does not exist.

