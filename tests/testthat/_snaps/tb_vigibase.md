# basic use and here package works

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub, force = TRUE,
        overwrite_existing_tables = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =>----------------------------- percent, seconds | Split suspdup 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.txt 
      =====>------------------------- percent, seconds | Split followup 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.txt 
      ========>---------------------- percent, seconds | Split out 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.txt 
      ==========>-------------------- percent, seconds | Split srce 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

---

    Code
      tb_vigibase(path_base = here_path_base, path_sub = here_path_sub, force = TRUE,
        overwrite_existing_tables = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =>----------------------------- percent, seconds | Split suspdup 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.txt 
      =====>------------------------- percent, seconds | Split followup 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.txt 
      ========>---------------------- percent, seconds | Split out 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.txt 
      ==========>-------------------- percent, seconds | Split srce 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

---

    Code
      tb_vigibase(path_base = path_base, path_sub = here_path_sub, force = TRUE,
        overwrite_existing_tables = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =>----------------------------- percent, seconds | Split suspdup 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.txt 
      =====>------------------------- percent, seconds | Split followup 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.txt 
      ========>---------------------- percent, seconds | Split out 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.txt 
      ==========>-------------------- percent, seconds | Split srce 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

---

    Code
      tb_vigibase(path_base = here_path_base, path_sub = path_sub, force = TRUE,
        overwrite_existing_tables = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =>----------------------------- percent, seconds | Split suspdup 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.txt 
      =====>------------------------- percent, seconds | Split followup 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.txt 
      ========>---------------------- percent, seconds | Split out 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.txt 
      ==========>-------------------- percent, seconds | Split srce 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

# path_base and path_sub exist before working on tables

    Code
      tb_vigibase(path_base = wrong_path, path_sub = wrong_path, force = TRUE)
    Condition <no_dir>
      Error in `tb_vigibase()`:
      ! `path_base` must exist.
      x "/a/wrong/filepath/" does not exist.

# rm_suspdup removes suspected duplicates in main tables

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub, force = TRUE,
        overwrite_existing_tables = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =>----------------------------- percent, seconds | Split suspdup 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.txt 
      =====>------------------------- percent, seconds | Split followup 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.txt 
      ========>---------------------- percent, seconds | Split out 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.txt 
      ==========>-------------------- percent, seconds | Split srce 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

---

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub, force = TRUE,
        rm_suspdup = FALSE, overwrite_existing_tables = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =>----------------------------- percent, seconds | Split suspdup 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.txt 
      =====>------------------------- percent, seconds | Split followup 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.txt 
      ========>---------------------- percent, seconds | Split out 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.txt 
      ==========>-------------------- percent, seconds | Split srce 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

# tb_screen_main_parquet and tb_screen_sub_parquet skip existing tables and overwrite_existing_tables works

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Checking for existing tables.
      The following tables were already found as parquet files in `path_base`: "adr.parquet", "demo.parquet", "drug.parquet", "followup.parquet", "link.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were already found as parquet files.
      i These tables won't be build again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Write ind.parquet 
      

---

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Checking for existing tables.
      The following tables were already found as parquet files in `path_base`: "adr.parquet", "demo.parquet", "drug.parquet", "followup.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were already found as parquet files.
      i These tables won't be build again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Write ind.parquet 
      

---

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, rm_suspdup = TRUE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Checking for existing tables.
      The following tables were already found as parquet files in `path_base`: "demo.parquet", "drug.parquet", "followup.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were already found as parquet files.
      i These tables won't be build again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Write ind.parquet 
      

---

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Checking for existing tables.
      The following tables were already found as parquet files in `path_base`: "demo.parquet", "followup.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were already found as parquet files.
      i These tables won't be build again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Write ind.parquet 
      

---

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Checking for existing tables.
      The following tables were already found as parquet files in `path_base`: "adr.parquet", "demo.parquet", "drug.parquet", "followup.parquet", "ind.parquet", "link.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      i These tables won't be build again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

---

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = TRUE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =>----------------------------- percent, seconds | Split suspdup 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.txt 
      =====>------------------------- percent, seconds | Split followup 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.txt 
      =======>----------------------- percent, seconds | Split adr 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.txt 
      ========>---------------------- percent, seconds | Split out 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.txt 
      ==========>-------------------- percent, seconds | Split srce 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.txt 
      ===========>------------------- percent, seconds | Split link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind (2nd longest step) 
      =======================>------- percent, seconds | Remove duplicates 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

---

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub, rm_suspdup = FALSE,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Checking for existing tables.
      The following tables were already found as parquet files in `path_base`: "adr.parquet", "demo.parquet", "followup.parquet", "ind.parquet", "link.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were already found as parquet files.
      i These tables won't be build again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ===>--------------------------- percent, seconds | Read DRUG.txt 
      ====>-------------------------- percent, seconds | Split drug 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      ==============================> percent, seconds | Write drug.parquet 
      

