# basic use and here package works

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub, force = TRUE,
        overwrite_existing_tables = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      v All expected csv files found in `path_base` and `path_sub`
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.csv 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.csv 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.csv 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.csv 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.csv 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.csv 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.csv 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.csv 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.csv 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.csv 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.csv 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.csv 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.csv 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.csv 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.csv 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.csv 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.csv 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.csv 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.csv 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.csv 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.csv 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.csv 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.csv 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.csv 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.csv 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.csv 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.csv 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.csv 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.csv 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.csv 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
      =============================>- percent, seconds | Write ind.parquet 
      ==============================> percent, seconds | Process Subsidiary files 
      ==============================> percent, seconds | Done 
      

# tb_screen_main and tb_screen_sub skip existing tables and overwrite_existing_tables works

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      v All expected csv files found in `path_base` and `path_sub`
      i Checking for existing tables.
      The following tables were found as .parquet files in `path_base`: "adr.parquet", "demo.parquet", "drug.parquet", "followup.parquet", "link.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were found as .parquet files.
      i These tables won't be built again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Checking for existing tables.
      The following tables were found as .parquet files in `path_base`: "adr.parquet", "demo.parquet", "drug.parquet", "followup.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were found as .parquet files.
      i These tables won't be built again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Checking for existing tables.
      The following tables were found as .parquet files in `path_base`: "demo.parquet", "drug.parquet", "followup.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were found as .parquet files.
      i These tables won't be built again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Checking for existing tables.
      The following tables were found as .parquet files in `path_base`: "demo.parquet", "followup.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were found as .parquet files.
      i These tables won't be built again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Checking for existing tables.
      The following tables were found as .parquet files in `path_base`: "adr.parquet", "demo.parquet", "drug.parquet", "followup.parquet", "ind.parquet", "link.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      i These tables won't be built again.
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
      v All expected csv files found in `path_base` and `path_sub`
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      >------------------------------ percent, seconds | Read SUSPECTEDDUPLICATES.csv 
      =>----------------------------- percent, seconds | Write suspdup.parquet 
      =>----------------------------- percent, seconds | Read DEMO.csv 
      ==>---------------------------- percent, seconds | Remove duplicates 
      ==>---------------------------- percent, seconds | Write demo.parquet 
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Remove duplicates 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      =====>------------------------- percent, seconds | Read FOLLOWUP.csv 
      ======>------------------------ percent, seconds | Remove duplicates 
      ======>------------------------ percent, seconds | Write followup.parquet 
      =======>----------------------- percent, seconds | Read ADR.csv 
      ========>---------------------- percent, seconds | Remove duplicates 
      ========>---------------------- percent, seconds | Write adr.parquet 
      ========>---------------------- percent, seconds | Read OUT.csv 
      =========>--------------------- percent, seconds | Remove duplicates 
      =========>--------------------- percent, seconds | Write out.parquet 
      =========>--------------------- percent, seconds | Read SRCE.csv 
      ==========>-------------------- percent, seconds | Remove duplicates 
      ==========>-------------------- percent, seconds | Write srce.parquet 
      ==========>-------------------- percent, seconds | Read LINK.csv 
      ===========>------------------- percent, seconds | Process link (longest step) 
      ======================>-------- percent, seconds | Remove duplicates 
      ======================>-------- percent, seconds | Write link.parquet 
      ======================>-------- percent, seconds | Read IND.csv 
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
      v All expected csv files found in `path_base` and `path_sub`
      i Checking for existing tables.
      The following tables were found as .parquet files in `path_base`: "adr.parquet", "demo.parquet", "followup.parquet", "ind.parquet", "link.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were found as .parquet files.
      i These tables won't be built again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      ===>--------------------------- percent, seconds | Read DRUG.csv 
      ====>-------------------------- percent, seconds | Write drug.parquet 
      ==============================> percent, seconds | Write drug.parquet 
      

# csv files are detected and required in path_base

    Code
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      v All expected csv files found in `path_base` and `path_sub`
      i Checking for existing tables.
      The following tables were found as .parquet files in `path_base`: "adr.parquet", "demo.parquet", "drug.parquet", "followup.parquet", "ind.parquet", "link.parquet", "out.parquet", "srce.parquet", and "suspdup.parquet"
      Subsidiary files were found as .parquet files.
      i These tables won't be built again.
      > Set `overwrite_existing_tables` to TRUE to rewrite them.
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.

---

    Code
      tb_vigibase(path_base = path_base, path_sub = path_sub,
        overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
    Condition
      Error in `tb_vigibase()`:
      ! All csv files must be present in `path_base` and `path_sub`.
      x Missing file: IND.csv.

---

    Code
      tb_vigibase(path_base = tempdir(), path_sub = path_sub,
      overwrite_existing_tables = FALSE, force = TRUE)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
    Condition
      Error in `tb_vigibase()`:
      ! All csv files must be present in `path_base` and `path_sub`.
      i As of vigicaen 1.1.0, input tables must be in .csv format.

