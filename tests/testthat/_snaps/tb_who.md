# basic use works

    Code
      tb_who(path_who = path_who, force = TRUE)
    Message
      
      -- tb_who() --------------------------------------------------------------------
      i Creating WHO Drug tables.
      This process must only be done once per database version.
      =>----------------------------- percent, seconds | Read MP.csv 
      ====>-------------------------- percent, seconds | Write mp.parquet 
      =========>--------------------- percent, seconds | Read ThG.csv 
      ==========>-------------------- percent, seconds | Write thg.parquet 
      ===========>------------------- percent, seconds | Read ING.csv 
      ==============>---------------- percent, seconds | Write ing.parquet 
      ===============>--------------- percent, seconds | Read SRCE.csv 
      ================>-------------- percent, seconds | Write srce.parquet 
      =================>------------- percent, seconds | Read ORG.csv 
      ==================>------------ percent, seconds | Write org.parquet 
      ===================>----------- percent, seconds | Read CCODE.csv 
      ======================>-------- percent, seconds | Write ccode.parquet 
      ========================>------ percent, seconds | Read ATC.csv 
      ========================>------ percent, seconds | Write atc.parquet 
      =========================>----- percent, seconds | Read SUN.csv 
      =========================>----- percent, seconds | Write sun.parquet 
      ==========================>---- percent, seconds | Read PF.csv 
      ==========================>---- percent, seconds | Write pf.parquet 
      ==========================>---- percent, seconds | Read STR.csv 
      ===========================>--- percent, seconds | Write str.parquet 
      ============================>-- percent, seconds | Read PRT.csv 
      =============================>- percent, seconds | Write prt.parquet 
      =============================>- percent, seconds | Read Unit-X.csv 
      ==============================> percent, seconds | Write unitx.parquet 
      ==============================> percent, seconds | Done 
      

---

    Code
      tb_who(path_who = path_who_no_slash, force = TRUE)
    Message
      
      -- tb_who() --------------------------------------------------------------------
      i Creating WHO Drug tables.
      This process must only be done once per database version.
      =>----------------------------- percent, seconds | Read MP.csv 
      ====>-------------------------- percent, seconds | Write mp.parquet 
      =========>--------------------- percent, seconds | Read ThG.csv 
      ==========>-------------------- percent, seconds | Write thg.parquet 
      ===========>------------------- percent, seconds | Read ING.csv 
      ==============>---------------- percent, seconds | Write ing.parquet 
      ===============>--------------- percent, seconds | Read SRCE.csv 
      ================>-------------- percent, seconds | Write srce.parquet 
      =================>------------- percent, seconds | Read ORG.csv 
      ==================>------------ percent, seconds | Write org.parquet 
      ===================>----------- percent, seconds | Read CCODE.csv 
      ======================>-------- percent, seconds | Write ccode.parquet 
      ========================>------ percent, seconds | Read ATC.csv 
      ========================>------ percent, seconds | Write atc.parquet 
      =========================>----- percent, seconds | Read SUN.csv 
      =========================>----- percent, seconds | Write sun.parquet 
      ==========================>---- percent, seconds | Read PF.csv 
      ==========================>---- percent, seconds | Write pf.parquet 
      ==========================>---- percent, seconds | Read STR.csv 
      ===========================>--- percent, seconds | Write str.parquet 
      ============================>-- percent, seconds | Read PRT.csv 
      =============================>- percent, seconds | Write prt.parquet 
      =============================>- percent, seconds | Read Unit-X.csv 
      ==============================> percent, seconds | Write unitx.parquet 
      ==============================> percent, seconds | Done 
      

# path_who exists before working on tables

    Code
      tb_who(path_who = wrong_path, force = TRUE)
    Condition <no_dir>
      Error in `tb_who()`:
      ! `path_who` must exist.
      x "/a/wrong/filepath/" does not exist.

