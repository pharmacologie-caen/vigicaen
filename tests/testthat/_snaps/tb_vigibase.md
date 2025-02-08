# basic use and here package works

    Code
      options(cli.progress_show_after = 0)
      options(cli.progress_clear = FALSE)
      tb_vigibase(path_base = path_base, path_sub = path_sub)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ====>-------------------------- percent, seconds | Write demo.parquet 
      =====>------------------------- percent, seconds | Read DRUG.txt 
      ======>------------------------ percent, seconds | Split drug 
      ========>---------------------- percent, seconds | Write drug.parquet 
      =========>--------------------- percent, seconds | Read FOLLOWUP.txt 
      ==========>-------------------- percent, seconds | Split followup 
      ==========>-------------------- percent, seconds | Write followup.parquet 
      ===========>------------------- percent, seconds | Read ADR.txt 
      ============>------------------ percent, seconds | Split adr 
      =============>----------------- percent, seconds | Write adr.parquet 
      =============>----------------- percent, seconds | Read OUT.txt 
      ==============>---------------- percent, seconds | Split out 
      ==============>---------------- percent, seconds | Write out.parquet 
      ==============>---------------- percent, seconds | Read SRCE.txt 
      ==============>---------------- percent, seconds | Split srce 
      ===============>--------------- percent, seconds | Write srce.parquet 
      ===============>--------------- percent, seconds | Read LINK.txt 
      ================>-------------- percent, seconds | Split link (longest step) 
      ====================>---------- percent, seconds | Write link.parquet 
      =====================>--------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind 
      =======================>------- percent, seconds | Write ind.parquet 
      ========================>------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =========================>----- percent, seconds | Split suspdup 
      =========================>----- percent, seconds | Write suspdup.parquet 
      ==========================>---- percent, seconds | Process AgeGroup_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge2_Lx.txt 
      ==========================>---- percent, seconds | Process Frequency_Lx.txt 
      ===========================>--- percent, seconds | Process Gender_Lx.txt 
      ===========================>--- percent, seconds | Process Notifier_Lx.txt 
      ===========================>--- percent, seconds | Process Outcome_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge2_Lx.txt 
      ============================>-- percent, seconds | Process Region_Lx.txt 
      ============================>-- percent, seconds | Process RepBasis_Lx.txt 
      =============================>- percent, seconds | Process ReportType_Lx.txt 
      =============================>- percent, seconds | Process RouteOfAdm_Lx.txt 
      =============================>- percent, seconds | Process Seriousness_Lx.txt 
      ==============================> percent, seconds | Process SizeUnit_Lx.txt 
      ==============================> percent, seconds | Done 
      

---

    Code
      tb_vigibase(path_base = here_path_base, path_sub = here_path_sub)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ====>-------------------------- percent, seconds | Write demo.parquet 
      =====>------------------------- percent, seconds | Read DRUG.txt 
      ======>------------------------ percent, seconds | Split drug 
      ========>---------------------- percent, seconds | Write drug.parquet 
      =========>--------------------- percent, seconds | Read FOLLOWUP.txt 
      ==========>-------------------- percent, seconds | Split followup 
      ==========>-------------------- percent, seconds | Write followup.parquet 
      ===========>------------------- percent, seconds | Read ADR.txt 
      ============>------------------ percent, seconds | Split adr 
      =============>----------------- percent, seconds | Write adr.parquet 
      =============>----------------- percent, seconds | Read OUT.txt 
      ==============>---------------- percent, seconds | Split out 
      ==============>---------------- percent, seconds | Write out.parquet 
      ==============>---------------- percent, seconds | Read SRCE.txt 
      ==============>---------------- percent, seconds | Split srce 
      ===============>--------------- percent, seconds | Write srce.parquet 
      ===============>--------------- percent, seconds | Read LINK.txt 
      ================>-------------- percent, seconds | Split link (longest step) 
      ====================>---------- percent, seconds | Write link.parquet 
      =====================>--------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind 
      =======================>------- percent, seconds | Write ind.parquet 
      ========================>------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =========================>----- percent, seconds | Split suspdup 
      =========================>----- percent, seconds | Write suspdup.parquet 
      ==========================>---- percent, seconds | Process AgeGroup_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge2_Lx.txt 
      ==========================>---- percent, seconds | Process Frequency_Lx.txt 
      ===========================>--- percent, seconds | Process Gender_Lx.txt 
      ===========================>--- percent, seconds | Process Notifier_Lx.txt 
      ===========================>--- percent, seconds | Process Outcome_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge2_Lx.txt 
      ============================>-- percent, seconds | Process Region_Lx.txt 
      ============================>-- percent, seconds | Process RepBasis_Lx.txt 
      =============================>- percent, seconds | Process ReportType_Lx.txt 
      =============================>- percent, seconds | Process RouteOfAdm_Lx.txt 
      =============================>- percent, seconds | Process Seriousness_Lx.txt 
      ==============================> percent, seconds | Process SizeUnit_Lx.txt 
      ==============================> percent, seconds | Done 
      

---

    Code
      tb_vigibase(path_base = path_base, path_sub = here_path_sub)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ====>-------------------------- percent, seconds | Write demo.parquet 
      =====>------------------------- percent, seconds | Read DRUG.txt 
      ======>------------------------ percent, seconds | Split drug 
      ========>---------------------- percent, seconds | Write drug.parquet 
      =========>--------------------- percent, seconds | Read FOLLOWUP.txt 
      ==========>-------------------- percent, seconds | Split followup 
      ==========>-------------------- percent, seconds | Write followup.parquet 
      ===========>------------------- percent, seconds | Read ADR.txt 
      ============>------------------ percent, seconds | Split adr 
      =============>----------------- percent, seconds | Write adr.parquet 
      =============>----------------- percent, seconds | Read OUT.txt 
      ==============>---------------- percent, seconds | Split out 
      ==============>---------------- percent, seconds | Write out.parquet 
      ==============>---------------- percent, seconds | Read SRCE.txt 
      ==============>---------------- percent, seconds | Split srce 
      ===============>--------------- percent, seconds | Write srce.parquet 
      ===============>--------------- percent, seconds | Read LINK.txt 
      ================>-------------- percent, seconds | Split link (longest step) 
      ====================>---------- percent, seconds | Write link.parquet 
      =====================>--------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind 
      =======================>------- percent, seconds | Write ind.parquet 
      ========================>------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =========================>----- percent, seconds | Split suspdup 
      =========================>----- percent, seconds | Write suspdup.parquet 
      ==========================>---- percent, seconds | Process AgeGroup_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge2_Lx.txt 
      ==========================>---- percent, seconds | Process Frequency_Lx.txt 
      ===========================>--- percent, seconds | Process Gender_Lx.txt 
      ===========================>--- percent, seconds | Process Notifier_Lx.txt 
      ===========================>--- percent, seconds | Process Outcome_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge2_Lx.txt 
      ============================>-- percent, seconds | Process Region_Lx.txt 
      ============================>-- percent, seconds | Process RepBasis_Lx.txt 
      =============================>- percent, seconds | Process ReportType_Lx.txt 
      =============================>- percent, seconds | Process RouteOfAdm_Lx.txt 
      =============================>- percent, seconds | Process Seriousness_Lx.txt 
      ==============================> percent, seconds | Process SizeUnit_Lx.txt 
      ==============================> percent, seconds | Done 
      

---

    Code
      tb_vigibase(path_base = here_path_base, path_sub = path_sub)
    Message
      
      -- tb_vigibase() ---------------------------------------------------------------
      i Creating vigibase tables.
      This process must only be done once per database version.
      It can take up to 30minutes.
      =>----------------------------- percent, seconds | Read DEMO.txt 
      ==>---------------------------- percent, seconds | Split demo 
      ====>-------------------------- percent, seconds | Write demo.parquet 
      =====>------------------------- percent, seconds | Read DRUG.txt 
      ======>------------------------ percent, seconds | Split drug 
      ========>---------------------- percent, seconds | Write drug.parquet 
      =========>--------------------- percent, seconds | Read FOLLOWUP.txt 
      ==========>-------------------- percent, seconds | Split followup 
      ==========>-------------------- percent, seconds | Write followup.parquet 
      ===========>------------------- percent, seconds | Read ADR.txt 
      ============>------------------ percent, seconds | Split adr 
      =============>----------------- percent, seconds | Write adr.parquet 
      =============>----------------- percent, seconds | Read OUT.txt 
      ==============>---------------- percent, seconds | Split out 
      ==============>---------------- percent, seconds | Write out.parquet 
      ==============>---------------- percent, seconds | Read SRCE.txt 
      ==============>---------------- percent, seconds | Split srce 
      ===============>--------------- percent, seconds | Write srce.parquet 
      ===============>--------------- percent, seconds | Read LINK.txt 
      ================>-------------- percent, seconds | Split link (longest step) 
      ====================>---------- percent, seconds | Write link.parquet 
      =====================>--------- percent, seconds | Read IND.txt 
      ======================>-------- percent, seconds | Split ind 
      =======================>------- percent, seconds | Write ind.parquet 
      ========================>------ percent, seconds | Read SUSPECTEDDUPLICATES.txt 
      =========================>----- percent, seconds | Split suspdup 
      =========================>----- percent, seconds | Write suspdup.parquet 
      ==========================>---- percent, seconds | Process AgeGroup_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge_Lx.txt 
      ==========================>---- percent, seconds | Process Dechallenge2_Lx.txt 
      ==========================>---- percent, seconds | Process Frequency_Lx.txt 
      ===========================>--- percent, seconds | Process Gender_Lx.txt 
      ===========================>--- percent, seconds | Process Notifier_Lx.txt 
      ===========================>--- percent, seconds | Process Outcome_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge_Lx.txt 
      ============================>-- percent, seconds | Process Rechallenge2_Lx.txt 
      ============================>-- percent, seconds | Process Region_Lx.txt 
      ============================>-- percent, seconds | Process RepBasis_Lx.txt 
      =============================>- percent, seconds | Process ReportType_Lx.txt 
      =============================>- percent, seconds | Process RouteOfAdm_Lx.txt 
      =============================>- percent, seconds | Process Seriousness_Lx.txt 
      ==============================> percent, seconds | Process SizeUnit_Lx.txt 
      ==============================> percent, seconds | Done 
      

