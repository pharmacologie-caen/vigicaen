# can process meddra tables

    Code
      tb_meddra(path_meddra)
    Message
      
      -- tb_meddra() -----------------------------------------------------------------
      i Creating MedDRA tables.
      This process must only be done once per database version.
      ====>-------------------------- percent, seconds | Read llt.asc 
      =========>--------------------- percent, seconds | Read mdhier.asc 
      ==============>---------------- percent, seconds | Write meddra_hierarchy.parquet 
      ==================>------------ percent, seconds | Read smq_list.asc 
      =====================>--------- percent, seconds | Write smq_list.parquet 
      ========================>------ percent, seconds | Read smq_content.asc 
      ===========================>--- percent, seconds | Write smq_content.parquet 
      ==============================> percent, seconds | Done 
      

# format is ok [plain]

    Code
      msg_tb_onceperdatabase()
    Message
      This process must only be done once per database version.

# format is ok [ansi]

    Code
      msg_tb_onceperdatabase()
    Message
      [1m[22mThis process must only be done [1m[33monce[39m[22m per database version.

# format is ok [unicode]

    Code
      msg_tb_onceperdatabase()
    Message
      This process must only be done once per database version.

# format is ok [fancy]

    Code
      msg_tb_onceperdatabase()
    Message
      [1m[22mThis process must only be done [1m[33monce[39m[22m per database version.

