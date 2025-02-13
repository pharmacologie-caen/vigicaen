# you can use arrow/parquet format

    Code
      res <- add_dose(demo_parquet, d_code = d_drecno_test, d_names = names(
        d_drecno_test), method = "DrecNo", repbasis = "sci", drug_data = drug_parquet)
    Message
      i `.data` detected as `demo` table.
      i Number of lines with a posology in mg/day found per drug:
      i nivolumab: 1 lines with a posology in mg/day
      i Important: Please check the results for posology, as coding issues are common. Some results may seem unbelievable and should be carefully reviewed and trimmed.
      i Summary of added dose columns:
    Output
       nivolumab_dose_mg_per_day
       Min.   :246              
       1st Qu.:246              
       Median :246              
       Mean   :246              
       3rd Qu.:246              
       Max.   :246              
       NA's   :4                

---

    Code
      res_a <- add_dose(demo_test, d_code = d_drecno_test, d_names = names(
        d_drecno_test), method = "DrecNo", repbasis = "sci", drug_data = drug_test)
    Message
      i `.data` detected as `demo` table.
      i Number of lines with a posology in mg/day found per drug:
      i nivolumab: 1 lines with a posology in mg/day
      i Important: Please check the results for posology, as coding issues are common. Some results may seem unbelievable and should be carefully reviewed and trimmed.
      i Summary of added dose columns:
    Output
       nivolumab_dose_mg_per_day
       Min.   :246              
       1st Qu.:246              
       Median :246              
       Mean   :246              
       3rd Qu.:246              
       Max.   :246              
       NA's   :4                

# works with mpi_list

    Code
      demo <- add_dose(demo_, d_code = mpi, d_names = names(mpi), method = "MedicinalProd_Id",
      repbasis = "sci", drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      i Number of lines with a posology in mg/day found per drug:
      i para: 2 lines with a posology in mg/day
      i Important: Please check the results for posology, as coding issues are common. Some results may seem unbelievable and should be carefully reviewed and trimmed.
      i Summary of added dose columns:
    Output
       para_dose_mg_per_day
       Min.   : 975        
       1st Qu.:1031        
       Median :1088        
       Mean   :1088        
       3rd Qu.:1144        
       Max.   :1200        
       NA's   :748         

