# finding or not finding drug dose displays correctly

    Code
      demo <- add_dose(.data = demo_, d_code = d_code[1], drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- v Drug dose found in mg/day 
      
      * `paracetamol`: 4 rows
      i Important: Please check the results for posology,
      as coding issues are common.
      Some results may seem unreliable.
      
      i Summary of added dose columns:
    Output
                                var level                               value n_avail
      1 paracetamol_dose_mg_per_day  <NA> 512.0 (24.0-1,250.0) [24.0-2,000.0]       4

---

    Code
      demo <- add_dose(.data = demo_, d_code = d_code[2], drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- x No drug dose found in mg/day 
      
      * `unknown_drug`
      i Other posology schemas are not supported in add_dose()).
      

---

    Code
      demo <- add_dose(.data = demo_, d_code = d_code, drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- x No drug dose found in mg/day 
      
      * `unknown_drug`
      i Other posology schemas are not supported in add_dose()).
      
      -- v Drug dose found in mg/day 
      
      * `paracetamol`: 4 rows
      i Important: Please check the results for posology,
      as coding issues are common.
      Some results may seem unreliable.
      
      i Summary of added dose columns:
    Output
                                var level                               value n_avail
      1 paracetamol_dose_mg_per_day  <NA> 512.0 (24.0-1,250.0) [24.0-2,000.0]       4

# works with mpi_list

    Code
      demo <- add_dose(demo_, d_code = mpi, d_dose_names = names(mpi), method = "MedicinalProd_Id",
      repbasis = "sci", drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- v Drug dose found in mg/day 
      
      * `para`: 2 rows
      i Important: Please check the results for posology,
      as coding issues are common.
      Some results may seem unreliable.
      
      i Summary of added dose columns:
    Output
                         var level                                     value n_avail
      1 para_dose_mg_per_day  <NA> 1,087.5 (1,031.2-1,143.8) [975.0-1,200.0]       2

