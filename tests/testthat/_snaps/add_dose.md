# finding or not finding drug dose displays correctly

    Code
      demo <- add_dose(.data = demo_, d_code = d_code[1], drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- v Drug dose found in mg/day 
      
      * `paracetamol`: 4 rows
      i Important: Check dose results,
      coding issues are common for drug dose.
      Some values may seem unreliable.
      
      i Dose summary (mg/day) - median (Q1-Q3) [min-max]
      * `paracetamol`: 512.0 (24.0-1,250.0) [24.0-2,000.0]
      

---

    Code
      demo <- add_dose(.data = demo_, d_code = d_code[2], drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- x No drug dose found in mg/day 
      
      * `unknown_drug`
      i Other dosage regimens not supported in `add_dose()`.
      

---

    Code
      demo <- add_dose(.data = demo_, d_code = d_code, drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- x No drug dose found in mg/day 
      
      * `unknown_drug`
      i Other dosage regimens not supported in `add_dose()`.
      
      -- v Drug dose found in mg/day 
      
      * `paracetamol`: 4 rows
      * `para2`: 4 rows
      i Important: Check dose results,
      coding issues are common for drug dose.
      Some values may seem unreliable.
      
      i Dose summary (mg/day) - median (Q1-Q3) [min-max]
      * `paracetamol`: 512.0 (24.0-1,250.0) [24.0-2,000.0]
      * `para2`: 512.0 (24.0-1,250.0) [24.0-2,000.0]
      

---

    Code
      demo <- add_dose(.data = demo_, d_code = d_code[c(1, 2, 3, 1, 2)], drug_data = drug_)
    Message
      i `.data` detected as `demo` table.
      
      -- x No drug dose found in mg/day 
      
      * `unknown_drug`
      * `unknown_drug`
      i Other dosage regimens not supported in `add_dose()`.
      
      -- v Drug dose found in mg/day 
      
      * `paracetamol`: 4 rows
      * `para2`: 4 rows
      * `paracetamol`: 4 rows
      i Important: Check dose results,
      coding issues are common for drug dose.
      Some values may seem unreliable.
      
      i Dose summary (mg/day) - median (Q1-Q3) [min-max]
      * `paracetamol`: 512.0 (24.0-1,250.0) [24.0-2,000.0]
      * `para2`: 512.0 (24.0-1,250.0) [24.0-2,000.0]
      * `paracetamol`: 512.0 (24.0-1,250.0) [24.0-2,000.0]
      

---

    Code
      demo <- add_dose(.data = demo_, d_code = d_code[1], drug_data = drug_, verbose = FALSE)
    Message
      i `.data` detected as `demo` table.

---

    Code
      demo <- add_dose(.data = demo_, d_code = d_code[2], drug_data = drug_, verbose = FALSE)
    Message
      i `.data` detected as `demo` table.
      
      -- x No drug dose found in mg/day 
      
      * `unknown_drug`
      i Other dosage regimens not supported in `add_dose()`.
      

