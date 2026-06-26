# verbose controls success messages

    Code
      r1 <- get_llt_smq(smq_sel, smq_scope = "narrow", smq_list = smq_list_,
        smq_content = smq_content_, verbose = TRUE)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- v Matched SMQs (number of LLT codes) --
      
      * `embolism`: "Embolic and thrombotic events, venous (SMQ)" (348)
      i Set `verbose` to FALSE to suppress this section.
      --------------------------------------------------------------------------------

# verbose can show matched section with sub-smq and failures

    Code
      r1 <- get_llt_smq(smq_sel, smq_scope = "narrow", smq_list = smq_list_,
        smq_content = smq_content_, verbose = TRUE)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- v Matched SMQs (number of LLT codes) --
      
      * `ischemic_heart_disease`: "Ischaemic heart disease (SMQ) and Myocardial
      infarction (SMQ) and Other ischaemic heart disease (SMQ)" (404)
      i Set `verbose` to FALSE to suppress this section.
      
      -- i Sub-SMQs found 
      
      i High SMQ | v Sub SMQ(s)
      
      * In `ischemic_heart_disease`:
        * i "Ischaemic heart disease (SMQ)" | v Myocardial infarction (SMQ) and Other
        ischaemic heart disease (SMQ)
      
      -- x Unmatched SMQs 
      
      * In `smq_failure`: "Not an SMQ (SMQ)"
      
      --------------------------------------------------------------------------------

# errors and warnings pop as needed

    Code
      r1 <- get_llt_smq(s1, smq_list = smq_list_, smq_content = smq_content_,
        verbose = FALSE)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- i Sub-SMQs found 
      
      i High SMQ | v Sub SMQ(s)
      
      * In `high_level_smq`:
        * i "Ischaemic heart disease (SMQ)" | v Myocardial infarction (SMQ) and Other
        ischaemic heart disease (SMQ)
      
      --------------------------------------------------------------------------------

---

    Code
      r1 <- get_llt_smq(s2, smq_list = smq_list_, smq_content = smq_content_,
        verbose = FALSE)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- x Unmatched SMQs 
      
      * In `wrong_alone`: "ouuioui (SMQ)"
      
      --------------------------------------------------------------------------------

---

    Code
      r2 <- get_llt_smq(s3, smq_list = smq_list_, smq_content = smq_content_,
        verbose = FALSE)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- x Unmatched SMQs 
      
      * In `wrong_inassociationwithagoodone`: "ouuioui (SMQ)"
      
      --------------------------------------------------------------------------------

# works with multiple smqs in a single item

    Code
      adr_llt3 <- get_llt_smq(smq_sel3, smq_scope = "narrow", smq_list_, smq_content_,
        verbose = FALSE)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- i Sub-SMQs found 
      
      i High SMQ | v Sub SMQ(s)
      
      * In `ischemic_heart_disease`:
        * i "Ischaemic heart disease (SMQ)" | v Myocardial infarction (SMQ) and Other
        ischaemic heart disease (SMQ)
      
      --------------------------------------------------------------------------------

---

    Code
      adr_llt4 <- get_llt_smq(smq_sel4, smq_scope = "narrow", smq_list_, smq_content_,
        verbose = FALSE)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- i Sub-SMQs found 
      
      i High SMQ | v Sub SMQ(s)
      
      * In `ischemic_heart_disease`:
        * i "Ischaemic heart disease (SMQ)" | v Myocardial infarction (SMQ) and Other
        ischaemic heart disease (SMQ)
      
      -- x Unmatched SMQs 
      
      * In `smq_failure`: "Not an SMQ (SMQ)"
      
      --------------------------------------------------------------------------------

