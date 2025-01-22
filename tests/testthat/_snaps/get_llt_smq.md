# errors and warnings pop as needed

    Code
      r1 <- get_llt_smq(s1, smq_list = smq_list_, smq_content = smq_content_)
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
      r1 <- get_llt_smq(s2, smq_list = smq_list_, smq_content = smq_content_)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- x Unmatched SMQs 
      
      * In `wrong_alone`: "ouuioui (SMQ)"
      
      --------------------------------------------------------------------------------

---

    Code
      r2 <- get_llt_smq(s3, smq_list = smq_list_, smq_content = smq_content_)
    Message
      
      -- get_llt_smq() ---------------------------------------------------------------
      
      -- x Unmatched SMQs 
      
      * In `wrong_inassociationwithagoodone`: "ouuioui (SMQ)"
      
      --------------------------------------------------------------------------------

# works with multiple smqs in a single item

    Code
      adr_llt3 <- get_llt_smq(smq_sel3, smq_scope = "narrow", smq_list_, smq_content_)
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
      adr_llt4 <- get_llt_smq(smq_sel4, smq_scope = "narrow", smq_list_, smq_content_)
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

