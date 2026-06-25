# verbose reports unmatched ATC classes

    Code
      d1 <- get_atc_code(atc_sel = atc_sel, mp = mp_, thg_data = thg_, vigilyze = FALSE,
        verbose = TRUE)
    Message
      
      -- get_atc_code() --------------------------------------------------------------
      
      -- v Matched ATC classes (`atc_sel`) --
      
      > `item_with_match`: 389 Record_Id values
      i vigilyze set to FALSE, extracting Record_Ids (?get_atc_code for details)
      i Set `verbose` to FALSE to suppress this section.
      
      -- x Unmatched ATC classes --
      
      * In `item_with_match`: "NOT_AN_ATC"
      * In `item_without_match`: "NOPE"
      --------------------------------------------------------------------------------

# matched section is hidden if all atc are unmatched

    Code
      d1 <- get_atc_code(atc_sel = atc_sel, mp = mp_, thg_data = thg_, vigilyze = FALSE,
        verbose = TRUE)
    Message
      
      -- get_atc_code() --------------------------------------------------------------
      
      -- x Unmatched ATC classes --
      
      * In `no_match`: "NOPE" and "STILL_NOPE"
      --------------------------------------------------------------------------------

