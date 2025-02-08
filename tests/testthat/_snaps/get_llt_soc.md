# verbose works

    Code
      r1 <- get_llt_soc(good_list, term_level = "pt", meddra = meddra_)
    Message
      
      -- get_llt_soc() ---------------------------------------------------------------
      
      -- v Matched reactions at `pt` level (number of codes) --
      
      > `item1`: "Colitis (25)"
      > `item2`: "Organising pneumonia (9)" and "Pneumonitis (6)"
      
      i Set `verbose` to FALSE to suppress this section.
      

# unmatched terms management

    Code
      r1 <- get_llt_soc(wrong_without_capital, term_level = "pt", meddra = meddra_,
        verbose = FALSE)
    Message
      
      -- get_llt_soc() ---------------------------------------------------------------
      
      -- x Unmatched reactions --
      
      -- ! Some reactions did not start with a Capital letter 
      
      * In `rate`: x "youps"
      

---

    Code
      r1 <- get_llt_soc(wrong_with_capital, term_level = "pt", meddra = meddra_,
        verbose = FALSE)
    Message
      
      -- get_llt_soc() ---------------------------------------------------------------
      
      -- x Unmatched reactions --
      
      -- ! Some reactions were not found at `pt` level 
      
      * In `rate`: x "Youps"
      

---

    Code
      r1 <- get_llt_soc(wrong_list, term_level = "pt", meddra = meddra_, verbose = FALSE)
    Message
      
      -- get_llt_soc() ---------------------------------------------------------------
      
      -- x Unmatched reactions --
      
      -- ! Some reactions were not found at `pt` level 
      
      * In `rate`: x "Youps"
      
      -- ! Some reactions did not start with a Capital letter 
      
      * In `rate`: x "youps"
      * In `encorerate`: x "another" and "yet another"
      

