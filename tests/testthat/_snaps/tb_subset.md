# you can subset on drecno, age, meddra_id

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/",
        "subset_drecno", "/"), subset_var = "drecno", sv_selection = sv_selection_drecno)
    Output
      demo.parquet subset has 2 rows.
      adr.parquet subset has 2 rows.
      out.parquet subset has 2 rows.
      srce.parquet subset has 2 rows.
      followup.parquet subset has 2 rows.
      drug.parquet subset has 2 rows.
      link.parquet subset has 2 rows.
      ind.parquet subset has 2 rows.

---

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/", "subset_age",
        "/"), subset_var = "age", sv_selection = c(7, 8))
    Output
      demo.parquet subset has 1 rows.
      adr.parquet subset has 1 rows.
      out.parquet subset has 1 rows.
      srce.parquet subset has 1 rows.
      followup.parquet subset has 1 rows.
      drug.parquet subset has 1 rows.
      link.parquet subset has 1 rows.
      ind.parquet subset has 1 rows.

---

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/",
        "subset_meddraid", "/"), subset_var = "meddra_id", sv_selection = sv_selection_mid)
    Output
      demo.parquet subset has 2 rows.
      adr.parquet subset has 2 rows.
      out.parquet subset has 2 rows.
      srce.parquet subset has 2 rows.
      followup.parquet subset has 2 rows.
      drug.parquet subset has 2 rows.
      link.parquet subset has 2 rows.
      ind.parquet subset has 2 rows.

# you can keep suspdup

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/",
        "subset_age_suspdup", "/"), subset_var = "age", sv_selection = c(7, 8),
      rm_suspdup = FALSE)
    Output
      demo.parquet subset has 3 rows.
      adr.parquet subset has 3 rows.
      out.parquet subset has 3 rows.
      srce.parquet subset has 3 rows.
      followup.parquet subset has 3 rows.
      suspdup.parquet subset has 1 rows.
      drug.parquet subset has 3 rows.
      link.parquet subset has 3 rows.
      ind.parquet subset has 3 rows.

---

    Code
      tb_subset(wd_in = paste0(wd_in, "/"), wd_out = paste0(wd_in, "/", "subset_age",
        "/"), subset_var = "age", sv_selection = c(7, 8), rm_suspdup = TRUE)
    Output
      demo.parquet subset has 2 rows.
      adr.parquet subset has 2 rows.
      out.parquet subset has 2 rows.
      srce.parquet subset has 2 rows.
      followup.parquet subset has 2 rows.
      drug.parquet subset has 2 rows.
      link.parquet subset has 2 rows.
      ind.parquet subset has 2 rows.

# alternative syntaxes work

    Code
      tb_subset(wd_in = wd_in, wd_out = paste0(wd_in, "/", "subset_age"), subset_var = "age",
      sv_selection = c(7, 8))
    Output
      demo.parquet subset has 2 rows.
      adr.parquet subset has 2 rows.
      out.parquet subset has 2 rows.
      srce.parquet subset has 2 rows.
      followup.parquet subset has 2 rows.
      drug.parquet subset has 2 rows.
      link.parquet subset has 2 rows.
      ind.parquet subset has 2 rows.

