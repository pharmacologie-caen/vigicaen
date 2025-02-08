test_that("can process meddra tables", {
  f_sets <-
    list(
      llt.asc = data.frame(f0 = c('486813518$A first LLT name$6548988$$$$$$$N$$',
                                  '568798788$A second LLT name$47984764$$$$$$$Y$$')
      ),
      mdhier.asc = data.frame(f0 = c("6548988$84068465$98765468$8746878$A PT name$An HLT class$An HLGT class$A SOC class$A SOC class abrev$$12365489$Y$",
"47984764$7852085$6852245$2358467$A 2nd PT name$HLT class$HLGT class$SOC class$SOC abrev$$10005329$Y$")),
      smq_content.asc = data.frame(f0 = c("20486512$6548641$5$1$A$0$A$7.1$7.1$",
                                          "20904441$98143546$5$1$A$0$A$7.1$7.1$")),
      smq_list.asc = data.frame(f0 = c("20486512$The SMQ name (SMQ)$1$Long text describing this SMQ to provide context to the user$References$$26.1$A$N$",
"20904441$Another name (SMQ)$1$Another long text$References$$26.1$A$N$")
)
    )

  tmp_folder <- tempdir()
  path_meddra <- paste0(tmp_folder, "/", "meddra", "/")
  dir.create(path_meddra)

  purrr::iwalk(f_sets, function(d_, name_){
      write.table(d_, file = paste0(path_meddra, name_), row.names = FALSE,
                  quote = FALSE, col.names = FALSE)
    })

  expect_snapshot(
    tb_meddra(path_meddra),
    transform =
      function(chr_line)
        stringr::str_replace(
          chr_line,
          "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
          " percent, seconds"
        )
  )

  meddra_res <-
    arrow::read_parquet(paste0(path_meddra, "meddra_hierarchy.parquet"))

  smq_list_res <-
    arrow::read_parquet(paste0(path_meddra, "smq_list.parquet"))


  smq_content_res <-
    arrow::read_parquet(paste0(path_meddra, "smq_content.parquet"))

  expect_equal(
    meddra_res$llt_code,
    c(486813518L, 568798788L)
  )

  expect_equal(
    meddra_res$llt_name,
    c("A first LLT name",
      "A second LLT name")
  )

  expect_equal(
    meddra_res$pt_name,
    c("A PT name",
      "A 2nd PT name")
  )

  expect_equal(
    meddra_res$pt_code,
    c(6548988L, 47984764L)
  )

  expect_equal(
    smq_list_res$smq_code,
    c(20486512L, 20904441L)
  )

  expect_equal(
    smq_content_res$smq_code,
    c(20486512L, 20904441L)
  )

  expect_equal(
    smq_content_res$term_code,
    c(6548641L, 98143546L)
  )

  unlink(tmp_folder, recursive = TRUE)
})

test_that("msg_tb_onceperdatabase prints in different formats", {
  cli::test_that_cli("format is ok", {
    expect_snapshot( {
      msg_tb_onceperdatabase()
    })
  })
  expect_message(
    msg_tb_onceperdatabase(),
    "process"
  )
})

