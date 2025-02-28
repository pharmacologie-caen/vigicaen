test_that("basic use works", {
  f_sets <-
    list(ATC.txt = data.frame(f0 = "A         1ALIMENTARY TRACT AND METABOLISM                                                                               "),
        CCODE.txt = data.frame(f0 = "ABW       Aruba                                                                           "),
        ING.txt = data.frame(f0 = "1         198512312301                                    38        1         1         "),
        MP.txt = data.frame(f0 = "1                                            0000010100100000000010000000001YMethyldopa                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              N/A                 0         001       N/A          001       0         1985123120170907"),
        ORG.txt = data.frame(f0 = "0         None                                                                            UNS       "),
        PF.txt = data.frame(f0 = "001       Unspecified                                                                     "),
        PP.txt = data.frame(f0 = "1         001                 1         0119851231"),
        PRT.txt = data.frame(f0 = "001       Medicinal product                                                               "),
        PRG.txt = data.frame(f0 = "0         None                                                        20020701"),
        SRCE.txt = data.frame(f0 = "001       INN - International Nonproprietary Names - WHO                                  N/A       "),
        STR.txt = data.frame(f0 = "000001    Unspecified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "),
        SUN.txt = data.frame(f0 = "1         0000050000EN        Formaldehyde solution                                                                                            180       "),
        ThG.txt = data.frame(f0 = "100007    N06BA     19890630Y40683     "),
        `Unit-X.txt` = data.frame(f0 = "01        kg                                      "))


  tmp_folder <- tempdir()

  path_who <- paste0(tmp_folder, "/", "who", "/")

  if(!dir.exists(path_who))
    dir.create(path_who)

  purrr::iwalk(f_sets, function(d_, name_){
      write.table(d_, file = paste0(path_who, name_), row.names = FALSE, quote = FALSE, col.names = FALSE)
  })

  expect_snapshot(
    tb_who(path_who = path_who, force = TRUE),
    transform =
      function(chr_line)
        stringr::str_replace(
          chr_line,
          "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
          " percent, seconds"
        )
  )

  mp_res <- arrow::read_parquet(paste0(path_who, "mp.parquet"))

  mp_true <-
    dplyr::tibble(
      MedicinalProd_Id = 1,
      Sequence.number.1 = "01",
      Sequence.number.2 = "001",
      DrecNo = 1,
      drug_name_t = "methyldopa",
      Create.date = "19851231",
      Date.changed = "20170907",
      Country = "N/A       ")


  expect_equal(mp_res, mp_true)

  # no end slash to path_who

  path_who_no_slash <- paste0(tmp_folder, "/", "who_no_slash")

  if(!dir.exists(paste0(path_who_no_slash, "/")))
    dir.create(paste0(path_who_no_slash, "/"))

  purrr::iwalk(f_sets, function(d_, name_){
    write.table(d_, file = paste0(path_who_no_slash, "/", name_), row.names = FALSE, quote = FALSE, col.names = FALSE)
  })

  expect_snapshot(
    tb_who(path_who = path_who_no_slash,
           force = TRUE),
    transform =
      function(chr_line)
        stringr::str_replace(
          chr_line,
          "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
          " percent, seconds"
        )
  )

  mp_res_ns <- arrow::read_parquet(paste0(path_who, "mp.parquet"))

  expect_equal(mp_res_ns, mp_true)

  unlink(tmp_folder, recursive = TRUE)
})

test_that("path_who exists before working on tables", {
  wrong_path <- "/a/wrong/filepath/"

  expect_snapshot(error = TRUE, cnd_class = TRUE,
    tb_who(path_who  = wrong_path, force = TRUE)
  )

  cnd <- rlang::catch_cnd(tb_who(path_who  = wrong_path, force = TRUE))

  expect_s3_class(cnd, "no_dir")
  expect_equal(cnd$dir, "path_who")
  expect_equal(cnd$wrong_dir, wrong_path)
})
