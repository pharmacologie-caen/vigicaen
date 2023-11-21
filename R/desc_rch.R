#' Rechallenge and TTO description
#'
#' drug-adr pair description of rechallenge cases and time to onsets.
#'
#' Description span from rechallenge cases to __informative__ rechallenge cases (those cases where the outcome is known). Drug and Adr identifiers refer to DrecNo and MedDRA_Id, respectively.
#' Terminology
#' \itemize{
#'   \item `Overall` as opposed to `rch` for rechallenged (`rch` + `no_rch` = `overall`).
#'   \item Among `rch`, `inf` (informative) as opposed to `non_inf` (`inf` + `non_inf` = `rch`)
#'   \item Among `inf`, `rec` (recurring) as opposed to `non_rec` (`rec` + `non_rec` = `inf`)
#' }
#'
#'
#'
#' While it might look surprising to have 2 different purposes in the same function (e.g. describe rechallenge and time to onset), it happens data management steps are almost identical. In a bright future, the function might be splitted in 2.
#'
#' @param luda_data A data.table. luda stands for Link with UmcreportId, Drug and Adr identifiers (see details).
#' @param demo_data A data.table. demo should have a serious and death columns.
#' @param adr_s A character string. The name of the adr column. Adr columns can be created with \code{\link{add_adr}} in a luda table.
#' @param drug_s A character string. The name of the drug column. Drug columns can be created with \code{\link{add_drug}} in a luda table.
#' @param tto_time_range Incertitude range of Time to onset, in days. Defaults to 1 as recommended by umc
#'
#' @return A one-row data.table with
#' \itemize{
#'   \item Counts of __overall__, __rch__, __inf__, and __rec__ cases (see details).
#'   \item Number of cases with a time to onset `tto` available in all settings.
#'   \item Median (interquartile range) time to onset in all settings.
#' }
#' @export
#' @import dplyr data.table
#'
#' @examples
#'
#' rch_desc(luda_data = luda_,
#'          demo_data = demo_rch_)
rch_desc <- function(luda_data,
                     demo_data, # same here
                     adr_s = "a_colitis",
                     drug_s = "pd1",
                     tto_time_range = 1
){

  luda_sel <- # selection
    luda_data %>%
    filter(if_any(all_of(adr_s), ~ .x == 1) &
             if_any(all_of(drug_s), ~ .x == 1)
           )

  demo_sel <- demo_data[UMCReportId %in% luda_sel[, UMCReportId]]

  luda_sel_rch <-
    luda_sel %>%
    filter(Rechallenge1 == "1")

  demo_sel_rch <- demo_sel[UMCReportId %in% luda_sel_rch[, UMCReportId]]

  luda_sel_inf <-
    luda_sel %>%
    filter(Rechallenge2 %in% c("1", "2"))

  demo_sel_inf <- demo_sel[UMCReportId %in% luda_sel_inf[, UMCReportId]]

  luda_sel_rec <-
    luda_sel %>%
    filter(Rechallenge2 %in% c("1"))

  demo_sel_rec <- demo_sel[UMCReportId %in% luda_sel_rec[, UMCReportId]]

  n_overall <- demo_sel[, .N]

  # ---- Counting rechal cases ---- #

  # ++++ Any ++++ #
  n_rch <- demo_sel_rch[, .N]

  # ++++ Inf ++++ #
  n_inf <- demo_sel_inf[, .N]

  # ++++ Recurring cases ++++ #
  n_rec <- demo_sel_rec[, .N]

  # ++++ Death and seriousness among inf and rec cases ++++ #
  n_death_among_inf <- demo_sel_inf[death == 1, .N]
  n_death_among_rec <- demo_sel_rec[death == 1, .N]
  n_serious_among_inf <- demo_sel_inf[serious == 1, .N]
  n_serious_among_rec <- demo_sel_rec[serious == 1, .N]

  # ---- TTO ---- #

  # ++++ Any & Overall ++++ #
  tmp_tto <- luda_sel[range <= tto_time_range &
                                tto_mean >= 0
                              ]

  tmp_tto <- tmp_tto[, .(tto_max = max(tto_mean, na.rm = TRUE)),
                     by = .(UMCReportId, Rechallenged = Rechallenge1 == "1")]

  tmp_tto[, `:=` (Rechallenged2 = max(Rechallenged, na.rm = TRUE)),
          by = UMCReportId]
  # ./technical_points/tech_note_20201104

  tmp_tto %<>% filter(!(Rechallenged == FALSE & Rechallenged2 == 1)) %>%
    # scorie de codage qui necessite un refiltre pour eliminer les lignes inutiles
    distinct(UMCReportId, .keep_all = TRUE) %>%
    mutate(Rechallenged = as.logical(Rechallenged2)) %>% data.table

  # TTO a l'echelle de l'umcreportid - delai le plus long intro d'un ttt - evenement

  tmp3 <- tmp_tto[,.(TTO = paste0(round(median(tto_max, na.rm = TRUE)), " (",
                                  round(quantile(tto_max, .25, na.rm = TRUE)), "-",
                                  round(quantile(tto_max, .75, na.rm = TRUE)), ")")), by=(Rechallenged)]
  tmp4 <- tmp_tto[,.(TTO = paste0(round(median(tto_max, na.rm = TRUE)), " (",
                                  round(quantile(tto_max, .25, na.rm = TRUE)), "-",
                                  round(quantile(tto_max, .75, na.rm = TRUE)), ")"))]

  tto_rch <- ifelse(length(tmp3[Rechallenged == TRUE, TTO]) == 0,
                    "-",
                    tmp3[Rechallenged == TRUE, TTO])
  tto_no_rch <- ifelse(length(tmp3[Rechallenged == FALSE, TTO]) == 0,
                       "-",
                       tmp3[Rechallenged == FALSE, TTO])

  tto_overall <- ifelse(length(tmp3[, TTO]) == 0,
                        "-",
                        tmp4[, TTO])
  test_tto_rch_vs_no_rch <- ifelse(nrow(tmp3) != 2,
                                   NA_real_,
                                   wilcox.test(tto_max ~ Rechallenged, data = tmp_tto)[["p.value"]])

  # ---- Counting cases with a TTO ---- #

  tmp_tto_n <- tmp_tto[, .N, by = Rechallenged]
  n_tto_avail_rch <- ifelse(length(tmp_tto_n[Rechallenged == TRUE, N])==0,
                            NA_integer_,
                                tmp_tto_n[Rechallenged == TRUE, N])
  n_tto_avail_no_rch <- ifelse(length(tmp_tto_n[Rechallenged == FALSE, N])==0,
                                   NA_integer_,
                                   tmp_tto_n[Rechallenged == FALSE, N])
  n_tto_avail_overall <- ifelse(length(tmp_tto_n[, N]) == 0,
                                NA_integer_,
                                tmp_tto_n[, sum(N)]) # n rechall + n not rechall = sum(N)

  # ++++ Inf ++++ # # ici l'id?e est de diff?rencier un inf d'un non_inf
  tmp_tto_any <- luda_sel_rch[range <= tto_time_range &
                                tto_mean >= 0]

  tmp_tto_any <- tmp_tto_any[,.(tto_max = max(tto_mean, na.rm = TRUE), RC.statut = Rechallenge2),
                             by=.(UMCReportId, rc_inf = Rechallenge2 %in% c("1", "2"))]
  # l'erreur est sans doute ici, un UMC report Id peut ?tre ? la fois rechallenge + et - je pense. Pour r?soudre : consid?rer qu'un ICSR rechalleng? est toujours rechalleng? quelque soit le statut des autres lignes
  tmp_tto_any %<>% distinct(.keep_all = TRUE) %>% data.table

  tmp_tto_any[, `:=` (rc_inf2 = max(rc_inf, na.rm = TRUE)), by = UMCReportId] # tech_note_20201104

  tmp_tto_any %<>% filter(!(rc_inf2 == 1 & !(RC.statut %in% c("1", "2")))) %>%
    distinct(UMCReportId, .keep_all = TRUE) %>%
    mutate(rc_inf = as.logical(rc_inf2)) %>% data.table

  # TTO a l'echelle de l'umcreportid - delai le plus long intro d'un brafi - evenement

  tmp3 <- tmp_tto_any[,.(TTO = paste0(round(median(tto_max, na.rm = TRUE)), " (",
                                      round(quantile(tto_max, .25, na.rm = TRUE)), "-",
                                      round(quantile(tto_max, .75, na.rm = TRUE)), ")")), by=(rc_inf)]
  tmp4 <- tmp_tto_any[,.(TTO = paste0(round(median(tto_max, na.rm = TRUE)), " (",
                                      round(quantile(tto_max, .25, na.rm = TRUE)), "-",
                                      round(quantile(tto_max, .75, na.rm = TRUE)), ")"))]

  tto_non_inf <- ifelse(length(tmp3[rc_inf == FALSE, TTO]) == 0,
                        "-",
                        tmp3[rc_inf == FALSE, TTO])
  tto_inf <- ifelse(length(tmp3[rc_inf == TRUE, TTO]) == 0,
                    "-",
                    tmp3[rc_inf == TRUE, TTO])
  tto_rch_2 <- ifelse(length(tmp3[, TTO]) == 0,"-",tmp4[, TTO])
  test_tto_inf_vs_non_inf <- ifelse(nrow(tmp3) != 2,
                                    NA_real_,
                                    wilcox.test(tto_max ~ rc_inf, data = tmp_tto_any)[["p.value"]])

  # TTO in recurring cases
  if(nrow(tmp_tto_any[rc_inf == TRUE]) == 0) {
    tmp3 <- data.table(TTO = "-", RC.statut = "1")
  } else {
    tmp3 <- tmp_tto_any[rc_inf == TRUE,
                        .(TTO = paste0(
                          round(median(tto_max, na.rm = T)),
                          " (",
                          round(quantile(tto_max, .25, na.rm = T)),
                          "-",
                          round(quantile(tto_max, .75, na.rm = T)),
                          ")"
                        )), by = .(RC.statut)]}

  tto_rec <- ifelse(length(tmp3[RC.statut == "1", TTO])==0,
                    "-",
                    tmp3[RC.statut == "1", TTO])
  tto_no_rec <- ifelse(length(tmp3[RC.statut == "2", TTO])==0,
                       "-",
                       tmp3[RC.statut == "2", TTO])
  test_tto_rec_vs_no_rec <- ifelse(nrow(tmp3) !=2,
                                   NA_real_,
                                   wilcox.test(tto_max ~ RC.statut,
                                                                  data = tmp_tto_any[rc_inf==TRUE])[["p.value"]])

  # ---- Counting cases with a TTO ---- #

  tmp_tto_n_any <- tmp_tto_any[,.N, by=rc_inf]
  n_tto_avail_inf <- ifelse(length(tmp_tto_n_any[rc_inf == TRUE, N])==0,
                            NA_integer_,
                            tmp_tto_n_any[rc_inf == TRUE, N])
  n_tto_avail_non_inf <- ifelse(length(tmp_tto_n_any[rc_inf == FALSE, N])==0,
                                NA_integer_,
                                tmp_tto_n_any[rc_inf == FALSE, N])
  n_tto_avail_rch_2 <- ifelse(length(tmp_tto_n_any[, N])==0,
                              NA_integer_,
                              tmp_tto_n_any[, sum(N)]) # n rechall + n not rechall = sum(N)

  tmp3 <- tmp_tto_any[rc_inf == TRUE, .N, by = RC.statut]
  n_tto_avail_rec <- ifelse(length(tmp3[RC.statut == "1", N])==0,
                            NA_integer_,
                            tmp3[RC.statut == "1", N])
  n_tto_avail_no_rec <- ifelse(length(tmp3[RC.statut == "2", N])==0,
                               NA_integer_,
                               tmp3[RC.statut == "2", N])

  # Output of results
  data.table(adr = adr_s,

             drug = drug_s,

             n_overall,

             n_rch,

             n_inf,

             n_rec,

             n_tto_avail_rch,
             n_tto_avail_no_rch,
             n_tto_avail_overall,

             tto_rch,
             tto_no_rch,
             tto_overall,
             test_tto_rch_vs_no_rch,

             n_tto_avail_inf,
             n_tto_avail_non_inf,
             n_tto_avail_rch_2, # je laisse ces doublons car obtenus a partir de manip differentes, permet de verifier la consistance.

             tto_inf,
             tto_non_inf,
             tto_rch_2,
             test_tto_inf_vs_non_inf,

             n_tto_avail_rec,
             n_tto_avail_no_rec,

             tto_rec,
             tto_no_rec,
             test_tto_rec_vs_no_rec,

             n_death_among_inf,
             n_death_among_rec,
             n_serious_among_inf,
             n_serious_among_rec
  )
}
