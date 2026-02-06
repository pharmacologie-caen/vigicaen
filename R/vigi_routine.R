#' Display routine pharmacovigilance summary
#'
#' @description `r lifecycle::badge('maturing')` `vigi_routine()` draws
#' an Information Component plot and
#' a Time to Onset plot for a given drug-adr pair.
#'
#' @details See `vignette("routine_pharmacovigilance")` for examples.
#' The output can be exported. Time to onset data are bounded between
#' 1 day and 10 years. Data outside this range are reassigned a 1 day and 10
#' years value, respectively.
#' The function only works if there is one item in `d_code` and `a_code`.
#' If you are working on a specific case, you can provide a `case_tto` value.
#' This value will be displayed on the Time to Onset plot.
#' If your demo table was filtered on specific cases (e.g. older adults,
#' a subset of all drugs), then you may want to indicate this setting on the
#' plot legend, with arg `analysis_setting`.
#'
#' @param demo_data A demo data.table.
#' @param drug_data A drug data.table.
#' @param adr_data An adr data.table.
#' @param link_data A link data.table.
#' @param d_code A named list. The drug code(s) to be used. There must be
#' only one item in d_code.
#' @param a_code A named list. The adr code(s) to be used. There must be
#' only one item in a_code.
#' @param case_tto A numeric. The time to onset of the studied case. See details.
#' @param vigibase_version A character. The version of VigiBase used (e.g.
#' "September 2024"). This is passed to the plot legend.
#' @param analysis_setting A character. The setting of the analysis. See details.
#' @param d_label A character. The name of the drug, as passed to the plot legend.
#' Defaults to names(d_code).
#' @param a_label A character. The name of the adr, as passed to the plot legend.
#' Defaults to names(a_code).
#' @param export_to A character. The path to export the plot. If NULL, the plot
#' is not exported. Should end by ".eps", ".ps", ".tex" (pictex), ".pdf", ".jpeg",
#' ".tiff", ".png", ".bmp", ".svg" or ".wmf" (windows only).
#' @param suspect_only Logical. If TRUE, only cases where the drug is suspected are used for IC analysis. If FALSE (default), all cases are used.
#' @param d_code_2 Optional named list. A second drug code for dual drug analysis. If provided, a single analysis is performed on cases exposed to both drugs.
#'
#' @returns A ggplot2 graph, with two panels.
#' The first panel, on top, is the Information Component (IC) plot.
#' The arrow and "IC025 label" indicate the IC value for the selected drug-adr pair.
#' The second panel, on the bottom, is the Time to Onset (TTO) density plot.
#' It is derived only of cases where the drug was **suspected** to be responsible
#' of the adr (irrespective of the `suspect_only` argument).
#' If you provide a case_tto value, it is represented by the red line, and the
#' label.
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @export
#'
#' @examples
#' # Say you want to perform a disproportionality analysis between colitis and
#' # nivolumab among ICI cases
#'
#' # identify drug DrecNo, and adr LLT code
#'
#' d_drecno <-
#'   ex_$d_drecno["nivolumab"]
#'
#' a_llt <-
#'   ex_$a_llt["a_colitis"]
#'
#' # But you could also use get_drecno() and get_llt_soc()
#'
#' # load tables demo, drug, adr, and link (real tables with
#' # open_dataset() or dt_parquet("x", in_memory = FALSE))
#'
#' demo <- demo_
#' adr  <- adr_
#' drug <- drug_
#' link <- link_
#'
#' # run routine
#'
#' vigi_routine(
#'   demo_data = demo,
#'   drug_data = drug,
#'   adr_data  = adr,
#'   link_data = link,
#'   d_code = d_drecno,
#'   a_code = a_llt,
#'   vigibase_version = "September 2024"
#' )
#'
#' # if you're working on a case, you can provide his/her time to onset
#' # with arg `case_tto`
#'
#' vigi_routine(
#'   case_tto = 150,
#'   demo_data = demo,
#'   drug_data = drug,
#'   adr_data  = adr,
#'   link_data = link,
#'   d_code = d_drecno,
#'   a_code = a_llt,
#'   vigibase_version = "September 2024"
#' )
#'
#'
#' # Customize with d_name and a_name, export the plot with export_to
#'
#' vigi_routine(
#'   case_tto = 150,
#'   demo_data = demo,
#'   drug_data = drug,
#'   adr_data  = adr,
#'   link_data = link,
#'   d_code = d_drecno,
#'   a_code = a_llt,
#'   vigibase_version = "September 2024",
#'   d_label = "Nivolumab",
#'   a_label = "Colitis",
#'   export_to = paste0(tempdir(), "/", "vigicaen_graph.png")
#' )

vigi_routine <-
  function(
    demo_data,
    drug_data,
    adr_data,
    link_data,
    d_code,
    a_code,
    case_tto = NULL,
    vigibase_version,
    analysis_setting = "All reports",
    d_label = NULL,
    a_label = NULL,
    export_to = NULL,
    suspect_only = FALSE,
    d_code_2
  ){
    # #### 0. arrow options #### ####

    # keep original user option, then set it

    original_user_option <- options("arrow.pull_as_vector")

    options(arrow.pull_as_vector = FALSE)

    # #### 0. checkers #### ####
    check_id_list_numeric(d_code)
    check_id_list_numeric(a_code)
    check_length_one(d_code, "vigi_routine()")
    check_length_one(a_code, "vigi_routine()")
    d_name <- names(d_code)
    a_name <- names(a_code)

    if(!is.null(export_to)){
      if(!grepl("\\.(eps|ps|tex|pdf|jpeg|tiff|png|bmp|svg|wmf)$", export_to)){
        cli::cli_abort(
          paste0("{.arg export_to} must end by '.bmp', '.eps', '.jpeg', '.pdf', '.png', '.ps'",
          "'.svg', '.tex', '.tiff', or '.wmf' (windows only)")
        )
      }
    }

    check_data_adr(adr_data)
    check_data_drug(drug_data)
    check_data_link(link_data)

    # #### 0.1 set internal params #### ####

    if(is.null(d_label) & rlang::is_missing(d_code_2))
      d_label <- d_name
    if(is.null(d_label) & !rlang::is_missing(d_code_2))
      d_label <- paste0(d_name, " + ", names(d_code_2))

    if(is.null(a_label)) a_label <- a_name

    repbasis_sel <-
      if (suspect_only) {"s"} else {"sci"}

    basis_sel <-
      c(
        if(grepl("s", repbasis_sel)){ 1 },
        # subsidiary_files / Repbasis_Lx
        if(grepl("c", repbasis_sel)){ 2 },
        if(grepl("i", repbasis_sel)){ 3 }
      )

    use_two_drugs <-
      !rlang::is_missing(d_code_2)

    # 0.5 check d_code_2 if provided
    if (use_two_drugs) {
      check_id_list_numeric(d_code_2)
      check_length_one(d_code_2, "vigi_routine()")
      d_name2 <- names(d_code_2)

      umc_drug_1 <-
        drug_data |>
        dplyr::filter(.data$DrecNo %in% d_code[[1]] &
                        .data$Basis %in% basis_sel) |>
        dplyr::pull(.data$UMCReportId) |>
        unique()

      umc_drug_2 <-
        drug_data |>
        dplyr::filter(.data$DrecNo %in% d_code_2[[1]] &
                        .data$Basis %in% basis_sel) |>
        dplyr::pull(.data$UMCReportId) |>
        unique()

      umc_drug <- intersect(umc_drug_1, umc_drug_2)

      umc_adr <-
        adr_data |>
        dplyr::filter(.data$MedDRA_Id %in% a_code[[1]])  |>
        dplyr::pull(.data$UMCReportId) |>
        unique()


      umc_cases <- intersect(umc_drug, umc_adr)

      cli::cli_alert_info(
        "Dual drug analysis: only cases exposed to both '{d_name}' and '{d_name2}' are included.")

      d_name_after_dm <- "both_drugs"

    } else {

      umc_drug <-
        drug_data |>
        dplyr::filter(.data$DrecNo %in% d_code[[1]] &
                        .data$Basis %in% basis_sel) |>
        dplyr::pull(.data$UMCReportId) |>
        unique()

      umc_adr <-
        adr_data |>
        dplyr::filter(.data$MedDRA_Id %in% a_code[[1]]) |>
        dplyr::pull(.data$UMCReportId) |>
        unique()

      umc_cases <- intersect(umc_drug, umc_adr)

      d_name_after_dm <- d_name
    }

    n_drug <-
      length(umc_drug)

    n_adr <-
      length(umc_adr)

    if(n_drug == 0){

      d_display_in_error <-
        if (use_two_drugs) {
          paste0(d_name, " + ", d_name2)
        } else {
          d_name
        }

      error_vigiroutine_nocases(
        d_display_in_error,
        "drug",
       "demo_data"
      )
    }

    if(n_adr == 0){
      error_vigiroutine_nocases(
        a_name,
        "adr",
        "demo_data"
      )
    }

    a <- length(umc_cases)
    b <- n_drug - a
    c <- n_adr  - a
    d <- nrow(demo_data) - a - b - c

    # ---- compute ic ----

    # args of compute_dispro
    alpha <- 0.05

    min_n_obs <- 0

    na_format = "-"

    dig = 2

    res_ic <-
      data.frame(a = a, b = b, c = c, d = d) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("a", "b", "c", "d")), ~ as.numeric(.x)),
        n_obs = .data$a,
        n_exp = (.data$a + .data$b) * # n drug
          (.data$a + .data$c) / # n event
          (.data$a + .data$b + .data$c + .data$d), # n pop
        ic = log((.data$a + .5) / (.data$n_exp + .5), base = 2),
        ic_tail = ic_tail(
          n_obs = .data$a,
          n_exp = .data$n_exp,
          p = .env$alpha / 2
        ),
        ci_level  = paste0((1 - .env$alpha) * 100, "%"),
        signif_ic = ifelse(.data$ic_tail > 0, 1, 0),
        # don't show results for pairs with less than min_n_obs
        dplyr::across(
          dplyr::all_of(
            c("n_exp", "ic", "ic_tail",
              "a", "b", "c", "d",
              "signif_ic")
          ),
          function(num_col)
            dplyr::if_else(.data$a < .env$min_n_obs,
                           NA_real_,
                           num_col)
        ),
        dplyr::across(
          dplyr::all_of(
            c("ci_level"
            )
          ),
          function(chr_col)
            dplyr::if_else(.data$a < .env$min_n_obs,
                           .env$na_format,
                           chr_col)
        )
      )

    # ---- build link ----



    suppressMessages({
      link_data <-
        link_data |>
        dplyr::filter(.data$UMCReportId %in% umc_cases) |>
        add_drug(d_code, drug_data = drug_data, repbasis = "s") |>
        add_adr(a_code, adr_data = adr_data)

      if (nrow(link_data) > 0 && use_two_drugs) {
        link_data <-
          link_data |>
          add_drug(d_code_2,
                   drug_data = drug_data,
                   repbasis = "s") |>
          dplyr::group_by(.data$UMCReportId) |>
          dplyr::mutate(both_drugs =
                          ifelse(
                            max(.data[[d_name]], na.rm = TRUE) == 1 &
                              max(.data[[d_name2]], na.rm = TRUE) == 1,
                            1,
                            0
                          )
          ) |>
          dplyr::ungroup()
        link_data <- link_data |> dplyr::filter(.data$both_drugs == 1)
      }
    })

    # ---- obtain drug basis breakdown ----
    basis_mask <-
      data.frame(
        label = c("Suspected", "Concomitant", "Interacting"),
        Basis = c("1", "2", "3")
      )
    suppressMessages({
      drug_basis <-
        drug_data |>
        dplyr::filter(.data$DrecNo %in% d_code[[d_name]] &
                        {
                          if (use_two_drugs) {
                            .data$UMCReportId %in% (umc_cases)
                          } else {
                            TRUE
                          }
                        }) |>
        add_adr(a_code, adr_data = adr_data) |>
        dplyr::filter(.data[[a_name]] == 1) |>
        dplyr::distinct(.data$UMCReportId, .data$DrecNo, .data$Basis) |>
        dplyr::count(.data$Basis) |>
        dplyr::collect()
    })

    drug_basis_table <-
      basis_mask |>
      dplyr::left_join(
        drug_basis,
        by = c("Basis")
      )

    # ---- extract and summarize ttos ----
    ttos <-
      suppressWarnings(
        extract_tto(link_data, adr_s =  a_name, drug_s = d_name) |>
          dplyr::mutate(
            tto_max =
              dplyr::case_when(
                .data$tto_max < 1 ~ 1,
                .data$tto_max > 3650 ~ 3650,
                TRUE ~ .data$tto_max
              )
          )
      )

    summary_ttos <-
      quantile(ttos$tto_max,
               c(0.10, 0.25, 0.5, 0.75, 0.90))

    # ---- extract positive rechallenge ----
    rch <-
      desc_rch(
        link_data,
        adr_s = a_name,
        drug_s = d_name
      )

    # #### 2. set custom legends #### ####

    # setting and vigibase version should be provided by the user.

    plot_subtitle = paste0(
      # "Disproportionality analysis and time to onset.\n",
                           "Drug: ", d_label, "\n",
                           "Adverse event: ", a_label, "\n",
                           # "N\u00b0 of cases: ", cff(res_ic$a), "\n",
                           "Setting: ", analysis_setting, "\n",
                           "VigiBase version: ", vigibase_version)


    # #### 1. Header plot #### ####

    g_header <-
      ggplot() +
      theme_void() +
      labs(
        title = toupper("Vigibase analysis"),
        subtitle = plot_subtitle
      ) +
      theme(
        plot.margin = margin(0.5, 0.5, 0.1, 0.5, "cm"),
        plot.background = element_rect(color = NA, fill = "grey97")
      )


    # #### 2. Case count plot #### ####

    # ---- case count dataframe

    db_table_withlab <-
      drug_basis_table |>
      dplyr::mutate(
        n = ifelse(is.na(n), 0, n),
        n_chr =
          cff(n),
        lab_n =
          label |>
          factor(levels = label,
                 labels = stringr::str_pad(
                   paste0(label, ": ", n_chr),
                   10, # gives some sort of control on distance of plot
                   side = "right")
          )
      )

    # plot vars to be defined to avoid a note in R CMD CHECK

    n        <- NULL
    lab_n    <- NULL
    label    <- NULL
    n_chr    <- NULL
    pos_x    <- NULL
    pos_y    <- NULL
    tile_width <- NULL
    lab      <- NULL

    g_db <-
      ggplot(db_table_withlab, aes(x = lab_n, y = n, fill = lab_n)) +
      geom_bar(stat = "identity") +
      guides(fill =
               guide_legend(title = NULL)
      ) +
      theme_void() +
      labs(subtitle =
             paste0("N\u00b0 of cases: ", cff(res_ic$a),
                    "\n") # extra space before plot
      ) +
      scale_fill_manual(values = c("grey15", "grey30", "grey80")) +
      theme(
        panel.border = element_rect(color = "black", fill = NA),
        plot.subtitle = element_text(face = "bold"),
        legend.position = "right",
        legend.margin = margin(0, 0.6, 0, 0.3, "cm"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.5, "cm"),
        legend.key.size = unit(0.5, "cm"),
        plot.background = element_rect(color = NA, fill = "grey97"))


    # #### 3. Rechallenge plot #### ####

    rch_lab <-
      data.frame(
        lab =
          c("Total", "Positive", "Rate",
            cff(rch$n_inf),
            cff(rch$n_rec),
            paste0(cff(rch$n_rec/rch$n_inf * 100, dig = 0), "%")
        ),
        pos_x = c(1, 1, 1, 2, 2, 2),
        pos_y = c(3, 2, 1, 3, 2, 1),
        tile_width = c(2, 2, 2, 1, 1, 1)
      )

    # handle absence of rechallenge cases
    if (rch$n_inf == 0) {
      g_rch <-
        ggplot() +
        annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = "No data"
        ) +
        theme_void() +
        labs(subtitle = paste0("Rechallenge", "\n")) +
        theme(
          plot.margin = margin(0.1, 0.5, 0.1, 0, "cm"),
          plot.subtitle = element_text(face = "bold"),
          plot.background = element_rect(color = NA, fill = "grey97"),
          plot.caption = element_text(family = "serif")
        )

    } else {
      g_rch <-
        rch_lab |>
        ggplot(aes(
          x = pos_x,
          y = pos_y,
          fill = factor(pos_x)
        )) +
        geom_tile(aes(width = tile_width), color = "white") +
        geom_text(aes(label = lab), size = 3) +
        scale_fill_manual(values = c("grey60", "grey85")) +
        guides(fill = "none") +
        theme_void() +
        labs(subtitle = paste0("Rechallenge", "\n"),
             caption = "Informative\nrechallenges only") +
        theme(
          plot.margin = margin(0.1, 0.5, 0.1, 0, "cm"),
          plot.subtitle = element_text(face = "bold"),
          plot.background = element_rect(color = NA, fill = "grey97"),
          plot.caption = element_text(family = "serif")
        )

    }

    # #### 4. information component plot #### ####

    # ---- tiles dataframe

    ic_df <-
      data.frame(
        x = 1:4,
        fill = c( "#aeac4c", "#f9c53b", "#e9851d", "#bf3626")
      )
    # derived from met.brewer("Homer2")[c(5, 4, 3, 2, 1)]

    # ---- arrow and label of IC position

    # res_ic$ic_tail <- -1 # test

    ic_position <-
      dplyr::case_when(
        res_ic$ic_tail < 0
        ~ 1,
        res_ic$ic_tail >= 0 & res_ic$ic_tail < 1
        ~ 2,
        res_ic$ic_tail >= 1 & res_ic$ic_tail < 3
        ~ 3,
        res_ic$ic_tail >= 3
        ~ 4,
        TRUE
        ~ NA_real_
      )

    ic_arrow_color <-
      dplyr::case_when(
        res_ic$ic_tail < 0
        ~ "#aeac4c",
        res_ic$ic_tail >= 0 & res_ic$ic_tail < 1
        ~ "#f9c53b",
        res_ic$ic_tail >= 1 & res_ic$ic_tail < 3
        ~ "#e9851d",
        res_ic$ic_tail >= 3
        ~ "#bf3626",
        TRUE
        ~ NA_character_
      )

    ic_label <-
      paste0(
        "IC025 = ",
        if(res_ic$ic_tail > 3 | res_ic$ic_tail < 0){
          cff(res_ic$ic_tail, dig = 1)
        } else {
          cff(res_ic$ic_tail, dig = 2)
        }
      )

    # plot vars to be defined to avoid a note in R CMD CHECK

    fill     <- NULL
    scaled   <- NULL
    x        <- NULL
    xmax     <- NULL
    xmin     <- NULL
    ymax     <- NULL
    ymin     <- NULL
    tto_max  <- NULL

    used_basis <-
      if (suspect_only) {
        "Suspect only"
      } else {
        "Suspect, concomitant or interacting"
      }

    x_label_ic_plot <-
      glue::glue("Cases used: {used_basis}")

    g1 <-
      ggplot(ic_df, aes(x = x, y = 1, fill = fill)) +

      # plot gauge
      geom_tile(color = "white", linewidth = 1) +

      scale_x_continuous(
        breaks = c(1.5, 2.5, 3.5),
        labels = c("0", "1", "3"),
        limits = c(0.4, 4.6)
      ) +

      scale_fill_manual(values = c( "#aeac4c",  "#bf3626", "#e9851d", "#f9c53b")) +
      # strange thing with color order

      # plot IC significance cutoff
      geom_segment(x = 1.5, xend = 1.5,
                   y = 0.5, yend = 2,
                   linewidth = 1,
                   color = "black") +

      # plot IC label and arrow

      geom_segment(x = ic_position, xend = ic_position,
                   y = 2.5, yend = 1.5,
                   linewidth = 2,
                   color = ic_arrow_color,
                   arrow = arrow(ends = "last")
      ) +
      annotate("label", x = ic_position,
               y = 2.5,
               label = ic_label,
               fontface = "bold",
               fill = "white",
               size = 5) +

      guides(fill = "none") +
      labs(
        subtitle = "Disproportionality Analysis",
        x = x_label_ic_plot
      ) +
      scale_y_continuous(limits = c(0, 3)) +
      theme_void() +
      theme(
        axis.text.x = element_text(size = 11, vjust = 1,
                                   margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 9, vjust = 0,
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        plot.subtitle = element_text(face = "bold"),
        plot.margin = margin(0.25, 0.5, 0.25, 0.5, "cm"),
        plot.background = element_rect(color = NA, fill = "grey97"),
      )

    # ggsave(filename = "graph_test.svg", plot = g1, width = 4, height = 1.2)


    # #### 5. time to onset plot #### ####

    # ---- dispersion rectangles

    rect_df <-
      data.frame(
        xmin = c(summary_ttos["10%"], summary_ttos["25%"]),
        xmax = c(summary_ttos["90%"], summary_ttos["75%"]),
        ymin = 0,
        ymax = 0.1,
        fill = c("80% of patients", "50% of patients")
      )

    # ---- patient label & h justification

    if (!is.null(case_tto) & nrow(ttos) > 2) {
      pl_label <-
        paste0("Patient: ", round(case_tto), " days")

      # Alignement based on 90 days (x-axis middle)
      pl_hjust <-
        if (case_tto < 90) {
          0
        } else {
          1
        }
    }

    # ---- x lab

    x_label <-
      paste0(
        "Time from drug initiation to event onset\n",
        "N\u00b0 of cases where drug was suspected: ", cff(nrow(ttos))
      )


    if (nrow(ttos) > 2) { # dont plot g2 if not enough data
      g2 <-
        ttos |>
        ggplot(aes(x = tto_max)) +
        geom_density(aes(y = after_stat(scaled)), fill = "grey") +
        # after_stat allows to use "scaled", the density scaled to 1.
        # which gives you a plot with always the same y limits.
        geom_rect(
          inherit.aes = FALSE,
          data = rect_df,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = fill
          )
        ) +
        stat_summary_bin(geom = "point",
                         fun = median,
                         orientation = "y",
                         aes(y = 0.05)) +
        scale_x_continuous(
          breaks = c(1, 3, 7, 14, 30, 90, 182, 365, 365 * 3, 3650),
          labels = c("1d", "3d", "1w", "2w", "1m", "3m", "6m", "1y", "3y", "10y"),
          limits = c(1, 3650),
          trans = "log10"
        ) +

        # patient time: line and label
        {
          if (!is.null(case_tto)) {
            geom_vline(xintercept = case_tto, color = "#bf3626")
          }
        }  +

        {
          if (!is.null(case_tto)) {
            annotate(
              "label",
              x = case_tto,
              y = 1.20,
              label = pl_label,
              vjust = 1,
              color = "#bf3626",
              hjust = pl_hjust
            )
          }
        } +

        # MetBrewer::met.brewer("Hokusai2")
        scale_fill_manual(values = c(
          "#abc9c8",
          "#72aeb6",
          "#4692b0",
          "#2f70a1",
          "#134b73",
          "#0a3351"
        )) +
        scale_y_continuous(breaks = NULL) +
        labs(
          subtitle = "Time to onset",
          caption =
            paste0(
              "d: day, w: week, m: month, y: year\n",
              "x axis capped at 1 day (min) and 10 years (max)\n\n"
            ),
          x = x_label,
          y = "Distribution"
        ) +
        guides(fill = guide_legend(title = NULL)) +
        theme_bw() +
        theme(
          plot.background = element_rect(color = NA, fill = "grey97"),
          legend.position = "bottom",
          plot.subtitle = element_text(face = "bold"),
          plot.caption = element_text(family = "serif"),
          plot.margin = margin(0.2, 0.5, 0, 0.5, "cm"),
        )
    }

    # #### 6. Footer plot #### ####

    g_footer <-
      ggplot() +
      labs(
        caption =
          "Created with vigicaen, the R package for VigiBase\u00ae"
      ) +
      theme_void() +
      theme(
        plot.background = element_rect(color = NA, fill = "grey97"),
        plot.caption = element_text(family = "serif"),
        plot.margin = margin(0, 0.5, 0.5, 0.5, "cm"),
      )

    # assemble plots #### ####

    if(nrow(ttos) > 2){
      g_assembled <-
      gridExtra::grid.arrange(
        g_header,
        g_db, g_rch,
        g1, g2, g_footer,
        layout_matrix =
          matrix(
            c(rep(1, 6 * 5),
              2, 2, 2, 3, 3,
              2, 2, 2, 3, 3,
              2, 2, 2, 3, 3,
              2, 2, 2, 3, 3,
              2, 2, 2, 3, 3,
              2, 2, 2, 3, 3,
              rep(4, 8 * 5),
              rep(5, 16 * 5),
              rep(6, 1 * 5)), byrow = TRUE, ncol = 5
          )
        )
    } else {
      g_assembled <-
        gridExtra::grid.arrange(
          g_header,
          g_db, g_rch,
          g1, g_footer,
          layout_matrix =
            matrix(
              c(rep(1, 6 * 5),
                2, 2, 2, 3, 3,
                2, 2, 2, 3, 3,
                2, 2, 2, 3, 3,
                2, 2, 2, 3, 3,
                2, 2, 2, 3, 3,
                2, 2, 2, 3, 3,
                rep(4, 8 * 5),
                rep(5, 1 * 5)), byrow = TRUE, ncol = 5
            )
        )
    }

    if(!is.null(export_to)){
      if(nrow(ttos) > 2){
        ggsave(
          filename = export_to,
          plot = g_assembled,
          width = 4,
          height = 7.6
          )
      } else {
      ggsave(
        filename = export_to,
        plot = g_assembled,
        width = 4,
        height = 4.4
      )
      }

      message(paste0("Plot exported to ", export_to))
    }

    if(nrow(ttos) <= 2){
      cli::cli_alert_info("Not enough data to plot time to onset")
    }

    # #### 7. restore user option #### ####

    options(arrow.pull_as_vector = original_user_option)


    # #### 8.Display #### ####
    invisible(g_assembled)

  }

# Helpers -------------------------

error_vigiroutine_nocases <-
  function(arg, arg_type, dataset,
           call = rlang::caller_env()){

    arg_type_fmted <-
      stringr::str_to_title(arg_type)

    cli::cli_abort(
      message =
        paste0("{arg_type_fmted} code(s) in {arg} didn't",
        " match any cases in {.arg {dataset}}."),
      class  = "no_cases",
      arg = arg,
      arg_type = arg_type,
      dataset = dataset,
      call = call
    )
  }

error_length_one <-
  function(arg, fn, wrong_length, call = rlang::caller_env()){

    cli::cli_abort(
      message =
        c("{.arg {arg}} must have only one item in {.arg {fn}}.",
          "x" = "{.arg {arg}} has {wrong_length} items."),
      class  = "length_one",
      arg = arg,
      fn = fn,
      wrong_length = wrong_length,
      call = call
    )
  }

check_length_one <-
  function(named_list,
           fn,
           call = rlang::caller_env()){
    if(length(named_list) > 1){
      error_length_one(
        arg = rlang::caller_arg(named_list),
        fn = fn,
        wrong_length = length(named_list),
        call = call
      )
    }
  }
