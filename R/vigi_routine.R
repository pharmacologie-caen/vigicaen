#' Pharmacovigilance routine function
#'
#' @description `r lifecycle::badge('experimental')` `vigi_routine()` draws
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
#' If you're demo table was filtered on specific cases (e.g. older adults,
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
#'
#' @returns A ggplot2 graph, with two panels.
#' The first panel, on top, is the Information Component (IC) plot.
#' The arrow and "IC025 label" indicate the IC value for the selected drug-adr pair.
#' The second panel, on the bottom, is the Time to Onset (TTO) density plot.
#' It is derived only of cases where the drug was **suspected** to be responsible
#' of the adr.
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
#' # load tables demo, drug, adr, and link
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
#' # Additional customization with d_name and a_name args
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
#'   a_label = "Colitis"
#' )
#'
#' # You can export the plot with export_to
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
    export_to = NULL
  ){
    # #### 0. checkers #### ####

    # 0.1 d_drecno and a_llt only have one item each.

    if (length(d_code) != 1) {
      stop("d_code must have only one item for this function.")
    }

    if (length(a_code) != 1) {
      stop("a_code must have only one item for this function.")
    }

    if(!is.list(d_code)){
      stop("d_code must be a named list")
    }

    if(!is.list(a_code)){
      stop("a_code must be a named list")
    }

    # 0.2 d_name and a_name, d_label and a_label

    d_name <- names(d_code)

    a_name <- names(a_code)

    if(is.null(d_label)){
      d_label <- d_name
    }

    if(is.null(a_label)){
      a_label <- a_name
    }

    # 0.3 export_to should end by
    #  "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
    # as is guessed by ggplot2::ggsave().

    if(!is.null(export_to)){
      if(!grepl("\\.(eps|ps|tex|pdf|jpeg|tiff|png|bmp|svg|wmf)$",
                export_to)){
        stop(paste0(
        "export_to must end by '.bmp', '.eps', '.jpeg', '.pdf', '.png', '.ps'",
        "'.svg', '.tex', '.tiff', or '.wmf' (windows only)"
        ))
      }
    }

    # #### 1. acquire data #### ####

    # ---- build demo ----

    suppressMessages(
      # messages from add_drug and add_adr
      demo_data <- demo_data |>
        add_drug(d_code, drug_data = drug_data) |>
        add_adr(a_code, adr_data = adr_data)
    )

    # ---- compute ic ----

    res_ic <-
      demo_data |>
      compute_dispro(
        y = a_name,
        x = d_name,
        export_raw_values = TRUE
      )

    # ---- build link ----

    suppressMessages(
      link_data <-
        link_data |>
        add_drug(d_code, drug_data = drug_data, repbasis = "s") |>
                 # Only suspect cases
        add_adr(a_code, adr_data = adr_data)
        )

    # ---- extract and summarize ttos ----

    ttos <-
      extract_tto(
        link_data,
        adr_s =  a_name,
        drug_s = d_name
      ) |>
      dplyr::mutate(
        # capping data
        tto_max =
          dplyr::case_when(
            .data$tto_max < 1
            ~ 1,
            .data$tto_max > 3650
            ~ 3650,
            TRUE
            ~ .data$tto_max
          )
      )

    summary_ttos <-
      quantile(ttos$tto_max,
               c(0.10, 0.25, 0.5, 0.75, 0.90))

    # #### 2. set custom legends #### ####

    # setting and vigibase version should be provided by the user.

    plot_subtitle = paste0("Disproportionality analysis and time to onset.\n",
                           "Drug: ", d_label, "\n",
                           "Adverse event: ", a_label, "\n",
                           "N\u00b0 of cases: ", cff(res_ic$a), "\n",
                           "Setting: ", analysis_setting, "\n",
                           "VigiBase version: ", vigibase_version)

    # #### 3. information component plot #### ####

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
               size = 5) +

      guides(fill = "none") +
      labs(
        title = toupper("Vigibase analysis"),
        subtitle = plot_subtitle
      ) +
      scale_y_continuous(limits = c(0, 3)) +
      theme_void() +
      theme(
        axis.text.x = element_text(size = 11, vjust = 1,
                                   margin = margin(t = -10, r = 0, b = 0, l = 0)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.background = element_rect(color = NA, fill = "grey97"),
      )

    # ggsave(filename = "graph_test.svg", plot = g1, width = 4, height = 1.2)


    # #### 4. time to onset plot #### ####

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

    if (!is.null(case_tto)) {
      pl_label <-
        paste0("Patient: ", round(case_tto), " days")

      pl_hjust <-
        if (case_tto < summary_ttos["50%"]) {
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
              y = 1.15,
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
          caption =
            paste0(
              "d: day, w: week, m: month, y: year\n",
              "x axis capped at 1 day (min) and 10 years (max)\n\n",
              "Created with vigicaen, the R package for VigiBase\u00ae"
            ),
          x = x_label,
          y = "Distribution"
        ) +
        guides(fill = guide_legend(title = NULL)) +
        theme_bw() +
        theme(
          plot.background = element_rect(color = NA, fill = "grey97"),
          legend.position = "bottom",
          plot.caption = element_text(family = "serif")
        )



    g_both <-
      gridExtra::grid.arrange(
        g1, g2, layout_matrix =
          matrix(
            c(1, 1, 1, 2, 2, 2, 2), byrow = TRUE, ncol = 1
          ))

    }

    if(!is.null(export_to)){
      if(nrow(ttos) > 2){
        ggsave(
          filename = export_to,
          plot = g_both,
          width = 4,
          height = 6.7
        )
      } else {
      ggsave(
        filename = export_to,
        plot = g1,
        width = 4,
        height = 2.9
      )
      }

      message(paste0("Plot exported to ", export_to))
    }

    if(nrow(ttos) > 2){
      invisible(g_both)
    } else {
      message("Not enough data to plot time to onset")
      return(g1)
    }

  }
