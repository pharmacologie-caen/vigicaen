#' Data of immune checkpoint inhibitors.
#'
#' Demo is the cornerstone table in Vigibase Extract Case Level data. It contains the `UMCReportId` identifier for individual case safety reports, which is a jointure key with most other tables (see "VigiBase Extract Case Level - file description.pdf" in VigiBase ECL folders for more details). It is the preferred table to update for drugs and adrs identification before performing disproportionality analyses.
#' This `demo_` table is a subset of reports reporting at least one ICI, from the September 2019 version of Vigibase ECL. All other tables were subsetted to match these records. I used the "substance" table at this time, with substances `"Ipilimumab", "Atezolizumab", "Durvalumab", "Nivolumab", "Pembrolizumab", "Avelumab", "Cemiplimab","REGN 2810", "Tremelimumab"` identified.
#' Others tables are `drug_`, `adr_`, `link_`, `ind_`, `out_`, `srce_`, `followup_`.
#'
#' @docType data
#'
#' @usage data(demo_)
#'
#' @format An object of class `data.frame` and `data.table`.
#'
#' @keywords datasets
#'
#' @references There is none
#'
#' @source None
#'
#' @examples
#' data(demo_)
#' demo_[, .N, by = AgeGroup]

"demo_"

#' @rdname demo_

"drug_"

#' @rdname demo_

"followup_"

#' @rdname demo_

"ind_"

#' @rdname demo_

"adr_"

#' @rdname demo_

"link_"

#' @rdname demo_

"out_"

#' @rdname demo_

"srce_"

#' Sample of Meddra.
#'
#' Anonymized data from MedDRA, used to illustrate the package examples and vignettes.
#' You can find term codes related to colitis, pneumonitis, hepatitis, a SMQ of
#' embolisms.
#' Compounds are `meddra_` and `smq_list_content_`.
#' See examples in \code{\link{get_llt_soc}} and \code{\link{get_llt_smq}}
#'
#' @docType data
#'
#' @usage data(meddra_)
#'
#' @format An object of class `data.frame` and `data.table`.
#'
#' @keywords datasets, meddra
#'
#' @references There is none
#'
#' @source None
#'
#' @examples
#' data(meddra_)

"meddra_"

#' @rdname meddra_

"smq_list_content_"

#' Sample of WHODrug
#'
#' A small part of WHODrug, used to illustrate the package examples and vignettes.
#' You can find DrecNo related to immune checkpoint inhibitors, paracetamol,
#' tramadol, tretinoin, anti-thrombin iii, and ATC classes
#' L03AA Colony stimulating factors, C09AA ACE inhibitors, plain,
#' J01CA Penicillins with extended spectrum.
#' Compounds are `thg_` and `mp_short_`.
#' See examples in \code{\link{get_drecno}} and \code{\link{get_atc_code}}
#'
#' @docType data
#'
#' @usage data(meddra_)
#'
#' @format An object of class `data.frame` and `data.table`.
#'
#' @keywords datasets, whodrug
#'
#' @references There is none
#'
#' @source None
#'
#' @examples
#' data(mp_short_)

"mp_short_"

#' @rdname mp_short_

"thg_"

#' Data for the immune checkpoint inhibitors example
#'
#' These are a set of data to provide examples on how my library works.
#' \itemize{
#'   \item `smq_sel` is a named list of smq names
#'   \item `pt_sel` is a named list of pt names
#'   \item `a_llt` is a named list of meddra llt codes related to adrs from `smq_sel` and `pt_sel`
#'   \item `mp_short` is a correspondence table between drug names and drecnos (first created in Vigibase ECL / who data). `mp_short` data.table is typically created using the `tb_mp.R` script in `/STAT/R_FUN`, or `tb_who.R`. `mp_short` is MP whose  `Drug.name` column has been `tolower(trimws())`-ed and with less details on MP.
#'   \item `d_drecno` is a named list of drecnos for immune checkpoint inhibitors (some of them)
#'   \item `d_groups` is a named list of ici classes according to icis
#'   \item `d_groups_drecno` is a named list of drecnos for drug groups
#' }
#'
#' @docType data
#'
#' @usage data(ex_)
#'
#' @format An object of class `list`.
#'
#' @keywords datasets
#'
#' @references There is none
#'
#' @source VigiBase Extract Case Level
#'
#' @examples
#' data(ex_)
#' ex_$pt_sel

"ex_"



#' Data for drug-adr pair analysis.
#'
#' Luda is a very special table used to compute time to onset, dechallenge,
#' and rechallenge. `luda_` is a Link table, augmented with Umcreportid,
#' Drug and Adr identifiers.
#' These identifiers are DrecNo and MedDRA_Id, respectively. If you wish to
#' create a luda table from a link one, see the example below.
#' Several examples in \code{\link{desc_dch}}, \code{\link{desc_rch}}, \code{\link{desc_tto}},
#' \code{\link{extract_tto}}
#'
#' @docType data
#'
#' @format An object of class data.table and data.frame.
#'
#' @keywords datasets
#'
#' @source VigiBase Extract Case Level
#'
#' @examples
#' # luda is preloaded in the package as luda_
#' data(luda_)
#'
#' # if you want to create it from a link table
#'
#' library(dplyr)
#'
#' link_
#'
#' luda <- link_ |>
#'   left_join(
#'     drug_[, .(UMCReportId, Drug_Id)],
#'     by = "Drug_Id"
#'   ) |>
#'   mutate(
#'     tto_mean = (.data$TimeToOnsetMax + .data$TimeToOnsetMin) / 2,
#'     range = (.data$TimeToOnsetMax + .data$TimeToOnsetMin) / 2 - .data$TimeToOnsetMin
#'   )

"luda_"
