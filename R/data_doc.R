#' Data of immune checkpoint inhibitors.
#'
#' Demo is the cornerstone table in Vigibase Extract Case Level data. It contains the `UMCReportId` identifier for individual case safety reports, which is a jointure key with most other tables (see "VigiBase Extract Case Level - file description.pdf" in VigiBase ECL folders for more details). It is the preferred table to update for drugs and adrs identification before performing disproportionality analyses.
#' This `demo_` table is a subset of reports reporting at least one ICI, from the September 2019 version of Vigibase ECL. All other tables were subsetted to match these records. I used the "substance" table at this time, with substances `"Ipilimumab", "Atezolizumab", "Durvalumab", "Nivolumab", "Pembrolizumab", "Avelumab", "Cemiplimab","REGN 2810", "Tremelimumab"` identified.
#' Others tables are `drug_`, `adr_`, `link_`, `ind_`, `out_`, `srce_`, `followup`.
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

#' @rdname demo_

"thg_"


#' Data for the immune checkpoint inhibitors example
#'
#' These are a set of data to provide examples on how my library works.
#' \itemize{
#'   \item `meddra` is V24.0 of meddra (full table)
#'   \item `smq_list_content` is a jointure of `smq_list` and `smq_content` from Meddra v X / english / medascii (full table)
#'   \item `smq_sel` is a named list of smq names
#'   \item `pt_sel` is a named list of pt names
#'   \item `adr_list` is a named list of meddra llt codes related to adrs from `smq_sel` and `pt_sel`
#'   \item `mp_short` is a correspondence table between drug names and drecnos (first created in Vigibase ECL / who data). `mp_short` data.table is typically created using the `tb_mp.R` script in `/STAT/R_FUN`, or `tb_who.R`. `mp_short` is MP whose  `Drug.name` column has been `tolower(trimws())`-ed and with less details on MP.
#'   \item `d_sel_names` is a named list of character vectors with drug names
#'   \item `d_drecno` is a named list of drecnos for d_sel_names
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
#' @source VigiBase Extract Case Level and MedDRA
#'
#' @examples
#' data(ex_)
#' ex_$pt_sel

"ex_"



#' Data for the rechallenge
#'
#' These are a set of data to provide examples on how my library works.
#' \itemize{
#'   \item `luda_` Is a Link table, augmented with Umcreportid, Drug and Adr identifiers. These identifiers are DrecNo and MedDRA_Id, respectively.
#'   \item `demo_rch_` is a subsetted demo that encompass luda_ UMCReportIds.
#' }
#'
#' @docType data
#'
#' @format Two objects of class data.table and data.frame.
#'
#' @keywords datasets
#'
#' @source VigiBase Extract Case Level
#'
#' @examples
#' data(luda_)
#' data(demo_rch_)
#' desc_rch(luda_, demo_rch_)

"luda_"

#' @rdname luda_

"demo_rch_"
