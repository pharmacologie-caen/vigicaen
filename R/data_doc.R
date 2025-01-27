#' Data of immune checkpoint inhibitors.
#'
#' Demo, drug, adr, link, ind, out, srce, and followup are the main table in
#' Vigibase Extract Case Level data.
#' In a regular workflow, you will work with those tables as R
#' objects (e.g. `demo`, `drug`, `adr`, `link`, `ind`, `out`, `srce`, `followup`).
#' These built-in example datasets use an underscore "_" to avoid ambiguity
#' with your own tables (e.g.
#' `demo_`, `drug_`, `adr_`, `link_`, `ind_`, `out_`, `srce_`, `followup_`).
#' This is a relational database, which means every table has a primary key
#' variable (e.g., `UMCReportId` for `demo_`. Keys will allow joints with other tables
#' The full details on the original structure can be found in
#' "VigiBase Extract Case Level - file description.pdf"
#' in your VigiBase ECL folders.
#' `demo_` will typically be your cornerstone table, since it contains
#' one row per report. It is the preferred table to update for drugs and adrs
#' identification before performing disproportionality analyses.
#' These tables are subsets of the original ones, with some of the
#' immune checkpoint inhibitor cases or immune-related adverse events.
#' All data shown in these example data are **FAKE**, which means
#' you shouldn't consider the counts and computations as accurate.
#' Immune checkpoint inhibitors drugs include
#' "Ipilimumab", "Atezolizumab", "Durvalumab", "Nivolumab", "Pembrolizumab",
#' "Avelumab", "Cemiplimab","REGN 2810", "Tremelimumab".
#' More details on how to use vigibase tables can be found in the vignettes.
#' `vignette("basic_workflow")`, `vignette("descriptive")`.
#' To build your own tables, use [tb_vigibase()]. See `vignette("getting_started")`.
#'
#' @docType data
#'
#' @usage data(demo_)
#'
#' @format
#' `demo_` is a data.table with 7 variables and 750 rows.
#' \itemize{
#'   \item `UMCReportId` Integer. The unique identifier of the case report.
#'   \item `AgeGroup` Character. The age group of the patient. Correspondence
#'   table is `path_sub/AgeGroup.parquet`.
#'   \item `Gender` Character. Case gender. `path_sub/Gender.parquet`
#'   \item `DateDatabase` Character (not date or numeric!). The date of the
#'   latest update of the report in the database.
#'   \item `Type` Character. The type of report. `path_sub/ReportType.parquet`
#'   \item `Region` Character. The world region where the report comes from
#'   `path_sub/Region.parquet`.
#'   \item `FirstDateDatabase` Character. The date the report was first
#'   submitted to the database.
#'   }
#' `drug_` is a data.table with 10 variables and 3514 rows.
#' \itemize{
#'   \item `UMCReportId` Integer. See `demo_`.
#'   \item `Drug_Id` Integer. The unique identifier of each drug report.
#'   \item `MedicinalProd_Id` Integer. The medicinalproduct identifier.
#'   See [get_atc_code()].
#'   \item `DrecNo` Integer. Drug Record Number, pivotal to identify drugs
#'   with [get_drecno()].
#'   \item `Seq1`, `Seq2` Character. Seq 1 and 2 complement DrecNo,
#'   in WHODrug dictionary.
#'   \item `Route` Character. The route of administration of the drug.
#'   \item `Basis` Character. The reputation basis of the drug (suspect,
#'   concomitant, or interacting). `path_sub/RepBasis.parquet`
#'   \item `Amount` Character. The amount of drug administered.
#'   \item `AmountU` Character. The unit of the amount of drug administered.
#'   `path_sub/SizeUnit.parquet`
#'   \item `Frequency` Character. The frequency of drug administration.
#'   \item `FrequencyU` Character. The unit of the frequency of drug
#'   administration. `path_sub/Frequency.parquet`
#'   }
#' `adr_` is a data.table with 4 variables and 2133 rows.
#' \itemize{
#'   \item `UMCReportId` Integer. See `demo_`.
#'   \item `Adr_Id` Integer. The unique identifier of each adverse event report.
#'   \item `MedDRA_Id` Integer. The MedDRA identifier of the adverse event.
#'   It is used in [get_llt_soc()] and [get_llt_smq()].
#'   \item `Outcome` Character. The outcome of the adverse event.
#'   `path_sub/Outcome.parquet`
#'  }
#' `link_` is a data.table with 3 variables and 3514 rows. The version
#' built with [tb_vigibase()] is slightly different than the original one.
#' \itemize{
#'   \item `Drug_Id` and `Adr_Id` . Integers. Together, they are the key variable
#'   of `link`. See `drug_` and `adr_`.
#'   \item `Dechallenge1` and `2` Characters. Dechallenge action and outcome.
#'   `path_sub/Dechallenge.parquet`, `path_sub/Dechallenge2.parquet`
#'   \item `Rechallenge1` and `2` Characters. Rechallenge action and outcome.
#'   `path_sub/Rechallenge.parquet`, `path_sub/Rechallenge2.parquet`
#'   \item `TimeToOnsetMin` and `Max` Numerics. The minimum and maximum time to
#'   onset of the adverse event.
#'   \item `tto_mean` Numeric. The mean time to onset of the adverse event.
#'   It is the average of `TimeToOnsetMin` and `Max`.
#'   \item `range` Numeric. The incertitude around `tto_mean`.
#'   See `vignette("descriptive")`.
#'   \item `UMCReportId` Integer. See `demo_`.
#'   }
#' `ind_` is a data.table with 2 variables and 2426 rows.
#' \itemize{
#'   \item `Drug_Id` Integer. See `drug_`.
#'   \item `Indication` Character. The indication of the drug.
#'  }
#'  `out_` is a data.table with 3 variables and 747 rows.
#'  \itemize{
#'   \item `UMCReportId` Integer. See `demo_`.
#'   \item `Seriousness` Character. The seriousness criteria of the report.
#'   `path_sub/Seriousness.parquet`
#'   \item `Serious` Character. Whether the case is serious or
#'  not ("N" No, "Y" Yes)
#'  }
#'  `srce_` is a data.table with 2 variables and 729 rows.
#'  \itemize{
#'   \item `UMCReportId` Integer. See `demo_`.
#'   \item `Type` Character. The Type of Reporter.
#'   `path_sub/Notifier.parquet`
#'  }
#'  `followup_` is a data.table with 2 variables and 902 rows.
#'  \itemize{
#'   \item `UMCReportId` Integer. See `demo_`.
#'   \item `ReplacedUMCReportId` Integer. Previous version of the case,
#'   which is no longer available in `demo_`.
#'  }
#'
#' @keywords datasets
#'
#' @references There is none
#'
#' @source None
#'
#' @examples
#' data(demo_)
#' demo_ |> dplyr::count(AgeGroup)

"demo_"

#' @rdname demo_

"drug_"

#' @rdname demo_

"adr_"

#' @rdname demo_

"link_"

#' @rdname demo_

"followup_"

#' @rdname demo_

"ind_"

#' @rdname demo_

"out_"

#' @rdname demo_

"srce_"

#' Sample of Meddra.
#'
#' Anonymized data from MedDRA, used to illustrate the package examples and vignettes.
#' You can find term codes related to colitis, pneumonitis, hepatitis, a SMQ of
#' embolisms.
#' Compounds are `meddra_`, `smq_list_`, `smq_content_` and `smq_list_content_`.
#' Create dedicated .parquet files using [tb_meddra()].
#' See examples in \code{\link{get_llt_soc}} and \code{\link{get_llt_smq}}
#'
#' @docType data
#'
#' @usage data(meddra_)
#'
#' @format
#' `meddra_` is a data.table with 15 variables and 677 rows.
#' \itemize{
#'  \item The `*_code` columns. Integers. MedDRA code for the given term.
#'  \item The `*_name` columns. Characters. The name of the term.
#'  \item `soc_abbrev` Character. The abbreviation of the SOC.
#'  \item `null_field` Logical. Empty column.
#'  \item `pt_soc_code` Integer. The preferred term code of the SOC itself.
#'  \item `primary_soc_fg` Character. Whether the SOC is primary for this code.
#'  "Y" or "N", Yes or No.
#'  \item `empty_col` Logical. Empty column.
#'  }
#' `smq_list_` is a data.table with 9 variables and 11 rows.
#' It is the list of SMQ.
#' \itemize{
#'  \item `smq_code` Integer. The code of the SMQ.
#'  \item `smq_name` Character. The name of the SMQ.
#'  \item `smq_level` Integer. The hierarchical level of the SMQ.
#'  \item `smq_description` Character. The description of the SMQ.
#'  \item `smq_source` Character. The source of the SMQ.
#'  \item `smq_note` Character. Additional note on the SMQ.
#'  \item `MedDRA_version` Numeric. The version of MedDRA.
#'  \item `status` Character. The status of the SMQ (active or not)
#'  \item `smq_algorithm` Character. Whether the SMQ is algorithmic or not.
#'  \item `empty_col` Logical. Empty column.
#'  }
#' `smq_content_` is a data.table with 9 variables and 3386 rows.
#' It is the content of each SMQ.
#' \itemize{
#'  \item `smq_code` Integer. The code of the SMQ.
#'  \item `term_code` Integer. The low-level term code.
#'  \item `term_level` Integer. The hierarchical level of the term.
#'  \item `term_scope` Integer. The scope of the term (narrow 2 or broad 1)
#'  \item `term_category` Character. In algorithmic SMQs, the category of the term.
#'  \item `term_weight` Integer. The weight of the term (algorithmic SMQs).
#'  \item `term_status` Integer. The status of the term (active or not)
#'  \item `term_addition_version` Numeric. The version of the term addition.
#'  \item `term_last_modified_version` Numeric. The last MedDRA version
#'  the term was modified.
#'  \item `empty_col` Logical. Empty column.
#'  }
#' `smq_list_content_` is a data.table with 19 variables and 3386 rows.
#' It is a fusion of smq_list and smq_content, as created with [tb_meddra()].
#' \itemize{
#'  \item `smq_code` Integer. The code of the SMQ.
#'  \item `smq_name` Character. The name of the SMQ.
#'  \item `smq_level` Integer. The hierarchical level of the SMQ.
#'  \item `smq_description` Character. The description of the SMQ.
#'  \item `smq_source` Character. The source of the SMQ.
#'  \item `smq_note` Character. Additional note on the SMQ.
#'  \item `MedDRA_version` Numeric. The version of MedDRA.
#'  \item `status` Character. The status of the SMQ (active or not)
#'  \item `smq_algorithm` Character. Whether the SMQ is algorithmic or not.
#'  \item `empty_col.x` Logical. Empty column.
#'  \item `term_code` Integer. The low-level term code.
#'  \item `term_level` Integer. The hierarchical level of the term.
#'  \item `term_scope` Integer. The scope of the term (narrow 2 or broad 1)
#'  \item `term_category` Character. In algorithmic SMQs, the category of the term.
#'  \item `term_weight` Integer. The weight of the term (algorithmic SMQs).
#'  \item `term_status` Integer. The status of the term (active or not)
#'  \item `term_addition_version` Numeric. The version of the term addition.
#'  \item `term_last_modified_version` Numeric. The last MedDRA version
#'  the term was modified.
#'  \item `empty_col.y` Logical. Empty column.
#'  }
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

#' @rdname meddra_

"smq_list_"

#' @rdname meddra_

"smq_content_"

#' Sample of WHODrug
#'
#' A small part of WHODrug, used to illustrate the package examples and vignettes.
#' You can find DrecNo related to immune checkpoint inhibitors, paracetamol,
#' tramadol, tretinoin, anti-thrombin iii, and ATC classes
#' L03AA Colony stimulating factors, C09AA ACE inhibitors, plain,
#' J01CA Penicillins with extended spectrum.
#' Compounds are `thg_` and `mp_`.
#' See examples in \code{\link{get_drecno}} and \code{\link{get_atc_code}}
#'
#' @docType data
#'
#' @usage data(mp_)
#'
#' @format
#' `mp_` is a data.table with 8 variables and 14146 rows.
#' \itemize{
#'   \item `MedicinalProd_Id` Integer. The medicinalproduct identifier.
#'   \item `Sequence.number.1` and `2` Characters. Complement to DrecNo.
#'   \item `DrecNo` Character. Drug Record Number, pivotal to identify drugs
#'   with [get_drecno()].
#'   \item `drug_name_t` Character. The name of the drug. Compared to the
#'   original `drug_name` variable in `mp` table,
#'   this variable is trimmed for white spaces, and names
#'   are in lowercase.
#'   \item `Create.date` Character. The date the record was created.
#'   \item `Date.changed` Character. The date the record was last changed.
#'   \item `Country` Character. The country where the record was created.
#'   }
#' `thg_` is a data.table with 5 variables and 4079 rows.
#' \itemize{
#'   \item `Therapgroup_Id` Integer. The identifier of the therapeutic group.
#'   \item `ATC.code` Character. The ATC code of the drug.
#'   \item `Create.date` Character. The date the record was created.
#'   \item `Official.ATC.code` Character. Whether the ATC code
#'   is official (Yes/No).
#'   \item `MedicinalProd_Id` Integer. The medicinalproduct identifier.
#'   }
#'
#' @keywords datasets, whodrug
#'
#' @references There is none
#'
#' @source None
#'
#' @examples
#' data(mp_)

"mp_"

#' @rdname mp_

"thg_"

#' Data for the immune checkpoint inhibitors example
#'
#' These are a set of data to provide examples on the package.
#' \itemize{
#'   \item `smq_sel` is a named list of smq names
#'   \item `pt_sel` is a named list of pt names
#'   \item `a_llt` is a named list of meddra llt codes related to adrs from `smq_sel` and `pt_sel`
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
