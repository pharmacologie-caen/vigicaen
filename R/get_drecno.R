#' Get DrecNo from drug names
#'
#' This function gets Drug Record Numbers associated to one or more drugs in a list.
#'
#' This is a function of the "create a drug column" workflow.
#' Use `inspect = TRUE` whenever you use this function for the first time on a new set of drug names. It is a critical checkpoint to your data analysis.
#' This function uses perl style regex to find drug names in generic names OR exact matching to MedicinalProd_Id, from the WHO shortened MP tables.
#' A drug can have multiple MedicinalProd_Ids, corresponding to different packagings. The MedicinalProd_Id matching is typically used to identify DrecNo(s) contained in an ATC class (extracted from THG), since not all MPI of drugs are present in THG (see Helena Skold 2020-07-01 mail for details).
#' WHO names are attributed to drugs by... the WHO. There is only one WHO name, when there can be multiple international nonproprietary names (e.g. tretinoin and all-trans retinoic acid). You should use WHO names to ensure proper identification of drugs and DrecNos.
#' Negative lookarounds are used to ensure that a string does not match to composite drug names including the string, i.e. `trastuzumab emtasine` is not retrieved when looking for `trastuzumab` and `alitretinoin` is not found when looking for `tretinoin`.
#' Fixed associations of drugs refers to specialty containing more than one active ingredient (for example, acetylsalicylic acid and clopidogrel). In VigiLyze, the default is NOT to account for these fixed associations. For example, when you call "acetylsalicylic acid" in VigiLyze, you don't have the cases reported with the fixed-association "acetylsalicylic acid; clopidogrel" **unless the substances were distinctly coded by the reporter.** Here, the default is to find a drug even if it is prescribed in a fixed association.
#' Importantly, when retrieving fixed-association drugs, the non-of-interest drug alone drecno is not found, hence the cases related to this drug will not be added to those of the drug of interest.
#' Drug names are automatically `tolower(trimws())`-ed in the function.
#'   d_sel must be a named list of character vectors. Character vectors do not work. The goal is to be able to group drugs in a list if needed and to add them in demo as a single class in a single column.
#'
#' @param d_sel A named list of character vectors. Selection of drug names or medicinalprod_id. See details
#' @param mp_short A modified MP data.table. See \code{\link{ex_}}
#' @param allow_combination A logical. Should fixed associations including the drug of interest be retrieved? See details.
#' @param method Should DrecNo be found from drug names or from MedicinalProd_Id?
#' @param inspect Instead of providing a list of drecnos, do you wish to see a larger data.frame with additional data? See details.
#' @param show_all Do you wish to see all MP_Ids entries from `mp_short` when working with drug names, or just one entry per DrecNo? Default to FALSE
#' @keywords atc
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @examples
#'
#' # ## Get drecnos for a list a drugs. Check spelling and/or use WHO name. Usually in lowercase
#'
#' d_sel_names <- rlang::list2(
#'   nivolumab = "nivolumab",
#'   ipilimumab = "ipilimumab",
#'   nivo_ipi = c("nivolumab", "ipilimumab")
#'   )
#'
#' # Read mp_short with get_drecno, to identify drugs without combinations
#'
#' # First, you shall **always** inspect mp_short reading before getting the codes
#'
#' get_drecno(d_sel_names,
#'            mp_short = mp_short_,
#'            allow_combination = FALSE,
#'            method = "drug_name",
#'            inspect = TRUE)
#'
#'
#' # If this matches your needs, then extract (inspect = FALSE, by default)
#'
#' d_drecno <-
#'   get_drecno(d_sel_names,
#'              mp_short = mp_short_,
#'              allow_combination = FALSE,
#'              method = "drug_name")
#' d_drecno
#'
#' # And DrecNos of drugs allowing for combinations
#'
#' d_drecno <-
#' get_drecno(d_sel = d_sel_names,
#'             mp_short = mp_short_,
#'             allow_combination = TRUE,
#'             method = "drug_name"
#'             )
#' d_drecno

get_drecno <- function(
    d_sel,
    mp_short,
    allow_combination = TRUE,
    method = c("drug_name", "mpi_list"),
    inspect = FALSE,
    show_all = FALSE
    ){

  method <- match.arg(method)

  if(method == "mpi_list" && allow_combination)
    warning("allow_combination set to TRUE but mpi requested")

  d_sel_renamed <-
    d_sel |>
    rlang::set_names(
      ~ .x |>
        stringr::str_trim() |>
        stringr::str_to_lower()
    )

  if(!all(names(d_sel) == names(d_sel_renamed))){
    warning("names of d_sel were tolower-ed and trimed")
  }

  find_combination <- function(x_drug_name, env = mp_short,
                               drug_name_t = {{ drug_name_t }}){
    x_drug_name <-
      gsub("\\(", "\\\\(", x_drug_name)

    x_drug_name <- # so that parenthesis are appropriately escaped
      gsub("\\)", "\\\\)", x_drug_name)

    eval(rlang::expr(grepl(paste0("(?<![[:alpha:]])", x_drug_name, "(?![\\s[:alpha:]])"),
                           drug_name_t,
                           perl = TRUE)),
         envir = env)
    # negative lookbehind: x is not preceeded by alphabetic characters
    # negative lookahead: x is not followed by a space or an alphabetic character.
  }

  find_isolated <- function(x_drug_name, env = mp_short,
                            drug_name_t = {{ drug_name_t }})
    eval(rlang::expr(drug_name_t == x_drug_name), envir = env) # exact match

  find_mpi <- function(x_mpi_list, env = mp_short,
                       MedicinalProd_Id = {{ MedicinalProd_Id }})
    eval(rlang::expr(MedicinalProd_Id %in% x_mpi_list), envir = env)


  # ---- Core function for drug_name finding ----

  # drug finder and exist checker (works for a single drug at a time, then it is vectorized)

  find_drug_and_check_exist <-
    function(x,
             Sequence.number.1 = {{ Sequence.number.1 }},
             Sequence.number.2 = {{ Sequence.number.2 }},
             DrecNo = {{ DrecNo }},
             drug_name_t = {{ drug_name_t }}) {
    # single drug checking
    if (length(x) > 1)
      stop(
        "Function is meant to be used for a single drug at a time. Check `d_sel` structure."
      )

    # drug finder
    drecno_list <- mp_short[find_select(x),]
    # 2022 06 25 il faut ajouter, quelque part par ici, un checker sur le nom du medicament isole, pour ne pas admettre des erreurs dorthographe dans le find combination, qui est plus permissif que find isolated. ou plutot modifier find combination pour etre moins permissif aux noms incomplets.

    # exist checker
    if (nrow(drecno_list) == 0)
      warning(paste0(
        "there is no match for `",
        x,
        "`, check spelling and/or use WHO name"
      ))

    # exist + WHO name checker
    if (nrow(drecno_list) > 0 &&
        nrow(drecno_list[Sequence.number.1 == "01" &
                         Sequence.number.2 == "001",]) == 0) {
      drecno <-
        drecno_list[find_isolated(x, env = drecno_list), unique(DrecNo)]
      # there can be combinations and there can be multiple MP_Ids

      who_name <- mp_short[DrecNo == drecno  &
                             Sequence.number.1 == "01" &
                             Sequence.number.2 == "001",
                           unique(drug_name_t)] # there can be multiple packagings for a non WHO name drug

      warning(
        paste0(
          "`",
          x,
          "` is NOT a WHO name. WHO name is `",
          who_name,
          "`. DrecNo will be missing if show_all is set to FALSE. You should rename to `",
          who_name,
          "`."
        )
      )

    }

    # return
    drecno_list
  }

  # ---- drug name finding ----

  if (method == "drug_name") {
    # Pick one
    find_select <-
      if (allow_combination) {
        find_combination
      } else {
        find_isolated
      }

    # res <- purrr::map_dfr(
    #   drug_name_reshaped,
    #   find_drug_and_check_exist,
    #   .id = "drug")

    res_list <-
      d_sel_renamed |>
      purrr::map(function(d_n) # 2 level map, because find_drug_and_check_exist is working with an atomic character vector
        purrr::map_dfr(d_n,
                       find_drug_and_check_exist,
                       .id = "drug"
                       ))
  }

  # drug_finder_and_exist_checker(c("nivolumab", "ipilimumab"))

  # ---- MedicinalProd_Id finding ----

  if(method == "mpi_list") {

    res_list <-
      purrr::map(d_sel_renamed, function(d_n)
        mp_short[find_mpi(d_n), ]
      )

  }


  # ---- Return results ----

  if (show_all)
  {
    res_list

  } else {

    if(inspect == TRUE) {
      purrr::map(res_list, function(r_l)
       r_l |>
         dplyr::filter(
           .data$Sequence.number.1 == "01" &
             .data$Sequence.number.2 == "001"
         ) |>
         dplyr::distinct(.data$drug, .data$DrecNo, .keep_all = TRUE)
      )
    } else {
      purrr::map(res_list, function(r_l,
                                    DrecNo = {{ DrecNo }})
        r_l[, unique(DrecNo)]
        )
    }


  }
}
