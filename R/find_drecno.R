
#' Drug record number finder
#'
#' Find drug record number from character string matching from a modified MP data.table.
#'
#' This function is superseeded by \code{\link{get_drecno}}.

#' @param x A character vector of either drug names (use WHO names, see details) or MedicinalProd_Ids to be matched.
#' @param mp_short A modified data.table of medicinal products, see details.
#' @param allow_combination A logical. Should fixed associations including the drug of interest be retrieved? See details.
#' @param method Should DrecNo be found from drug names or from MedicinalProd_Id?
#' @param show_all Do you wish to see all MP_Ids entries from `mp_short` when working with drug names, or just one entry per DrecNo? Default to FALSE
#' @param mpi_meth_name If you use mpi_list, this name will be printed in the drug column.
#' @seealso \code{\link{pharmacocaen-deprecated}}
#' @keywords drug drecno
#' @rdname pharmacocaen-deprecated
#' @section \code{find_drecno}:
#' For \code{find_drecno}, use \code{\link{get_drecno}}.
#'
#' @export
#' @import data.table
#' @examples


# Matching from mp_short

find_drecno <- function(x,
                        mp_short,
                        allow_combination = TRUE,
                        method = c("drug_name", "mpi_list"),
                        show_all = FALSE,
                        mpi_meth_name = "atc")
{
  .Defunct("get_drecno")

  method <- match.arg(method)

  if(method == "mpi_list" && allow_combination)
    warning("allow_combination set to TRUE but mpi requested")

  drug_name_reshaped <- tolower(trimws(x))
  names(drug_name_reshaped) <- drug_name_reshaped

  find_combination <- function(x_drug_name, env = mp_short){
    x_drug_name <-
      gsub("\\(", "\\\\(", x_drug_name) %>% # so that parenthesis are appropriately escaped
      gsub("\\)", "\\\\)", .)
    eval(rlang::expr(grepl(paste0("(?<![[:alpha:]])", x_drug_name, "(?![\\s[:alpha:]])"),
                           drug_name_t,
                           perl = TRUE)),
         envir = env)
         # negative lookbehind: x is not preceeded by alphabetic characters
         # negative lookahead: x is not followed by a space or an alphabetic character.
  }

  find_isolated <- function(x_drug_name, env = mp_short)
    eval(rlang::expr(drug_name_t == x_drug_name), envir = env) # exact match

  find_mpi <- function(x_mpi_list, env = mp_short)
    eval(rlang::expr(MedicinalProd_Id %in% x_mpi_list), envir = env)


  # ---- Drug_name finding ----

  if (method == "drug_name") {
    # Pick one
    find_select <-
      if (allow_combination) {
        find_combination
      } else {
        find_isolated
      }

    # drug finder and exist checker (works for a single drug at a time, then it is used in an lapply)

    drug_finder_and_exist_checker <- function(x) {
      # single drug checking
      if (length(x) > 1)
        stop(
          "function is meant to be used for a single drug at a time. This is likely an internal error message"
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

    res <- data.table::rbindlist(lapply(drug_name_reshaped,
                                        drug_finder_and_exist_checker),
                                 idcol = "drug")
  }

  # ---- MedicinalProd_Id finding ----

    if(method == "mpi_list") {
      res <-
        mp_short[find_mpi(x), ]

      res[, c("drug") := .(mpi_meth_name)]
    }


  # ---- Return results ----

  if (show_all)
  {
    res

    } else {

      unique(res[Sequence.number.1 == "01" &
                   Sequence.number.2 == "001",], by = c("drug", "DrecNo"))
      }
}
