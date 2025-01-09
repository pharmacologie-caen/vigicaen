#' Drug record number finder
#'
#' @description **Defunct** Find drug record
#' number from character string matching from a modified MP data.table.
#'
#' This function is superseeded by \code{\link{get_drecno}}.

#' @param x A character vector of either drug names (use WHO names, see details) or MedicinalProd_Ids to be matched.
#' @param mp A modified data.table of medicinal products, see details.
#' @param allow_combination A logical. Should fixed associations including the drug of interest be retrieved? See details.
#' @param method Should DrecNo be found from drug names or from MedicinalProd_Id?
#' @param show_all Do you wish to see all MP_Ids entries from `mp` when working with drug names, or just one entry per DrecNo? Default to FALSE
#' @param mpi_meth_name If you use mpi_list, this name will be printed in the drug column.
#' @seealso \code{\link{vigicaen-deprecated}}
#' @keywords drug drecno
#' @rdname vigicaen-deprecated
#' @section \code{find_drecno}:
#' For \code{find_drecno}, use \code{\link{get_drecno}}.
#'
#' @export

# Matching from mp

find_drecno <- function(x,
                        mp,
                        allow_combination = TRUE,
                        method = c("drug_name", "mpi_list"),
                        show_all = FALSE,
                        mpi_meth_name = "atc")
{
  .Defunct("get_drecno")

}
