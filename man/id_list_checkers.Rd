% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_id_list.R
\name{id_list_checkers}
\alias{id_list_checkers}
\alias{check_id_list}
\alias{check_id_list_numeric}
\title{Internal id list checkers}
\usage{
check_id_list(
  id_list,
  arg = rlang::caller_arg(id_list),
  call = rlang::caller_env()
)

check_id_list_numeric(
  id_list,
  arg = rlang::caller_arg(id_list),
  call = rlang::caller_env()
)
}
\arguments{
\item{id_list}{The id list to check}

\item{arg}{Helper to format the error message.}

\item{call}{Helper to format the error message.}
}
\value{
An error if the dataset is invalid. Nothing in other cases
}
\description{
Internal helpers to check id list are appropriate named lists.
}
\section{Functions}{
\itemize{
\item \code{check_id_list()}: named list checker

\item \code{check_id_list_numeric()}: numeric list id checker

}}
\examples{

drug_valid <- data.frame(
  DrecNo = 1, UMCReportId = 1, MedicinalProd_Id = 1, Drug_Id = 1)

 vigicaen:::check_data_drug(drug_valid, ".data")

}
