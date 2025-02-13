% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_adr.R
\name{add_adr}
\alias{add_adr}
\title{Add ADR column(s) to a dataset}
\usage{
add_adr(
  .data,
  a_code,
  a_names = names(a_code),
  adr_data,
  data_type = deprecated()
)
}
\arguments{
\item{.data}{The dataset to update (demo, link, drug, adr).}

\item{a_code}{A named list of low level terms codes (llt_codes).}

\item{a_names}{A character vector. Names for adr columns (must be the
same length as adr_list), default to \code{names(a_code)}}

\item{adr_data}{A data.frame containing the adr data (usually, it is \code{adr})}

\item{data_type}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}. Data_type is now detected
internally.}
}
\value{
A dataset with the new adr columns.
Each element of \code{a_names} will add a column with the same name in \code{.data}.
The value can be 0 (the corresponding adr is absent)
or 1 (the adr is present in the case if \code{.data} is \code{demo} or \code{drug},
or "this row correspond to this adr", if \code{.data} is \code{adr} or \code{link}).
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}Creates adr columns
in vigibase datasets (demo, link, drug, but also adr).
}
\details{
Low-level term codes are the preferred level of requesting in
Vigibase extract case level since they capture all possible codes for a given
Preferred Term. Collect low-level terms with \code{\link[=get_llt_soc]{get_llt_soc()}} and
\code{\link[=get_llt_smq]{get_llt_smq()}}. You can add adr identification to a \code{demo}, a \code{link}, \code{drug}
or even an \code{adr} dataset (in this latter case, you must provide \code{adr} twice,
as \code{.data} and \code{adr_data}). Column names of these dataset should not have been
modified from the original vigibase dataset (as created with \code{\link[=tb_vigibase]{tb_vigibase()}}).
}
\examples{
# create adr_colitis, adr_embolism and adr_pneumonitis columns in demo

# be careful, this example may overwrite your own demo dataset
demo <- demo_

a_pt_sel <- ex_$pt_sel


adr <- adr_

a_llt <-
  get_llt_soc(
  term_sel = a_pt_sel,
  term_level = "pt",
  meddra = meddra_
  )

demo <-
  demo |>
    add_adr(
      a_code = a_llt,
      adr_data = adr
    )

demo |>
  check_dm(names(a_pt_sel))
}
\seealso{
\code{\link[=add_drug]{add_drug()}}, \code{\link[=get_llt_soc]{get_llt_soc()}}, \code{\link[=get_llt_smq]{get_llt_smq()}}
}
\keyword{adr}
\keyword{data_management}
