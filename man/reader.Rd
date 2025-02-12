% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reader.R
\name{reader}
\alias{reader}
\title{Internal arrow reader}
\usage{
reader(file_name, folder)
}
\arguments{
\item{file_name}{Character string}

\item{folder}{Character string}
}
\description{
Used in \code{\link[=tb_vigibase]{tb_vigibase()}}, \code{\link[=tb_who]{tb_who()}}, \code{\link[=tb_meddra]{tb_meddra()}}
}
\examples{

demo <-data.frame(f0= c("96548661   32194501051119460820"))

tmp_folder <- tempdir()

path_base <- paste0(tmp_folder, "/")

write.table(demo, file = paste0(path_base, "DEMO.txt"),
            row.names = FALSE, quote = FALSE, col.names = FALSE)

vigicaen:::reader("DEMO.txt", path_base)
}
