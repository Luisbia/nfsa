% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nfsa_read_xml.R
\name{nfsa_read_xml}
\alias{nfsa_read_xml}
\title{Read an xml file or several xml files}
\usage{
nfsa_read_xml(file_sel, folder_sel, interactive = TRUE)
}
\arguments{
\item{file_sel}{the file(s) to read}

\item{folder_sel}{the folder to look for xml files}

\item{interactive}{if the user choose the file via the menu}
}
\value{
a data frame
}
\description{
Pass a list of files or select one interactively.
}
\examples{
tmp <- nfsa_read_xml()
tmp1 <- nfsa_read_xml(file_sel = "M:/Incoming_SDMX_files/NASEC_T0800_A_SK_2024_0000_V0003.xml",
interactive = FALSE)
}
