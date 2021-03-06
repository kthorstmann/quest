% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qscales.R
\name{shortKeys}
\alias{shortKeys}
\title{Short Keys for Questionnaires}
\usage{
shortKeys(keytable = "BFI-K")
}
\arguments{
\item{keytable}{The short name of the questionnaire that should be analyzed. See details for all available questionnaires.}
}
\value{
Returns a vector with short keys readable by the package \code{quest} for further analyses.
}
\description{
Insert the commonly used short name of a questionnaire to generate the
}
\details{
Currently available questionnaires are:
\itemize{
\item \href{www.google.com}{BFI-K} QUELLE LINK
\item NEO-PI-R
}
}
\examples{
shortKeys("BFI-K")
}

