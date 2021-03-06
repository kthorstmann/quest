% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qscales.R
\name{codeScales}
\alias{codeScales}
\title{codeScales}
\usage{
codeScales(data, scales, recode = TRUE, valid.values = NULL,
  return.both = FALSE)
}
\arguments{
\item{data}{A data frame that contains \emph{only} the values of the questionnaire.}

\item{scales}{A character vector that indicates which items belong to which scale}

\item{recode}{Logical. Should the values be recoded automatically? Default is to \code{TRUE}}

\item{valid.values}{Vector of numeric values that are valid for the questionnaire, i. e. the numeric transformation of the likert scale.}

\item{return.both}{Logical. Should the original data frame and the new scales be returned? Defaul is ti \code{FALSE}, which returnes only the scores scales.}
}
\value{
A data frame wich contains the scored scales of the questionnaire used.
}
\description{
A function to compute the means for each participant in a certain questionnaire. Any questionnaire contains a table that indicates which items belong to which scale and which items are recoded. This table can be inserted into the function via the argument \code{scales}.
}
\examples{
scales <-  c("E = 1r, 6, 11R, 16",
             "A = 2r, 7, 12r, 17r",
             "C = 3, 8r, 13, 18",
             "N = 4, 9r, 14, 19",
             "O = 5, 10, 15, 20 21")
data <- bigfive[grep(x = names(bigfive), pattern = "BFI_")]
transformed.data <- codeScales(data, scales, recode = TRUE, return.both = FALSE)
}

