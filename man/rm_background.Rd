% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm_background.R
\name{rm_background}
\alias{rm_background}
\title{Performing background correction on raw measurements.}
\usage{
rm_background(clean_df, method)
}
\arguments{
\item{clean_df}{data frame that is the output of read_and_tidy function}

\item{method}{a string indicates whether background correction
should be performed by division or subtraction}
}
\value{
\code{rm_background} returns a data frame containing
an additional column in the original data frame from read_and_tidy
output called MFI_BG, which is the background corrected MFI.
}
\description{
This function takes the ouput of the read_and_tidy function
and performs background correction on all measurements either through
subtraction, which is median fluoresecnce intensity (MFI) minus
the median of MFI among background wells on the plate,
or division which is MFI divided by the median of MFI among
background wells on the plate. This is done on an individual plate basis.
}
