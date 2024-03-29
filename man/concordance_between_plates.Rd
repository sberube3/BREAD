% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concordance_between_plates.R
\name{concordance_between_plates}
\alias{concordance_between_plates}
\title{Computes Lin's Concordance Correlation Coefficient on repeated
measurements across plates}
\usage{
concordance_between_plates(data_df, num_plates, stage)
}
\arguments{
\item{data_df}{a data frame is the output of either filter_low_beads,
get_concentration, or normalize_between_plates where the outputs for
all plates in the study are bound together (rbind) into one large data
frame.}

\item{num_plates}{numeric value, indicating the number of plates
to be compared.}

\item{stage}{a string describing the stage of analysis being
analysed either "Background", "Std_Curve" or "Normalized".}
}
\value{
\code{concordance_between_plates} returns a data frame
showing the concordance correlation coefficient (with 95\% CI)
across all pairs of plates.
}
\description{
This function takes in MFI or transformed MFI data from various stages
of the cleaning and normalization process and computes Lin's
Concordance Correlation Coefficient on repeated measurements across pairs
of plates.
}
\examples{
R code examples of how to use your function

}
