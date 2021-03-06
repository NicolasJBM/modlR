% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_path.R
\name{analyze_path}
\alias{analyze_path}
\title{Compute the moderated paths coefficients for analyses of moderated mediations.}
\usage{
analyze_path(coefficients, moderations, mediations)
}
\arguments{
\item{coefficients}{Tibble. Output of the function compute_coefficients.}

\item{moderations}{Tibble. Output of the function retrieve_modval with option "allval" FALSE.}

\item{mediations}{List. List of vectors specifying the paths, from the first independent variable to the last dependent variable through the moderators in order.}
}
\value{
A tibble gathering all (direct, indirect, and total) the moderated path coefficients.
}
\description{
Compute the moderated paths coefficients for analyses of moderated mediations.
}
\references{
Edwards, J. R., & Lambert, L. S. (2007). Methods for integrating moderation and mediation: a general analytical framework using moderated path analysis. Psychological Methods, 12(1), 1–22.
}
