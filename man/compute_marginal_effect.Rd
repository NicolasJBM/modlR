% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_marginal_effect.R
\name{compute_marginal_effect}
\alias{compute_marginal_effect}
\title{Compute the moderated direct paths coefficients for analayses of moderated mediations.}
\usage{
compute_marginal_effect(
  coefficients,
  moderations,
  indep,
  mediate = NA,
  dep,
  moderate,
  p
)
}
\arguments{
\item{coefficients}{Tibble. Output of the function compute_coefficients.}

\item{moderations}{Tibble. Output of the function retrieve_modval with option "allval" TRUE.}

\item{indep}{Character. Name of the independent variable.}

\item{mediate}{Character. Name of the mediating variable (if indirect path).}

\item{dep}{Character. Name of the dependent variable.}

\item{moderate}{Character. Name of the moderating variable.}

\item{p}{Double. Significance threshold.}
}
\value{
A tibble gathering the coefficient associating the independent pas dependent variables for all values of the moderator.
}
\description{
Compute the moderated direct paths coefficients for analayses of moderated mediations.
}
\seealso{
analyze_path
}
