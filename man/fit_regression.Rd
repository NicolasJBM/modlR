% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_regression.R
\name{fit_regression}
\alias{fit_regression}
\title{Compute the paths coefficients for analayses of moderated mediations.}
\usage{
fit_regression(est_sample, est_formula, est_method, est_family)
}
\arguments{
\item{est_sample}{Tibble. Table containing the data.}

\item{est_formula}{Formula or list of formulas for a system of equations.}

\item{est_method}{Character. Method for the regression ("glm", "felm", or "sysfit").}

\item{est_family}{Character. Family for glm (e.g. "gaussian", "binomial").}
}
\value{
A tibble gathering the regression coefficients.
}
\description{
Compute the paths coefficients for analayses of moderated mediations.
}
\references{
Arne Henningsen and Jeff D. Hamann (2007). systemfit: A Package for Estimating Systems of Simultaneous Equations in R. Journal of Statistical Software 23(4), 1-40. http://www.jstatsoft.org/v23/i04/.
}
