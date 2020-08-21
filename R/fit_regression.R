#' Compute the paths coefficients for analayses of moderated mediations.
#' @param sample  Tibble. Table containing the data.
#' @param formula Formula or list of formulas for a system of equations.
#' @param method  Character. Method for the regression ("glm", "felm", or "sysfit").
#' @param family  Character. Family for glm (e.g. "gaussian", "binomial").
#' @return A tibble gathering the regression coefficients.
#' @references Arne Henningsen and Jeff D. Hamann (2007). systemfit: A Package for Estimating Systems of Simultaneous Equations in R. Journal of Statistical Software 23(4), 1-40. http://www.jstatsoft.org/v23/i04/.
#' @importFrom systemfit systemfit
#' @importFrom lfe felm
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom broom tidy
#' @importFrom purrr map
#' @importFrom stats glm
#' @importFrom stringr str_split
#' @importFrom stringr str_extract
#' @importFrom tidyr separate
#' @export


fit_regression <- function(sample, formula, method, family){
  
  # Bind variables
  term <- NULL
  dependent <- NULL
  Estimate <- NULL
  estimate <- NULL
  std.error <- NULL
  p.value <- NULL
  
  if (is.na(family)) family <- "gaussian"
  
  if (method == "sysfit"){
    dep <- stringr::str_split(as.character(formula), "~")
    for (i in 1:length(dep)) dep[[i]] <- dep[[i]][1]
    dep <- trimws(unlist(dep))
  } else {
    dep <- stringr::str_split(as.character(formula[2]), "~")
    dep <- unlist(dep)
  }
  
  fit <- switch(
    method,
    glm = glm(formula, data = sample, family = family),
    felm = lfe::felm(formula, sample),
    sysfit = systemfit::systemfit(formula = formula, data = sample, method = "SUR")
  )
  
  if (method == "sysfit"){
    fit <- as.data.frame(summary(fit)$coefficients)
    fit <- tibble::rownames_to_column(fit, "term")
    fit <- tidyr::separate(fit, term, into = c("dependent","term"), sep = "_", extra = "merge")
    fit <- dplyr::mutate(fit, dependent = stringr::str_extract(dependent, "[0-9]"))
    fit <- dplyr::mutate(fit, dependent = purrr::map(dependent, function(x,dep) dep[as.numeric(x)], dep = dep))
    fit <- dplyr::select(fit, dependent, independent = term, coefficient = Estimate, error = 'Std. Error', pval = 'Pr(>|t|)')
    fit <- dplyr::mutate(fit, dependent = unlist(dependent))
  } else {
    fit <- broom::tidy(fit)
    fit <- dplyr::mutate(fit, dependent = dep)
    fit <- dplyr::select(fit, dependent, independent = term, coefficient = estimate, error = std.error, pval = p.value)
  }
  
  rm(sample, formula, method, family)
  gc()
  
  return(fit)
}