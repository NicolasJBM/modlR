#' Compute the paths coefficients for analayses of moderated mediations.
#' @param est_sample  Tibble. Table containing the data.
#' @param est_formula Formula or list of formulas for a system of equations.
#' @param est_method  Character. Method for the regression ("glm", "felm", or "sysfit").
#' @param est_family  Character. Family for glm (e.g. "gaussian", "binomial").
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


fit_regression <- function(est_sample, est_formula, est_method, est_family){
  
  # Bind variables
  term <- NULL
  dependent <- NULL
  Estimate <- NULL
  estimate <- NULL
  std.error <- NULL
  p.value <- NULL
  
  if (is.na(est_family)) est_family <- "gaussian"
  
  if (est_method == "sysfit"){
    dep <- stringr::str_split(as.character(est_formula), "~")
    for (i in 1:length(dep)) dep[[i]] <- dep[[i]][1]
    dep <- trimws(unlist(dep))
  } else {
    dep <- stringr::str_split(as.character(est_formula[2]), "~")
    dep <- unlist(dep)
  }
  
  fit <- switch(
    est_method,
    glm = glm(est_formula, data = est_sample, family = est_family),
    felm = lfe::felm(est_formula, est_sample),
    sysfit = systemfit::systemfit(formula = est_formula, data = est_sample, method = "SUR")
  )
  
  if (est_method == "sysfit"){
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
  
  rm(est_sample, est_formula, est_method, est_family)
  gc()
  
  return(fit)
}