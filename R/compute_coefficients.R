#' Compute the simple coefficients for several models
#' @param est_sample Tibble. Initial sample.
#' @param md_imputation Character. Kind of imputation ("multiple","single","deletion")
#' @param md_method Character. Method for the imputation of missing data ("pmm", "mean", "sample", "deletion").
#' @param size Integer. Size of the sample for each draw. If NA, use the sample size.
#' @param formulas Vector of formulas. The various models to run on a sample.
#' @param est_method  Character. Method for the regression ("glm", "felm", or "sysfit").
#' @param est_family  Character. Family for glm (e.g. "gaussian", "binomial") .
#' @return A tibble gwith all the coefficients associating each dependent, independent, and moderating variables.
#' @seealso estimate
#' @importFrom tibble tibble
#' @importFrom tibble is_tibble
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr pmap
#' @importFrom purrr map
#' @importFrom stringr str_replace_all
#' @export


compute_coefficients <- function(est_sample = NULL,
                                 md_imputation = "multiple",
                                 md_method = "pmm",
                                 size = NA,
                                 formulas = NULL,
                                 est_method = "felm",
                                 est_family = NA){
  
  stopifnot(
    !is.null(est_sample),
    !is.null(formulas)
  )
  
  # Bind variables
  independent <- NULL
  error <- NULL
  pval <- NULL
  moderator <- NULL
  coefficient <- NULL
  moderation <- NULL
  base <- NULL
  dependent <- NULL
  data <- NULL
  relation <- NULL
  path <- NULL
  moderator <- NULL
  moderation <- NULL
  
  est_sample <- modlR::prepare_sample(est_sample = est_sample, md_imputation = md_imputation, md_method = md_method, size = size)
  estimations <- list()
  for (i in 1:length(formulas)){
    estimations[[i]] <- modlR::fit_regression(est_sample = est_sample, est_formula = formulas[[i]], est_method = est_method, est_family = est_family)
  }
  
  estimations <- estimations %>%
    bind_rows() %>%
    dplyr::mutate(independent = stringr::str_replace_all(independent, '\\(Intercept\\)','constant')) %>%
    tidyr::separate(independent, into = c("independent","moderator"), sep = ".x.", fill = "right") %>%
    dplyr::select(-error, -pval) %>%
    tidyr::replace_na(list(moderator = "base"))
  
  prep_path <- function(x){
    x %>%
      tidyr::spread(moderator, coefficient) %>%
      tidyr::gather(moderator, moderation, -base)
  }
  
  estimations <- estimations %>%
    dplyr::group_by(dependent, independent) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, prep_path)) %>%
    tidyr::unnest(data) %>%
    tidyr::replace_na(list(moderator = "base", moderation = 0)) %>%
    dplyr::mutate(
      path = paste(independent, dependent, sep = "_"),
      relation = "direct"
    ) %>%
    dplyr::select(relation, path, dependent, independent, moderator, base, moderation)
  
  return(estimations)
}