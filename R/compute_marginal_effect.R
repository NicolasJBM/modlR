#' Compute the moderated direct paths coefficients for analayses of moderated mediations.
#' @param coefficients Tibble. Output of the function compute_coefficients.
#' @param moderations Tibble. Output of the function retrieve_modval with option "allval" TRUE.
#' @param indep Character. Name of the independent variable.
#' @param mediate Character. Name of the mediating variable (if indirect path).
#' @param dep Character. Name of the dependent variable.
#' @param moderate Character. Name of the moderating variable.
#' @param p Double. Significance threshold.
#' @return A tibble gathering the coefficient associating the independent pas dependent variables for all values of the moderator.
#' @seealso analyze_path
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom stats qnorm
#' @export

compute_marginal_effect <- function(coefficients, moderations, indep, mediate = NA, dep, moderate, p){
  
  
  # bind variables
  init <- NULL
  mult <- NULL
  moderator <- NULL
  mediator <- NULL
  dependent <- NULL
  independent <- NULL
  moderation <- NULL
  modval <- NULL
  coeff <- NULL
  std.error <- NULL
  draw <- NULL
  base <- NULL
  estimate <- NULL
  
  if (is.na(mediate)){
    marginal_effect <- dplyr::filter(coefficients, moderator == moderate, dependent == dep, independent == indep) %>%
      dplyr::left_join(dplyr::filter(moderations, moderator == moderate), by = "moderator") %>%
      dplyr::mutate(coeff = base + moderation * modval) %>%
      dplyr::select(independent, dependent, moderator, modval, coeff) %>%
      dplyr::group_by(independent, dependent, moderator, modval) %>%
      dplyr::summarise(estimate = mean(coeff), std.error = sd(coeff)) %>% 
      dplyr::mutate( 
        lower_bound = estimate - std.error * stats::qnorm(1-p/2), 
        upper_bound = estimate + std.error * stats::qnorm(1-p/2) 
      ) %>%
      dplyr::mutate(relation = paste("relation between", independent, "and", dependent, sep = " "))
  } else {
    
    first_stage <- dplyr::filter(coefficients, independent == indep, dependent == mediate)
    second_stage <- dplyr::filter(coefficients, independent == mediate, dependent == dep)
    
    if (unique(first_stage$moderator) == "base"){
      first_stage <- first_stage %>%
        dplyr::select(mediator = dependent, independent, draw, init = base)
    } else {
      first_stage <- first_stage %>%
        dplyr::filter(moderator == moderate) %>%
        dplyr::left_join(moderations, by = "moderator") %>%
        dplyr::mutate(init = base + moderation * modval) %>%
        dplyr::select(mediator = independent, dependent, moderator, draw, modval, init) 
    }
    
    if (unique(second_stage$moderator) == "base"){
      second_stage <- second_stage %>%
        dplyr::select(mediator = dependent, independent, draw, coeff = base)
    } else {
      second_stage <- second_stage %>%
        dplyr::filter(moderator == moderate) %>%
        dplyr::left_join(moderations, by = "moderator") %>%
        dplyr::mutate(mult = base + moderation * modval) %>%
        dplyr::select(mediator = independent, dependent, moderator, draw, modval, mult) 
    }
    
    if ("modval" %in% names(first_stage) & "modval" %in% names(second_stage)){
      marginal_effect <- first_stage %>%
        dplyr::left_join(second_stage, by = c("mediator","moderator","draw","modval"))
    } else {
      marginal_effect <- first_stage %>%
        dplyr::left_join(second_stage, by = c("mediator","draw"))
    }
    
    marginal_effect <- marginal_effect %>%
      dplyr::mutate(coeff = init * mult) %>%
      dplyr::select(independent, mediator, dependent, moderator, modval, coeff) %>%
      dplyr::group_by(independent, mediator, dependent, moderator, modval) %>%
      dplyr::summarise(estimate = mean(coeff), std.error = stats::sd(coeff)) %>% 
      dplyr::mutate( 
        lower_bound = estimate - std.error * stats::qnorm(1-p/2), 
        upper_bound = estimate + std.error * stats::qnorm(1-p/2) 
      ) %>%
      dplyr::mutate(relation = paste("relation between", independent, "and", dependent, "through", mediator, sep = " "))
  }
  
  return(marginal_effect)
}
