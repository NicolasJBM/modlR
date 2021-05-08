#' Compute the moderated paths coefficients for analyses of moderated mediations.
#' @param coefficients Tibble. Output of the function compute_coefficients.
#' @param moderations Tibble. Output of the function retrieve_modval with option "allval" FALSE.
#' @param mediations List. List of vectors specifying the paths, from the first independent variable to the last dependent variable through the moderators in order.
#' @return A tibble gathering all (direct, indirect, and total) the moderated path coefficients.
#' @references Edwards, J. R., & Lambert, L. S. (2007). Methods for integrating moderation and mediation: a general analytical framework using moderated path analysis. Psychological Methods, 12(1), 1–22.
#' @importFrom tibble tibble
#' @importFrom tibble is_tibble
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr pmap
#' @importFrom purrr map
#' @importFrom stats sd
#' @importFrom stats pnorm
#' @export


analyze_path <- function(coefficients, moderations, mediations){
  
  stopifnot(
    tibble::is_tibble(coefficients),
    tibble::is_tibble(moderations),
    is.list(mediations) & length(mediations[[1]]) <= 3
  )
  
  # Bind variables
  position <- NULL
  coefficient <- NULL
  relation <- NULL
  path <- NULL
  dependent <- NULL
  independent <- NULL
  moderator <- NULL
  draw <- NULL
  count <- NULL
  estimate <- NULL
  sdev <- NULL
  p.value <- NULL
  text <- NULL
  coeff <- NULL
  
  pathana <- direct_path(coefficients, moderations)
  
  if (length(mediations[[1]]) == 3){
    for (i in 1:length(mediations)) {
      pathana <- dplyr::bind_rows(pathana, indirect_path(coefficients, moderations, mediations[[i]]))
    }
  }
  
  pathana <- pathana %>%
    tidyr::gather(position, coefficient, -relation, -path, -dependent, -independent, -moderator, -draw)
  
  totals <- pathana %>%
    dplyr::mutate(
      relation = "total",
      path = paste(independent, dependent, sep = "_")
    ) %>%
    dplyr::group_by(relation, path, dependent, independent, moderator, position, draw) %>%
    dplyr::summarise(coefficient = sum(coefficient), count = n()) %>%
    dplyr::filter(count > 1) %>%
    dplyr::select(-count)
  
  pathana <- pathana %>%
    dplyr::bind_rows(totals) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(relation, path, dependent, independent, moderator, position) %>%
    dplyr::select(-draw) %>%
    dplyr::summarise(estimate = mean(coefficient), sdev = sd(coefficient)) %>%
    dplyr::mutate(p.value = 2-2*pnorm(abs(estimate/sdev))) %>%
    dplyr::ungroup() %>%
    na.omit() %>%
    dplyr::mutate(coeff = modlR::fmt_stars(estimate, p.value)) %>%
    dplyr::filter(moderator != "base" | moderator == "base" & position == "low") %>%
    dplyr::mutate(position = dplyr::case_when(moderator == "base" ~ "all", TRUE ~ position)) %>%
    dplyr::mutate(moderator = dplyr::case_when(moderator == "base" ~ "", TRUE ~ moderator)) %>%
    dplyr::mutate(
      relation = factor(relation, levels = c("direct","indirect","total")),
      position = factor(position, levels = c("all","low","high","diff"))
    ) %>%
    dplyr::arrange(relation, path, position) %>%
    dplyr::select(relation, path, dependent, independent, moderator, position, estimate, std.error = sdev, p.value, coeff)
  
  return(pathana)
}