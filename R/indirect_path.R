#' Compute the moderated indirect paths coefficients for analayses of moderated mediations (limited to one medietor).
#' @param coefficients Tibble. Output of the function compute_coefficients.
#' @param moderations Tibble. Output of the function retrieve_values.
#' @param mediation List. List of vectors specifying the paths, from the first independent variable to the last dependent variable through the moderators in order.
#' @return A tibble gathering indirect and total moderated path coefficients.
#' @seealso analyze_path
#' @importFrom tibble tibble
#' @importFrom tibble is_tibble
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr pmap
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_replace_all
#' @importFrom dplyr case_when
#' @export

indirect_path <- function(coefficients, moderations, mediation){
  
  stopifnot(
    tibble::is_tibble(coefficients),
    tibble::is_tibble(moderations),
    length(mediation) <= 3
  )
  
  modval <- NULL
  independent <- NULL
  position <- NULL
  high <- NULL
  moderator <- NULL
  coefficient <- NULL
  low <- NULL
  base <- NULL
  dependent <- NULL
  draw <- NULL
  relation <- NULL
  path <- NULL
  moderation <- NULL
  data <- NULL
  selection <- NULL
  
  tmp <- tibble::tibble(
    independent = mediation[c(1:(length(mediation)-1))],
    dependent = mediation[c(2:(length(mediation)))]
  ) %>%
    dplyr::mutate(
      selection = purrr::map2(
        independent,
        dependent,
        function(ind,dep,coeff){
          coeff %>%
            dplyr::filter(independent == ind, dependent == dep) %>%
            dplyr::select(-dependent, -independent) %>%
            dplyr::mutate(path = paste(mediation, collapse = "_"), relation = "indirect")
        },
        coeff = coefficients
      )
    ) %>%
    tidyr::unnest(selection) %>%
    dplyr::left_join(moderations, by = "moderator") %>%
    dplyr::mutate(coefficient = base + moderation * modval) %>%
    dplyr::select(-independent,-dependent, -base, -modval, -moderation)
  
  if (length(unique(tmp$moderator)) > 1){
    tmp2 <- dplyr::filter(tmp, position == "all") %>%
      dplyr::mutate(position = stringr::str_replace_all(position, "all","high"))
    tmp <- tmp %>%
      dplyr::mutate(position = stringr::str_replace_all(position, "all","low")) %>%
      dplyr::bind_rows(tmp2)
  }
  
  tmp <- tmp %>%
    dplyr::group_by(path, draw, relation, position) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, function(x) {
      mod <- unique(setdiff(x$moderator, "base"))
      if (length(mod) < 1) mod <- "base"
      dplyr::mutate(x, moderator = dplyr::case_when(moderator == "base" ~ mod, TRUE ~ moderator))
    })) %>%
    tidyr::unnest(data) %>%
    dplyr::group_by(path, draw, moderator, relation, position)  %>%
    dplyr::summarise(coefficient = prod(coefficient)) %>%
    dplyr::ungroup()
  
  if ("high" %in% unique(tmp$position) & "low" %in% unique(tmp$position)){
    tmp <- tmp %>%
      tidyr::spread(position, coefficient) %>%
      dplyr::mutate(diff = high - low) %>%
      dplyr::mutate(dependent = mediation[c(length(mediation))], independent = mediation[1]) %>%
      dplyr::select(relation, path, dependent, independent, moderator, draw, high, low, diff) %>%
      na.omit()
  } else {
    tmp <- tmp %>%
      tidyr::spread(position, coefficient) %>%
      dplyr::rename(low = all) %>%
      dplyr::mutate(high = low) %>%
      dplyr::mutate(diff = high - low) %>%
      dplyr::mutate(dependent = mediation[c(length(mediation))], independent = mediation[1]) %>%
      dplyr::select(relation, path, dependent, independent, moderator, draw, high, low, diff) %>%
      na.omit()
  }
  
  return(tmp)
}