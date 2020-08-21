#' Compute the moderated direct paths coefficients for analayses of moderated mediations.
#' @param coefficients Tibble. Output of the function compute_coefficients.
#' @param moderations Tibble. Output of the function retrieve_values.
#' @return A tibble gathering direct and total moderated path coefficients.
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
#' @export

direct_path <- function(coefficients, moderations){
  
  stopifnot(
    tibble::is_tibble(coefficients),
    tibble::is_tibble(moderations)
  )
  
  # Bind variables
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
  
  coefficients %>%
    dplyr::left_join(moderations, by = "moderator") %>%
    dplyr::mutate(coefficient = base + moderation * modval) %>%
    dplyr::select(relation, path, dependent, independent, moderator, draw, position, coefficient) %>%
    tidyr::spread(position, coefficient) %>%
    dplyr::mutate(
      high = dplyr::case_when(
        is.na(high) ~ all,
        TRUE ~ high
      ),
      low = dplyr::case_when(
        is.na(low) ~ all,
        TRUE ~ low
      )
    ) %>%
    dplyr::mutate(diff = high - low) %>%
    dplyr::select(relation, path, dependent, independent, moderator, draw, high, low, diff)
}