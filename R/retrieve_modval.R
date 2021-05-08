#' Retrieve values of the moderators necessary for analyses of moderated mediations.
#' @param sample  Tibble. Table containing the data.
#' @param moderators Character vector. vector speciying which variables moderate the relationships.
#' @param types  Character vector. Whether the corresponding moderator is "discrete" (actually binary) or "continuous".
#' @param allval Logical. Whether all values of the moderating variables should be retrieved (TRUE) or just two representative values.
#' @return A tibble gathering the requested values for the moderators.
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


retrieve_modval <- function(sample, moderators, types, allval){
  
  stopifnot(
    is.character(moderators),
    is.character(types),
    is.logical(allval)
  )
  
  # Bind variables
  moderator <- NULL
  type <- NULL
  modval <- NULL
  
  retrieve <- function(moderator, type, sample, allval){
    
    x <- sample %>%
      dplyr::select(moderator) %>%
      unlist()
    
    if (allval){
      
      y <- table(x) %>%
        tibble::as_tibble()
      
      names(y) <- c("modval", "freq")
      
    } else {
      
      y <- tibble::tibble(
        position = c("low","high"),
        modval = dplyr::case_when(
          type == "discrete" ~ c(min(x),max(x)),
          TRUE ~ c(-sd(x),sd(x))
        )
      )
    }
    return(y)
  }
  
  results <- tibble::tibble(
    moderator = moderators,
    type = types
  ) %>%
    dplyr::mutate(
      modval = purrr::map2(
        moderator,
        type,
        retrieve,
        sample = sample,
        allval = allval
      )
    ) %>%
    dplyr::select(-type) %>%
    tidyr::unnest(modval)
  
  if (allval){
    results <- results %>%
      dplyr::mutate(modval = as.numeric(as.character(modval)))
    dplyr::bind_rows(
      tibble::tibble(moderator = "base", value = 0, freq = nrow(sample))
    )
  } else {
    results <- results %>%
      dplyr::bind_rows(
        tibble::tibble(moderator = "base", position = "all", modval = 1)
      )
  }
  
  return(results)
}