#' Create a formula based on lists of variables. All independent variables will be interacted with the moderator if the latter is specified. Controls are not.
#' @param dependent    Character string. Dependent variable.
#' @param independent  Character string. Variables of interest.
#' @param moderator    Character vector. Moderating variable.
#' @param control      Character vector. Control variables.
#' @param fixed_effect Character vector. Fixed effects.
#' @param instrument   Character vector. Instruments.
#' @param cluster      Character vector. Clusterings for errors.
#' @return A formula to run in regressions. Interactions are introduced under the form "independent_x_moderator" and therefore the corresponding variable must be created in the dataset.
#' @examples
#' library(modlR)
#' model <- prep_model(dependent = "turnover",
#'                     independent = c("distributive", "procedural"),
#'                     moderator = "pay",
#'                     control = c("age","gender"),
#'                     fixed_effect = c("team","period"),
#'                     instrument = c("chock","manip"),
#'                     cluster = c("country"))
#' model
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @export


prep_model <- function(dependent = NULL,
                       independent = c(NA),
                       moderator = c(NA),
                       control = c(NA),
                       fixed_effect = c(NA),
                       instrument = c(NA),
                       cluster = c(NA)) {

  # Check entries
  stopifnot(
    !is.null(dependent),
    length(na.omit(independent)) > 0
  )
  
  # Create visibile binds
  moderation <- NULL
  
  # Interact all independent variables with the moderator.
  mod_indep <- tibble::tibble(
    independent = unlist(independent),
    moderator = unlist(moderator)
  ) %>%
    dplyr::mutate(
      moderation = dplyr::case_when(
        !is.na(independent) & !is.na(moderator) ~ paste0(independent, "_x_", moderator, sep = ""),
        TRUE ~ "NA"
      )
    ) %>%
    dplyr::mutate(moderation = gsub("NA", NA, moderation))
  
  # Create the equation based on the list of variables.
  interest <- na.omit(unlist(mod_indep$independent))
  interact <- na.omit(unlist(mod_indep$moderation))
  controls <- na.omit(unique(c(moderator, unlist(control))))
  fixeff <- na.omit(unlist(fixed_effect))
  instr <- na.omit(unlist(instrument))
  clust <- na.omit(unlist(cluster))
  
  if (length(fixeff) > 0 | length(instr) > 0 | length(clust)){
    if (length(instr) > 0) {
      first_part <- paste(c(interact, controls), collapse = " + ")
      third_part <- paste0("(", paste(paste(interest, collapse = " | "), " ~ ", paste(instr, collapse = " + ")), ")")
    } else {
      first_part <- paste(c(interest, interact, controls), collapse = " + ")
      third_part <- "0"
    }
    if (length(fixeff) > 0) second_part <- paste(fixeff, collapse = " + ") else second_part <- "0"
    if (length(clust) > 0) fourth_part <- paste(clust, collapse = " + ") else fourth_part <- "0"
    formula <- paste0(dependent, " ~ ", paste(na.omit(c(first_part, second_part, third_part, fourth_part)), collapse = " | "))
    method = "FELM"
  } else {
    formula = paste0(dependent, " ~ ", paste(c(interest, interact, controls), collapse = " + "))
    method = "SYSFIT"
  }
  
  model <- list(
    formula = as.formula(formula),
    method = method
  )
  
  return(model)
}
