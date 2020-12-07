#' Add stars to a vector of coefficients based on the p-value.
#' @param coeff      Numeric vector. Coefficients.
#' @param pval       Numeric vector. Corresponding p-values.
#' @param thresholds Numeric vector. three values indicating the cut-off p-values.
#' @param digits     Integer. Number of digits for rounding.
#' @return Character vector with coefficients formatted for publication.
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @export

fmt_stars <- function(coeff = NULL, pval = NULL, thresholds = c(0.01, 0.05, 0.10), digits = 3) {

  # Check entries
  stopifnot(
    !is.null(coeff),
    !is.null(pval)
  )

  # Create a visible binding
  fcoeff <- NULL
  stars <- NULL

  x <- tibble(coeff = coeff, pval = pval) %>%
    mutate(stars = "   ") %>%
    mutate(
      stars = case_when(
        pval < thresholds[[1]] ~ "***",
        pval >= thresholds[[1]] & pval < thresholds[[2]] ~ "** ",
        pval >= thresholds[[2]] & pval < thresholds[[3]] ~ "*  ",
        TRUE ~ "   "
      )
    ) %>%
    mutate(coeff = format(round(coeff, digits = digits), nsmall = digits)) %>%
    mutate(fcoeff = case_when(
      coeff >= 0 ~ paste0(" ", coeff),
      TRUE ~ paste0("", coeff)
    )) %>%
    mutate(fcoeff = paste0(fcoeff, stars))

  return(x$fcoeff)
}
