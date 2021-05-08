#' Compute the paths coefficients for analayses of moderated mediations.
#' @param est_sample  Tibble. Table containing the data.
#' @param md_imputation Character. Kind of imputation ("multiple","single","deletion")
#' @param md_method Character. Method for the imputation of missing data ("pmm", "mean", "sample", "deletion").
#' @param size Integer. Size of the sample for each draw. If NA, use the sample size.
#' @return A tibble gathering the regression coefficients for each draw.
#' @references Arne Henningsen and Jeff D. Hamann (2007). systemfit: A Package for Estimating Systems of Simultaneous Equations in R. Journal of Statistical Software 23(4), 1-40. http://www.jstatsoft.org/v23/i04/.
#' @importFrom systemfit systemfit
#' @importFrom stats na.omit
#' @importFrom dplyr sample_n
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom mice complete
#' @importFrom mice mice
#' @export


prepare_sample <- function(est_sample, md_imputation, md_method, size){
  
  stopifnot(
    md_imputation %in% c("multiple","single","deletion"),
    md_method %in% c("mean","pmm","sample","deletion"),
    md_imputation == "deletion" & md_method == "deletion" |
      md_imputation != "deletion" & md_method != "deletion"
  )
  
  if (is.na(size)) size <- nrow(est_sample) else size <- size
  
  # Apply the desired imputation and sampling method
  if (md_imputation == "multiple"){
    
    est_sample <- est_sample %>%
      dplyr::sample_n(size, replace = TRUE) %>%
      mice::mice(m = 1, meth = md_method, print=FALSE) %>%
      mice::complete()
    
  } else if (md_imputation == "single"){
    
    est_sample <- est_sample %>%
      mice::mice(m = 1, meth = md_method, print=FALSE) %>%
      mice::complete() %>%
      dplyr::sample_n(size, replace = TRUE)
    
  } else {
    
    est_sample <- na.omit(est_sample) %>%
      dplyr::sample_n(size, replace = TRUE)
    
  }
  
  rm(md_imputation, md_method, size)
  gc()
  
  return(est_sample)
}