#' Data from Edwards and Lambert 2007 for tests of moderated mediation.
#'
#' A dataset to test the impact of feedback on commitment through satisfaction, moderated by gender or centrality.
#' 
#' @format A tiblle with 1,307 observations and 17 variables:
#' \itemize{
#'   \item gen. Gender (raw)
#'   \item cen. Centrality (raw)
#'   \item fbk. Feedback (raw)
#'   \item sat. Satisfaction (raw)
#'   \item com. Commitment (raw)
#'   \item cenc. Centrality (scaled)
#'   \item fbkc. Feedback (scaled)
#'   \item satc. Satisfaction (scaled)
#'   \item comc. Commitment (scaled)
#'   \item fbkgen. Interaction between Feedback and Gender
#'   \item satgen. Interaction between Satisfaction and Gender
#'   \item fbkcen. Interaction between Feedback and Centrality
#'   \item satcen. Interaction between Satisfaction and Centrality
#'   \item fbkcgen. Interaction between Feedback and Gender
#'   \item satcgen. Interaction between Satisfaction and Gender
#'   \item fbkccenc. Interaction between Feedback and Centrality
#'   \item satccenc. Interaction between and Centrality
#' }
#' @source \url{http//:dx.doi.org/10.1037/1082-989X.12.1.1.supp}
#' @docType data
#' @keywords datasets
#' @name mm_edwards2007
#' @usage data("mm_edwards2007")
#' @references {Edwards, J. R., & Lambert, L. S. (2007). Methods for integrating moderation and mediation: a general analytical framework using moderated path analysis. Psychological Methods, 12(1), 1–22.}
"mm_edwards2007"