#'
#' Data on Children who have had Corrective Spinal Surgery
#' 
#' The \code{kyphosis} data frame has 81 rows and four columns representing data
#' on children who have had corrective spinal surgery.
#' 
#' @docType data
#' 
#' @name kyphosis
#' 
#' @usage data(kyphosis)
#' 
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{Kyphosis}}{
#'     a factor with levels
#'     \code{absent} 
#'     \code{present}
#'     indicating if a kyphosis (a type of deformation)
#'     was present after the operation.
#'   }
#'   \item{\code{Age}}{
#'     in months
#'   }
#'   \item{\code{Number}}{
#'     the number of vertebrae involved 
#'   }
#'   \item{\code{Start}}{
#'     the number of the first (topmost) vertebra operated on.
#'   }
#' }
#' 
#' @details Provided with the original package
#' 
#' @source Chambers and Hastie (1992).
#' 
#' @references
#'  John M. Chambers and Trevor J. Hastie eds. (1992) Statistical Models in S,
#'  Wadsworth and Brooks/Cole, Pacific Grove, CA 1992.
#' 
#' @examples ## Load and print the data frame:
#' data("kyphosis")
#' kyphosis
#' 
#' @keywords datasets
#' 
NULL
