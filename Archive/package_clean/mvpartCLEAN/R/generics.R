#' 
#' Generic Methods
#' 
#' A set of generic functions used by mvpart.
#' 
#' @name rpart-generics
#' 
#' @param object An object of class \code{rpart}. This is assumed to be the
#' result of some function that produces an object with the same named
#' components as that returned by the \code{rpart} function.
#' @param x Identical as argument \code{object}.
#' @param ... Optional arguments to be passed internally to other functions.
#' 
#' @return
#' \describe{
#'   \item{prune}{...}
#'   \item{rsq}{...}
#'   \item{meanvar}{...}
#' }
#' 
#' @seealso \code{\link{prune.rpart}} \code{\link{rsq.rpart}}, and
#' \code{meanvar.rpart}
#' 
NULL
#' 
#' @describeIn rpart-generics
#' 
#' Prune Method
#' 
#' Method to prune branch for a regression or classification tree
#' 
#' @export
prune <- function(object, ...)  UseMethod("prune")
#' 
#' @describeIn rpart-generics
#' 
#' R-square Method
#' 
#' Method to show the variation of the R-square with the number of splits.
#' 
#' @export
rsq <- function(x, ...) UseMethod("rsq")
#' 
#' @describeIn rpart-generics
#' 
#' Meanvar method
#' 
#' [Description of the meanvar generic here.]
#' 
#' @export
meanvar <- function(object, ...) UseMethod("meanvar")
