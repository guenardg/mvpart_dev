#'
#' Handles Missing Values in an Rpart Object
#' 
#' Handles missing values in an \code{rpart} object.
#' 
#' @name na.rpart
#' 
#' @param x A model frame.
#' 
#' @details Internal function that handles missing values when calling the
#' function \code{rpart}.
#' 
#' @keywords tree
#' 
#' @export
na.rpart <- function(x) {
  Terms <- attr(x, 'terms')
  if(!is.null(Terms)) yvar <- attr(Terms, "response") else yvar <- 0
  if(yvar == 0) {
    xmiss <- is.na(x)
    keep <-  (xmiss %*% rep(1,ncol(xmiss))) < ncol(xmiss)
  } else {
    xmiss <- is.na(x[-yvar])
    ymiss <- is.na(x[[yvar]])
    if(is.matrix(ymiss))
      keep <- ((xmiss %*% rep(1,ncol(xmiss))) < ncol(xmiss)) &
      ((ymiss %*% rep(1,ncol(ymiss))) == 0)
    else
      keep <- ((xmiss %*% rep(1,ncol(xmiss))) < ncol(xmiss)) & !ymiss
  }
  if(all(keep)) x else {
    temp <- seq(keep)[!keep]
    names(temp) <- row.names(x)[!keep]
    #the methods for this group are all the same as for na.omit
    class(temp) <- c("na.rpart", "omit")
    structure(x[keep,], na.action=temp)
  }
}
