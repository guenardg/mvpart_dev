#'
#' Row and Column Scaling of a Data Matrix
#' 
#' The function provides some popular (and effective) standardization methods
#' for community ecologists.
#' 
#' @name scaler
#' 
#' @param x Data matrix.
#' @param col Character vector of column standardization.
#' @param row Character vector of row standardizations.
#' 
#' @details The function offers following data matrix standardizations:
#' \itemize{
#'   \item \code{mean1 }: scale to mean of 1.
#'   \item \code{max1 }: scale to maximum of 1.
#'   \item \code{ssq1 }: scale to sums of sqaures equal 1.
#'   \item \code{range01 }: scale range to 0-1.
#'   \item \code{zsc }: standardize to z-scores (mean=0, sd=1).
#'   \item \code{pa }: scale to presence/absence scale (0/1).
#'   \item \code{rank }: scale to rank order (1=lowest).
#' }
#' 
#' Standardizations are performed first on columns then on rows. "pa" applies to
#' the whole matrix and can be specied using row or col.
#' 
#' @returns Returns the standardized matrix.
#' 
#' @author Jari Oksanen -- modified Glenn De'ath (Dec 03)
#' 
#' @note Common transformations can be made with standard \R functions.
#' 
#' @examples
#' data(spider)
#' spid.data <- scaler(spider, col = "max", row="mean1")
#' 
#' @keywords multivariate manip
#' 
#' @export
scaler <- function(
    x, col = c("mean1","max1","min0","ssq1","range01","zsc","pa","rank")[2L],
    row = c("mean1","max1","min0","ssq1","range01","zsc","pa","rank")[1L]) {
  fun <- function(x, method, MARGIN) {
    x <- as.matrix(x)
    switch(
      method,
      mean1 = {
        tmp <- apply(x, MARGIN, mean, na.rm = TRUE)
        x <- sweep(x, MARGIN, tmp, "/")
      }, max1 = {
        tmp <- apply(x, MARGIN, max, na.rm = TRUE)
        x <- sweep(x, MARGIN, tmp, "/")
      }, min0 = {
        tmp <- apply(x, MARGIN, min, na.rm = TRUE)
        x <- sweep(x, MARGIN, tmp, "-")
      }, ssq1 = {
        tmp <- apply(x^2, MARGIN, sum, na.rm = TRUE)
        tmp <- sqrt(tmp)
        x <- sweep(x, MARGIN, tmp, "/")
      }, range01 = {
        tmp <- apply(x, MARGIN, min, na.rm = TRUE)
        ran <- apply(x, MARGIN, max, na.rm = TRUE)
        ran <- ran - tmp
        x <- sweep(x, MARGIN, tmp, "-")
        x <- sweep(x, MARGIN, ran, "/")
      }, zsc = {
        if(MARGIN == 1L) x <- t(scale(t(x))) else x <- scale(x)
      }, pa = {
        tmp <- dim(x)
        nam <- dimnames(x)
        x <- as.numeric(x > 0)
        dim(x) <- tmp
        dimnames(x) <- nam
      }, rank = {
        x <- apply(x, MARGIN, rank)
        if (MARGIN == 1) 
          x <- t(x)
      }
    )
    x
  }
  METHODS <- c("mean1","max1","min0","ssq1","range01","zsc","pa","rank")
  if (is.null(col) & is.null(row)) 
    cat("Scalings are",METHODS,"\n")
  if(!is.null(col)) {
    for (i in 1:length(col)){
      method <- match.arg(col[i], METHODS)
      x <- fun(x, method = method, MARGIN = 2L)
    }
  }
  if(!is.null(row)) {
    for(i in 1:length(row)) {
      method <- match.arg(row[i], METHODS)
      x <- fun(x, method = method, MARGIN = 1L)
    }
  }
  x
}
