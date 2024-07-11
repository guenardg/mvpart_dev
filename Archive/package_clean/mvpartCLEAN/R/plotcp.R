#'
#' Plot a Complexity Parameter Table for an Rpart Fit
#' 
#' Gives a visual representation of the cross-validation results in an
#' \code{rpart} object.
#' 
#' @name plotcp
#' 
#' @param x An object of class \code{rpart}.
#' @param xvse Multiplier for xvse * SE above the minimum of the curve.
#' @param minline Whether a horizontal line is drawn 1SE above the minimum of
#' the curve.
#' @param lty Type of lines used in plot, default = 3 (a dotted line).
#' @param col Color of lines, default = 1 (black).
#' @param upper What is plotted on the top axis: the size of the tree (the
#' number of leaves), the number of splits or nothing.
#' @param tab Used for multiple cross-validation.
#' @param resub.err Uses the re-substitution error for the calculation of SEs.
#' @param adj.df Adjusts the degrees of freedom of the re-substitution error
#' estimate in the calculation of SEs.
#' @param ... Additional graphical parameters
#' 
#' @returns \code{NULL} (invisibly).
#' 
#' @details The set of possible cost-complexity prunings of a tree from a nested
#' set. For the geometric means of the intervals of values of \code{cp} for
#' which a pruning is optimal, a cross-validation has (usually) been done in
#' the initial construction by \code{rpart}. The \code{cptable} in the fit
#' contains the mean and standard deviation of the errors in the cross-validated
#' prediction against each of the geometric means, and these are plotted by this
#' function. A good choice of \code{cp} for pruning is often the leftmost value
#' for which the mean lies below the horizontal line.
#'
#' @note A plot is produced on the current graphical device.
#' 
#' @seealso \code{rpart}, \code{printcp}, and \code{rpart.object}
#' 
#' @keywords tree
#' 
#' @importFrom graphics axis box par points segments text
#' 
#' @export
plotcp <- function(x, xvse = 1, minline = TRUE , lty = 3, col = 1,
                   upper = c("size", "splits", "none"), tab, resub.err = TRUE ,
                   adj.df = FALSE , ...) {
  if(!inherits(x, "rpart"))
    stop("Not legitimate rpart object")
  upper <- match.arg(upper)
  p.rpart <- x$cptable
  if(xv <- (ncol(p.rpart) > 3L)) {
    xstd <- p.rpart[,5L]
    xerror <- p.rpart[,4L]
  }
  error <- p.rpart[,3L]
  nsplit <- p.rpart[,2L]
  ns <- seq(along = nsplit)
  cp0 <- p.rpart[,1L]
  cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
  if (xv) {
    ylo <- min(c(xerror - xstd, error)) - 0.05
    yhi <- max(c(xerror + xstd, error)) + 0.05
  } else {
    ylo <- min(error) - 0.05
    yhi <- max(error) + 0.05
  }
  ylim <- c(ylo, yhi)
  plot(ns, error, axes=FALSE , xlab="cp", ylab="X-val Relative Error",
       ylim=ylim, type="n", ...)
  if(xv) {
    inpt <- (xerror == min(xerror))
    points(ns[inpt], xerror[inpt], col = "red", pch = 16L, cex = 2)
    inpt <- min(ns[xerror < min(xerror + xvse * xstd)])
    points(ns[inpt], xerror[inpt], col = "orange", pch = 16L, cex = 2)
    points(ns, xerror, type = "b", col = "blue", ...)
    segments(ns, xerror - xstd, ns, xerror + xstd, col = "blue", ...)
  }
  if(resub.err)
    points(ns, error, type="b", lty=1L, col = "darkgreen", ...)
  box()
  axis(2L, ...)
  axis(1L, at=ns, labels=as.character(signif(cp, 2)), ...)
  if (!missing(tab)) {
    xp <- as.numeric(names(tab))
    segments(ns[match(xp, nsplit + 1)], yhi, ns[match(xp,nsplit + 1)],
             yhi - 0.5*(tab/sum(tab))*(yhi - ylo), col=col + 1, lwd=2L, ...)
  }
  switch(
    upper,
    size = {
      axis(3L, at=ns, labels=as.character(nsplit + 1), ...)
      mtext("Size of tree", side=3L, line=3, cex=par()$cex, ...)
    },
    splits = {
      axis(3L, at=ns, labels=as.character(nsplit), ...)
      mtext("Number of splits", side=3L, line=3, ...)
    },
  )
  if(xv) {
    minpos <- min(seq(along = xerror)[xerror == min(xerror)])
    if(minline) {
      abline(h = (xerror + xvse * xstd)[minpos], lty=1L, col=col, xpd=FALSE)
      text(ns[2], (xerror + 0.5*xvse*xstd)[minpos],
           paste("Min +", xvse, "SE"), col=col, ...)
    }
  }
  invisible()
}
