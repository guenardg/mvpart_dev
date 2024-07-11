#'
#' Classical Scaling of Dissimilarity Measures
#' 
#' The function first computes the dissimilarity matrix according to the
#' specified method -- see \code{mvpart}. The dissimilarities are then scaled
#' using classical scaling -- see \code{\link[stats]{cmdscale}}. The returned
#' matrix can be input into \code{mvpart} for multivariate regression tree
#' splitting.
#' 
#' @name cmds.diss
#' 
#' @param data Data matrix.
#' @param k Number of vectors to be returned.
#' @param x.use Whether to use extended dissimilarity.
#' @param zero.chk Check for zero row sums -- if zero ignore these rows
#' according to method.
#' @param plt Whether to plot the relationship between the dissimilarities and
#' the distances calculated from the scaled output vectors.
#' @param plot.subset Plot a subset of the points -- useful for large data sets.
#' @param plot.subn Controls how many points are plotted when
#' \code{plot.subset = TRUE}. The number of points plotted is
#' \code{750 + N * plot.subn} where N = number of rows in \code{data}.
#' @param ... Arguments passed to either \code{xdiss} or \code{gdist}.
#' 
#' @details The function knows the same dissimilarity indices as
#' \code{mvpart}. Plotting the relationship between the dissimilarities and the
#' distances calculated from the scaled output vectors is useful in assessing
#' potential loss of information. If the loss is high then the results from
#' partitioning directly from the dissimilarity matrix using distance-base
#' partitioning (see \code{dist} in \code{mvpart}), and those obtained from
#' partitioning the output of \code{cmds.diss} using multivariate regression
#' trees (see \code{mrt} in \code{mvpart}) can be substantial.
#' 
#' @author Glenn De'ath
#' 
#' @examples
#' data(spider)
#' dist.vecs <- cmds.diss(spider)
#' 
#' @keywords multivariate
#' 
#' @importFrom graphics abline mtext locator
#' @importFrom stats cmdscale cor dist
#' 
#' @export
cmds.diss <- function(data, k = ncol(data), x.use = FALSE, zero.chk = TRUE,
                      plt = FALSE, plot.subset = FALSE , plot.subn = 5, ...) {
  
  if(x.use) {
    xdists <- xdiss(data, ...)
    xds <- cmdscale(xdists, k = k)
    colnames(xds) <- paste("s",1:ncol(xds),sep="")
  }
  else {
    xdists <- gdist(data, ...)
    xds <- cmdscale(xdists, k = k)
    colnames(xds) <- paste("s",1:ncol(xds),sep="")
  }
  if(zero.chk) {
    apply(
      xds, 2L,
      function(x) (all(is.nan(x)) || all(x == 0) || all(is.na(x)))
    ) -> drop.cols
    if(any(drop.cols)) {
      cat(sum(drop.cols), " columns with NAs or all zeros dropped \n")
      xds <- xds[, !drop.cols]
    }
  }
  if(plt) {
    n <- nrow(data)
    if (n < 30 || !plot.subset)
      plot(xdists, dxds <- dist(xds), xlim=c(0,max(xdists)),
           ylim=c(0,max(dxds)), xlab="Dists", ylab="CMDS Dists", pch=1L)
      else {
        samp <- sample(n*n, floor(750 + n*plot.subn))
        dxds <- dist(xds)
        plot(xdists[samp], dxds[samp], xlim=c(0,md <- max(xdists)),
             ylim=c(0, max(dxds)), xlab="Dists", ylab="CMDS Dists", pch=1L)
      }
    abline(c(0,1), col=2L, xpd=FALSE)
    mtext("Pairwise distances vs CMD scaled pairwise distances", 3L, line=1.5)
    mtext(paste("R2 =", signif(cor(xdists, dxds)^2, 4), sep=""), 3L, line=-1.5)
    locator(1L)
  }
  xds
}
