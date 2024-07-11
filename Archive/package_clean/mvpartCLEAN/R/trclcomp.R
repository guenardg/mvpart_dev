#' 
#' Tree-Clustering Comparison
#' 
#' This function compares the within-group variation for groups formed by tree
#' partitioning and unconstrained clustering. The results are plotted and
#' returned invisibly.
#' 
#' @name trclcomp
#' 
#' @param x Rpart object with method "mrt" -- see \code{\link{rpart}}.
#' @param method The clustering method for the unconstrained clustering.
#' @param km If \code{TRUE} a K-Means clustering is compared with the
#' multivariate tree partitioning.
#' @param mrt If \code{TRUE} an additional K-Means clustering with a starting
#' configuration based on the multivariate tree partitioning is generated.
#' 
#' @returns Returns a list (invisibly) of the within-tree and within-cluster
#' variation for all tree sizes.
#' 
#' @details The within-group variation for groups formed by multivariate tree
#' partitioning and unconstrained clusterings are compared for all sizes of the
#' hierarchy of tree partitions.
#' 
#' @references
#' De'ath G. (2002) Multivariate Regression Trees : A New Technique for
#' Constrained Classification Analysis. Ecology 83(4):1103-1117.
#' 
#' @examples
#' data(spider)
#' fit <- mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+water,
#'               spider)
#' trclcomp(fit)
#' 
#' @keywords multivariate
#' 
#' @importFrom graphics title
#' @importFrom stats cutree hclust kmeans lm resid
#' 
#' @export
trclcomp <- function (x, method = "com", km = TRUE, mrt = TRUE) {
  if(!inherits(x,"rpart"))
    stop("Rpart object needed!")
  if(x$method != "mrt")
    stop("Multivariate tree needed!")
  pruner <- function(x) {
    cps <- x$cptable[,1L]
    nps <- length(cps)
    groups <- matrix(0,nrow=length(x$where),ncol=length(cps))
    groups[,1L] <- x$where
    for (i in 1L:nps) {
      new.x <- prune.rpart(x, cp=cps[i])
      cat(length(unique(new.x$where)),"")
      groups[,i] <- new.x$where
    }
    cat("\n")
    groups
  }
  cpt <- x$cptable
  size <- cpt[,2L] + 1
  nr <- nrow(cpt)
  mrt.err <- cpt[,3L]
  mrt.clust.err <- clust.err <- rep(1, nr)
  sst <- sum(scale(x$y, scale = FALSE)^2)
  n <- nrow(x$y)
  d <- dist(x$y)
  if(any(is.na(d))) {
    cat("Warning -- NA distances in cluster -- replacing by 0\n")
    d[is.na(d)] <- 0
  }
  hclout <- hclust(d, method = method)
  grp.mrt <- pruner(x)
  for(i in 2L:nr) {
    grp.clust <- factor(cutree(hclout, k = size[i]))
    cents <- t(sapply(split(as.data.frame(x$y), grp.clust), colMeans))
    grp.clust <- factor(kmeans(x$y, centers = cents)$cluster)
    cents.mrt <- t(sapply(split(as.data.frame(x$y), grp.mrt[, i]), colMeans))
    grp.mrt.clust <- factor(kmeans(x$y, centers = cents.mrt)$cluster)
    clust.err[i] <- sum(resid(lm(x$y ~ factor(grp.clust), singular.ok = TRUE))^2)/sst
    mrt.clust.err[i] <- sum(resid(lm(x$y ~ factor(grp.mrt.clust), singular.ok = TRUE))^2)/sst
  }
  minerr <- min(c(mrt.err, mrt.clust.err, clust.err))
  plot(size, mrt.err, type = "n", ylim = c(minerr, 1), xlab = "Size",
      ylab = "Resubstition Error")
  points(size, mrt.err, type = "o", col = 2, pch = 16)
  points(size, mrt.clust.err, type = "o", col = 3, pch = 16)
  points(size, clust.err, type = "o", col = 4, pch = 16)
  legend(mean(size), 1, c("MRT","MRT-Cluster","Cluster"), col=c(2L:4L), lty=1,
         bty="n")
  title("Comparison of tree and cluster errors across size")
  cat("MRT error                : ", signif(mrt.err, 3), "\n")
  cat("MRT.Cluster error        : ", signif(mrt.clust.err, 3), "\n")
  cat("Cluster error            : ", signif(clust.err, 3), "\n")
  invisible(
    list(
      mrt.err = mrt.err,
      mrt.clust.err = mrt.clust.err,
      clust.err = clust.err
    )
  )
}
