#' 
#' Recursive Partitioning and Regression Trees
#' 
#' Wrapper function for fitting and plotting \code{rpart} models.
#' 
#' @param form As for \code{rpart} function.
#' @param data Optional data frame in which to interpret the variables named in
#' the formula
#' @param minauto If \code{TRUE} uses smart minsplit and minbucket based on N
#' cases.
#' @param size The size of tree to be generated.
#' @param xv Selection of tree by cross-validation: \code{"1se"} - gives best
#' tree within one SE of the overall best, \code{"min"} - the best tree,
#' \code{"pick"} - pick the tree size interactively, \code{"none"} - no
#' cross-validation.
#' @param xval Number of cross-validations or vector defining cross-validation
#' groups.
#' @param xvmult Number of multiple cross-validations.
#' @param xvse Multiplier for the number of SEs used for \code{xv = "1se"}.
#' @param plot.add Plot the tree and (optionally) add text.
#' @param text.add Add output of \code{text.rpart} to tree.
#' @param snip Interactively prune the tree.
#' @param digits Number of digits on labels.
#' @param margin Margin around plot, 0.1 gives an extra 10 percent space around
#' the plot.
#' @param uniform Uniform lengths to the branches of the tree.
#' @param which Which split labels and where to plot them, 1=centered, 2 = left,
#' 3 = right and 4 = both.
#' @param pretty Pretty labels or full labels.
#' @param use.n Add number of cases at each node.
#' @param all.leaves Annotate all nodes.
#' @param bars If \code{TRUE} adds barplots to nodes.
#' @param legend If \code{TRUE} adds legend for mrt and classification trees.
#' @param bord Border (box) around the barplots.
#' @param xadj Adjust the horizontal size of the individual barplots
#' (default = 1).
#' @param yadj Adjust the vertical size of the individual barplots
#' (default = 1).
#' @param prn If \code{TRUE} prints tree details.
#' @param branch Controls spread of branches: 1=vertical lines, 0=maximum slope.
#' @param rsq If \code{TRUE} gives "rsq" plot.
#' @param big.pts Plot colored points at leaves -- useful to link to PCA plot.
#' @param pca If \code{TRUE} plots PCA of group means and add species and site
#' information.
#' @param interact.pca If \code{TRUE} runs interactive PCA. See
#' \code{rpart.pca}.
#' @param wgt.ave.pca If \code{TRUE} plot weighted averages across sites for
#' species.
#' @param keep.y If \code{TRUE} y values are returned.
#' @param ... Other arguments passed to \code{rpart}.
#' 
#' @returns An \code{\link{rpart-class}} object, which contain a superset of
#' regression or classification trees.
#' 
#' @seealso \code{\link{rpart}}, \code{rpart.pca}
#' 
#' @keywords tree
#' 
#' @examples
#' data(spider)
#' mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+
#'          water,spider)       # defaults
#' mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+
#'          water,spider,xv="p")  # pick the tree size
#' # pick cv size and do PCA
#' fit <- mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+
#'                 twigs+water,spider,xv="1se",pca=TRUE)
#' rpart.pca(fit,interact=TRUE,wgt.ave=TRUE)
#' # interactive PCA plot of saved multivariate tree
#' 
#' @export
mvpart <- function(form, data, minauto = TRUE, size,
                   xv = c("1se","min","pick","none"), xval = 10, xvmult = 0,
                   xvse = 1, snip = FALSE, plot.add = TRUE, text.add = TRUE,
                   digits = 3, margin = 0, uniform = FALSE, which = 4,
                   pretty = TRUE, use.n = TRUE, all.leaves = FALSE, bars = TRUE,
                   legend, bord = FALSE, xadj = 1, yadj = 1, prn = FALSE,
                   branch = 1, rsq = FALSE, big.pts = FALSE, pca = FALSE,
                   interact.pca = FALSE, wgt.ave.pca = FALSE, keep.y = TRUE,
                   ...) {
  call <- match.call()
  number <- function(x)
    match(x, sort(unique(x)))
  cv.var <- function(x, cv = 10) {
    x <- match(x, sort(unique(x)))
    luni <- length(unique(x))
    if(luni >= cv) {
      grps <- ceiling((cv * cumsum(table(x)))/length(x))
      x <- number(grps[x])
    }
    x
  }
  if(length(xval) > 1L) {
    if(xvmult > 1) 
      xvalvar <- xval
    xval <- cv.var(xval)
  }
  choice <- c("1se","min","pick","none")
  xv <- choice[pmatch(xv[1], choice)]
  if(!missing(size) || xv == "none") 
    xval <- 0
  if(minauto) {
    n <- nrow(data)
    minsplit <- ceiling(log2(n))
    minbucket <- ceiling(minsplit/3)
  }
  z <- rpart(form, data = data, ...)
  if(all(z$where==1)) {
    cat("No splits possible -- try decreasing cp\n")
    return(z)
  }
  old.par <- par(mar = c(6, 4, 4, 4) + 0.1, xpd = NA, cex = par()$cex)
  on.exit(par(old.par))
  if(!is.null(z)) {
    xval <- z$control$xval
    if(xvmult > 1) {
      zresse <- zres <- matrix(NA, nrow=nrow(z$cptable), ncol=xvmult)
      zres[,1L] <- z$cptable[,4L]
      zresse[,1L] <- z$cptable[,5L]
      cat("X-Val rep : 1")
      for(i in 2:xvmult) {
        if(length(xval) == nrow(data))
          xval <- cv.var(xvalvar)
        ztemp <- rpart(form, data=data, ...)$cptable[,4L:5L]
        zres[,i] <- ztemp[,1L]
        zresse[,i] <- ztemp[,2L]
        cat(" ", i)
        NULL
      }
      cat("\n")
      z$cptable[,4L] <- apply(zres, 1L, mean)
      z$cptable[,5L] <- apply(zresse, 1L, mean)
      tabmins <- apply(
        zres, 2L,
        function(x, nc, sizes) {
          sizes[x == min(x)][1L]
        },
         nc = nrow(zres), sizes = z$cptable[, 2] + 1
      )
      cat("Minimum tree sizes\n")
      print(table(tabmins))
    }
    if(missing(size)) {
      if(xv == "pick") {
        if(xvmult <= 1) {
          plotcp(z, xvse, pch = 16L, col = 2L)
        } else plotcp(z, xvse, pch = 16L, col = 2L, tab = table(tabmins))
        size.loc <- locator(1L)
        if(!is.null(size.loc)) {
          splt <- round(size.loc$x)
          if(splt < 2) {
            splt <- 2
          } else {
            if(splt > length(z$cptable[,1L]))
              splt <- length(z$cptable[, 1])
          }
          cpp <- z$cptable[,1L][splt]
          z <- prune.rpart(z, cp = cpp)
        }
      } else if((xv == "1se" | xv == "min") && (xval[1] != 0)) {
        xerror <- z$cptable[,4L]
        xstd <- z$cptable[,5L]
        if(xv == "min") {
          splt <- min(seq(along = xerror)[xerror == min(xerror)])
        } else
          splt <- min(seq(along = xerror)[xerror <= min(xerror) + xvse*xstd])
          if(!is.na(splt)) {
            if(splt == 1L)
              splt <- 2L
            cpp <- z$cptable[,1L][splt]
            z <- prune.rpart(z, cp = cpp)
          } else {
            (cat("No pruning possible : size 2 tree produced ?? \n"))
            use.size <- TRUE
            size <- 2L
          }
        }
      } else {
        if(size <= 2L) {
          cpp <- z$cptable[2L,1L]
        } else if(size >= max(z$cptable[,2L] + 1)) {
          cpp <- z$cptable[dim(z$cptable)[1L],1L]
        } else {
          sel <- min(abs(size - z$cptable[,2] - 1)) ==
            abs(size - z$cptable[, 2] - 1)
          cpp <- z$cptable[,1L][sel][1L]
        }
        z <- prune.rpart(z, cp = cpp)
      }
    if(snip) {
      plot(z)
      z <- snip.rpart(z)
    }
    if(rsq && xval != 0 && z$method != "class") {
      par(mfrow = c(1L,2L))
      rsq.rpart(z)
      locator(1L)
      par(mfrow = c(1L,1L))
    }
    if(plot.add) {
      plot.rpart(z, uniform = uniform, branch = branch, margin = margin)
      if(text.add)
        text.rpart(
          z, digits = digits, xadj = xadj, yadj = yadj, 
          which = which, pretty = pretty, use.n = use.n, bars = bars,
          legend = ifelse(
            missing(legend),
            (z$method == "mrt") && (ncol(z$frame$yval2) < 20),
            legend
          ),
          all.leaves = all.leaves, bord = bord, big.pts = big.pts | pca
        )
      len <- dim(z$cptable)[1L]
      foot <- paste("Error : ", signif(z$cptable[len, 3], digits))
      if(xval[1] > 0)
        foot <- paste(foot, "  CV Error : ", signif(z$cptable[len,4L], digits),
                      "  SE : ", signif(z$cptable[len,5L], digits))
      mtext(foot, side = 1L, line = 3.5, cex = par()$cex)
      n <- dim(data)[1L]
      if(z$method == "class") {
        nex <- max(table(z$y))
        foot2 <- paste(
          "Missclass rates : Null = ", signif(1 - nex/n, digits),
          " : Model = ", signif((1 - nex/n) * z$cptable[len, 3], digits)
        )
        if(xval[1] > 0) 
          foot2 <- paste(
            foot2, " : CV = ", signif((1 - nex/n)*z$cptable[len, 4], digits)
          )
        mtext(foot2, side = 1, line = 4.5, cex = par()$cex)
      }
    }
    if(prn) 
      printcp(z)
    if(pca) {
      locator(1L)
      rpart.pca(z, interact = interact.pca, wgt.ave = wgt.ave.pca)
    }
  } else {
    plot(c(-1, 1), c(-1, 1), axes = FALSE, type = "n", xlab = "", ylab = "")
    text(c(0), c(0), "No splits could be formed", col = 2)
    cat("No splits could be formed\n")
  }
  if(!is.null(z)) {
    if (!keep.y)
      z$y <- NULL
    z$call <- call
    invisible(z)
  }
}
