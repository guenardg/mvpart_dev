#'
#' Recursive Partitioning and Regression Trees
#' 
#' Fit a \code{rpart} model.
#' 
#' @name rpart
#' @aliases rpartcallback
#' 
#' @param formula A formula, as in the \code{lm} function.
#' @param data An optional data frame in which to interpret the variables named
#' in the formula.
#' @param weights Optional case weights.
#' @param subset Optional expression saying that only a subset of the rows of
#' the data should be used in the fit.
#' @param na.action The default action deletes all observations for which
#' \code{y} is missing, but keeps those in which one or more predictors are
#' missing.
#' @param method One of \code{"anova"}, \code{"poisson"}, \code{"class"},
#' \code{"mrt"}, \code{"dist"}, or \code{"exp"}. If \code{method} is missing
#' then the routine tries to make an intellegent guess. If \code{y} is a
#' survival object, then \code{method="exp"} is assumed, if \code{y} is a matrix
#' then \code{method="mrt"} is assumed, if \code{y} is a factor then
#' \code{method="class"} is assumed, otherwise \code{method="anova"}.is assumed.
#' It is wisest to specifiy the method directly, especially as more criteria are
#' added to the function. For method=\code{"dist"} the response must be a square
#' symmetric distance matrix; e.g. returned by \code{\link[mvpart]{gdist}} or
#' \code{\link[mvpart]{xdiss}}. Weights and cross-validation are currently not
#' implemented for method=\code{"dist"}. Alternatively, \code{method} can be a
#' list of functions named \code{init}, \code{split} and \code{eval}.
#' @param dissim Used when method=\code{"anova"} or method=\code{"mrt"}.
#' Dissimilarity types are either \code{"euc"} for Euclidean (sums of squares
#' about the mean) or \code{"man"} for Manhattan (sums of absolute deviations
#' about the mean. The latter is experimental and has proved useful for
#' ecological data.
#' @param model Keep a copy of the model frame in the result. If the input value
#' for \code{model} is a model frame (likely from an earlier call to the
#' \code{rpart} function), then this frame is used rather than constructing new
#' data.
#' @param x Keep a copy of the \code{x} matrix in the result.
#' @param y Keep a copy of the dependent variable in the result.
#' @param parms Optional parameters for the splitting function. Anova splitting
#' has no parameters. Poisson splitting has a single parameter, the coefficient
#' of variation of the prior distribution on the rates. The default value is 1.
#' Exponential splitting has the same parameter as Poisson. For classification
#' splitting, the list can contain any of: the vector of prior probabilities
#' (component \code{prior}), the loss matrix (component \code{loss}) or the
#' splitting index (component \code{split}). The priors must be positive and sum
#' to 1.  The loss matrix must have zeros on the diagonal and positive
#' off-diagonal elements. The splitting index can be \code{gini} or
#' \code{information}. The default priors are proportional to the data counts,
#' the losses default to 1, and the split defaults to \code{gini}.
#' @param control Options that control details of the \code{rpart} algorithm.
#' @param cost A vector of non-negative costs, one for each variable in the
#' model. Defaults to one for all variables.  These are scalings to be
#' applied when considering splits, so the improvement on splitting on a
#' variable is divided by its cost in deciding which split to choose.
#' @param ... Arguments to \code{rpart.control} may also be specified in the
#' call to \code{rpart}.  They are checked against the list of valid arguments.
#' 
#' @returns An object of class \code{rpart}; a superset of a classification
#' tree.
#' 
#' @details This differs from the tree function mainly in its handling of
#' surrogate variables.  In most details it follows Breiman et. al. quite
#' closely.
#' 
#' @references
#' Breiman, Friedman, Olshen, and Stone. (1984) Classification and Regression
#' Trees. Wadsworth.
#' 
#' De'ath G. (2002) Multivariate Regression Trees : A New Technique for
#' Constrained Classification Analysis. Ecology 83(4): 1103-1117.
#' 
#' @seealso \code{rpart.control}, \code{rpart.object}, \code{summary.rpart},
#' \code{print.rpart}
#' 
#' @examples
#' data(car.test.frame)
#' z.auto <- rpart(Mileage ~ Weight, car.test.frame)
## summary(z.auto)
## plot(z.auto); text(z.auto)
## 
#' data(spider)
#' fit1 <- rpart(data.matrix(spider[,1:12])~water+twigs+reft+herbs+
#'                 moss+sand,spider,method="mrt")
## plot(fit1); text(fit1)
## 
#' fit2 <- rpart(data.matrix(spider[,1:12])~water+twigs+reft+herbs+
#'                 moss+sand,spider,method="mrt",dissim="man")
## plot(fit2); text(fit2)
## 
#' fit3 <- rpart(gdist(spider[,1:12],meth="bray",full=TRUE,sq=TRUE)
#'               ~water+twigs+reft+herbs+moss+sand,spider,method="dist")
## plot(fit3); text(fit3)
#' 
#' @keywords multivariate
#' 
#' @importFrom stats model.extract
#' 
#' @useDynLib mvpartCLEAN, .registration = TRUE
#' 
#' @export
rpart <- function(formula, data = NULL, weights, subset, na.action = na.rpart,
                  method, dissim, model = FALSE, x = FALSE, y = TRUE, parms,
                  control, cost, ...) {
  call <- match.call()
  if(is.data.frame(model)) {
    m <- model
    model <- FALSE
  } else {
    m <- match.call(expand.dots = FALSE)
    m$model <- m$method <- m$control <- NULL
    m$x <- m$y <- m$parms <- m$... <- NULL
    m$cost <- m$dissim <- NULL
    m$na.action <- na.action
    m[[1]] <- as.name("model.frame.default")
    m <- eval(m, parent.frame())
  }
  Terms <- attr(m, "terms")
  if(any(attr(Terms, "order") > 1))
    stop("Trees cannot handle interaction terms")
  Y <- model.extract(m, "response")
  wt <- model.extract(m, "weights")
  if(length(wt) == 0L)
    wt <- rep(1, nrow(m))
  offset <- attr(Terms, "offset")
  X <- rpart.matrix(m)
  nobs <- nrow(X)
  nvar <- ncol(X)
  if(missing(method)) {
    if(is.factor(Y) || is.character(Y)) {
      method <- "class"
    } else if(inherits(Y, "Surv")) {
      method <- "exp"
    } else if(is.matrix(Y)) {
      method <- "mrt"
    } else method <- "anova"
  }
  if(is.list(method)) {
    mlist <- method
    method <- "user"
    if(missing(parms)) {
      init <- mlist$init(Y, offset, wt = wt)
    } else init <- mlist$init(Y, offset, parms, wt)
    method.int <- 6L
    keep <- rpartcallback(mlist, nobs, init)
    ### added 15/04/13
    parms <- init$parms
  } else {
    method.int <- pmatch(method, c("anova","mrt","poisson","class","dist","exp"))
    if(is.na(method.int))
      stop("Invalid method")
    method <- c("anova","mrt","poisson","class","dist","exp")[method.int]
    if(method.int == 6L)
      method.int <- 3L
    if(missing(parms)) {
      init <- (get(paste("rpart", method, sep = ".")))(Y, offset, , wt)
    } else
      init <- (get(paste("rpart", method, sep = ".")))(Y, offset, parms, wt)
  }
  if(missing(dissim) || is.null(dissim))  dissim <- "euclidean"
  dissim.int <- pmatch(dissim, c("euclidean", "manhattan"))
  if(is.na(dissim.int))
    stop("Invalid dissimilarity")
  dissim <- c("euclidean", "manhattan")[dissim.int]
  Y <- init$y
  if(method == "dist") {
    Y <- Y[row(Y) > col(Y)]
    init$y <- init$y[row(init$y) > col(init$y)]
  }
  xlevels <- attr(X, "column.levels")
  cats <- rep(0, ncol(X))
  if(!is.null(xlevels)) {
    cats[match(names(xlevels), dimnames(X)[[2]])] <- unlist(lapply(xlevels, length))
  }
  extraArgs <- list(...)
  if(length(extraArgs)) {
    controlargs <- names(formals(rpart.control))
    indx <- match(names(extraArgs), controlargs, nomatch = 0)
    if(any(indx == 0))
      stop(paste("Argument", names(extraArgs)[indx == 0], "not matched"))
  }
  controls <- rpart.control(...)
  if (!missing(control))
    controls[names(control)] <- control
  xval <- controls$xval
  if(is.null(xval) || (length(xval) == 1 && xval == 0) ||
       method == "user" || method == "dist") {
    xgroups <- 0
    #   Set xval to 0 for dist splitting and reset controls$xval -- GD 12/03
    xval <- 0
    controls$xval <- xval
  } else if(length(xval) == 1L) {
    xgroups <- sample(rep(1L:xval, length = nobs), nobs, replace = FALSE)
  } else if(length(xval) == nobs) {
    xgroups <- xval
    xval <- length(unique(xgroups))
  } else {
    if (!is.null(attr(m, "na.action"))) {
      temp <- as.integer(attr(m, "na.action"))
      xval <- xval[-temp]
      if (length(xval) == nobs) {
        xgroups <- xval
        xval <- length(unique(xgroups))
      } else stop("Wrong length for xval")
    } else stop("Wrong length for xval")
  }
  if(missing(cost)) {
    cost <- rep(1, nvar)
  } else {
    if(length(cost) != nvar)
      stop("Cost vector is the wrong length")
    if(any(cost <= 0))
      stop("Cost vector must be positive")
  }
  tfun <- function(x)
    if(is.matrix(x)) rep(is.ordered(x), ncol(x)) else is.ordered(x)
  isord <- unlist(lapply(m[attr(Terms, "term.labels")], tfun))
  .C("s_to_rp",
     n = as.integer(nobs),
     nvarx = as.integer(nvar),
     ncat = as.integer(cats * (!isord)),
     method = as.integer(method.int),
     as.double(unlist(controls)),
     parms = as.double(unlist(init$parms)),
     as.integer(xval),
     as.integer(xgroups),
     as.double(t(init$y)),
     as.double(X),
     as.integer(dissim.int),
     as.integer(!is.finite(X)),
     error = character(1),
     wt = as.double(wt),
     as.integer(init$numy),
     as.double(cost),
     NAOK = TRUE,
     PACKAGE = "mvpartCLEAN") -> rpfit
  if(rpfit$n == -1)
    stop(rpfit$error)
  nodes <- rpfit$n
  nsplit <- rpfit$nvarx
  numcp <- rpfit$method
  ncat <- rpfit$ncat[1]
  numresp <- init$numresp
  if(nsplit == 0) xval <- 0
  cpcol <- if (xval > 0 && nsplit > 0) 5 else 3
  if(ncat == 0) {
    catmat <- 0
  } else catmat <- matrix(integer(1), ncat, max(cats))
  .C("s_to_rp2",
     as.integer(nobs),
     as.integer(nsplit),
     as.integer(nodes),
     as.integer(ncat),
     as.integer(cats*(!isord)),
     as.integer(max(cats)),
     as.integer(xval),
     which = integer(nobs),
     cptable = matrix(double(numcp*cpcol), nrow = cpcol),
     dsplit = matrix(double(1L), nsplit, 3L),
     isplit = matrix(integer(1L), nsplit, 3L),
     csplit = catmat,
     dnode = matrix(double(1L), nodes, 3L + numresp),
     inode = matrix(integer(1), nodes, 6L),
     PACKAGE = "mvpartCLEAN") -> rp
  tname <- c("<leaf>", dimnames(X)[[2L]])
  if(cpcol == 3) {
    temp <- c("CP","nsplit","rel error")
  } else temp <- c("CP","nsplit","rel error","xerror","xstd")
  dimnames(rp$cptable) <- list(temp, 1:numcp)
  dn1 <- if(nsplit == 0) character(0L) else tname[rp$isplit[,1L] + 1L]
  matrix(
    c(rp$isplit[, 2:3], rp$dsplit),
    ncol = 5,
    dimnames = list(dn1, c("count","ncat","improve","index","adj"))
  ) -> splits
  index <- rp$inode[,2L]
  nadd <- sum(isord[rp$isplit[,1L]])
  if(nadd > 0) {
    newc <- matrix(integer(1L), nadd, max(cats))
    cvar <- rp$isplit[,1L]
    indx <- isord[cvar]
    cdir <- splits[indx, 2]
    ccut <- floor(splits[indx, 4])
    splits[indx, 2] <- cats[cvar[indx]]
    splits[indx, 4] <- ncat + 1:nadd
    for(i in 1:nadd) {
      newc[i,1L:(cats[(cvar[indx])[i]])] <- -1L*as.integer(cdir[i])
      newc[i,1L:ccut[i]] <- as.integer(cdir[i])
    }
    if (ncat == 0) catmat <- newc else catmat <- rbind(rp$csplit, newc)
    ncat <- ncat + nadd
  } else catmat <- rp$csplit
  if (nsplit == 0) {
    data.frame(
      row.names = 1,
      var = "<leaf>",
      n = rp$inode[,5L],
      wt = rp$dnode[,3L],
      dev = rp$dnode[,1L],
      yval = rp$dnode[,4L],
      complexity = rp$dnode[,2L],
      ncompete = pmax(0,rp$inode[,3L] - 1),
      nsurrogate = rp$inode[,4L]) -> frame
  } else {
    temp <- ifelse(index == 0, 1, index)
    svar <- ifelse(index == 0, 0, rp$isplit[temp, 1])
    data.frame(
      row.names = rp$inode[,1L],
      var = factor(svar, 0L:ncol(X), tname),
      n = rp$inode[,5L],
      wt = rp$dnode[,3L],
      dev = rp$dnode[,1L],
      yval = rp$dnode[,4L],
      complexity = rp$dnode[,2L],
      ncompete = pmax(0, rp$inode[,3L] - 1),
      nsurrogate = rp$inode[,4L]) -> frame
  }
  if(method == "class") {
    numclass <- init$numresp - 1L
    temp <- rp$dnode[, -(1L:4L), drop = FALSE] %*%
      diag(init$parms$prior * sum(init$counts)/pmax(1, init$counts))
    yprob <- temp/rowSums(temp)
    yval2 <- matrix(rp$dnode[, -(1L:3L), drop = FALSE], ncol = numclass + 1)
    frame$yval2 <- cbind(yval2, yprob)
  } else if(method == "mrt") {
    frame$yval <- apply(rp$dnode[, -c(1L:3L), drop = FALSE], 1, mean)
    frame$yval2 <- rp$dnode[, -(1L:3L), drop = FALSE]
  }
  if(is.null(init$summary))
    stop("Initialization routine is missing the summary function")
  if(is.null(init$print)) {
    functions <- list(summary = init$summary)
  } else functions <- list(summary = init$summary, print = init$print)
  if(!is.null(init$text))
    functions <- c(functions, list(text = init$text))
  if(!is.null(init$bar))
    functions <- c(functions, list(bar = init$bar))
  if(method == "user")
    functions <- c(functions, mlist)
  where <- rp$which
  names(where) <- row.names(m)
  if(nsplit == 0) {
    list(
      frame = frame, where = where, call = call, terms = Terms,
      cptable = t(rp$cptable), method = method, dissim = dissim,
      parms = init$parms, control = controls, functions = functions
    ) -> ans
  } else {
    list(
      frame = frame, where = where, call = call, terms = Terms,
      cptable = t(rp$cptable), splits = splits, method = method,
      dissim = dissim, parms = init$parms, control = controls,
      functions = functions
    ) -> ans
  }
  if(ncat > 0)
    ans$csplit <- catmat + 2
  if(model) {
    ans$model <- m
    if(missing(y))
      y <- FALSE
  }
  if(y)
    ans$y <- Y
  if(x) {
    ans$x <- X
    ans$wt <- wt
  }
  ans$ordered <- isord
  if(!is.null(attr(m, "na.action")))
    ans$na.action <- attr(m, "na.action")
  if(!is.null(xlevels))
    attr(ans, "xlevels") <- xlevels
  if(method == "class")
    attr(ans, "ylevels") <- init$ylevels
  class(ans) <- "rpart"
  ans
}
