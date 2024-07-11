#' 
#' Return Cross-Validated Predictions
#' 
#' Gives the predicted values for an \code{rpart} fit, under cross validation,
#' for a set of complexity parameter values.
#' 
#' @name xpred.rpart
#' 
#' @param fit An \code{rpart} object.
#' @param xval Number of cross-validation groups. This may also be an explicit
#' list of integers that define the cross-validation groups.
#' @param cp The desired list of complexity values. By default it is taken from
#' the \code{cptable} component of the fit.
#' 
#' @returns A matrix with one row for each observation and one column for each
#' complexity value.
#' 
#' @details
#' Complexity penalties are actually ranges, not values. If the \code{cp} values
#' found in the table were .36, .28, and .13, for instance, this means that the
#' first row of the table holds for all complexity penalties in the >=.36, <=1
#' range, the second row for \code{cp} in the >=.28, <.36 and the third row for
#' >=.13 <.28. By default, the geometric mean of each interval is used for cross
#' validation.
#' 
#' @seealso \code{\link{rpart}}
#' 
#' @examples
#' data(car.test.frame)
#' fit <- rpart(Mileage ~ Weight, car.test.frame)
#' xmat <- xpred.rpart(fit)
#' xerr <- (xmat - car.test.frame$Mileage)^2
#' apply(xerr, 2, sum)   # cross-validated error estimate
#' 
#' # approx same result as rel. error from printcp(fit)
#' apply(xerr, 2, sum)/var(car.test.frame$Mileage)
#' printcp(fit)
#' 
#' @keywords tree
#' 
#' @export
xpred.rpart <- function(fit, xval=10, cp) {
  if (!inherits(fit,"rpart"))
    stop("Invalid fit object")
  method <- fit$method
  method.int <- pmatch(method, c("anova","poisson","class","user","exp"))
  if(method.int == 5)
    method.int <- 2
  Terms <- fit$terms
  Y <- fit$y
  X <- fit$x
  wt <- fit$wt
  if(is.null(Y) || is.null(X)) {
    m <- fit$model
    if(is.null(m)) {
      m <-fit$call[match(c("","formula","data","weights","subset","na.action"),
                         names(fit$call), nomatch=0)]
      if(is.null(m$na.action))
        m$na.action <- na.rpart
      m[[1L]] <- as.name("model.frame.default")
      m <- eval(m, parent.frame())
    }
    if(is.null(X))
      X <- rpart.matrix(m)
    if(is.null(wt))
      wt <- model.extract(m, "weights")
    if(is.null(Y)) {
      yflag <- TRUE
      Y <- model.extract(m, "response")
      offset <- attr(Terms, "offset")
      if(method != "user") {
        init <- (get(paste("rpart", method, sep='.')))(Y,offset, NULL)
        Y <- as.matrix(init$y)
        numy <- ncol(Y)
      }
    } else {
      yflag <- FALSE
      Y <- as.matrix(Y)
      numy <- ncol(Y)
      offset <- 0
    }
  } else {
    yflag <- FALSE
    Y <- as.matrix(Y)
    numy <- ncol(Y)
    offset <- 0
  }
  nobs <- nrow(X)
  nvar <- ncol(X)
  if(length(wt)==0)
    wt <- rep(1.0, nobs)
  cats <- rep(0, nvar)
  xlevels <- attr(fit, "xlevels")
  if(!is.null(xlevels)) {
    cats[match(names(xlevels), dimnames(X)[[2]])] <-
      unlist(lapply(xlevels, length))
  }
  controls <- fit$control
  if(missing(cp)) {
    cp <- fit$cptable[,1L]
    cp <- sqrt(cp*c(10, cp[-length(cp)]))
    cp[1] <- (1 + fit$cptable[1L,1L])/2
  }
  ncp <- length(cp)
  if(length(xval)==1) { # make random groups
    xgroups <- sample(rep(1:xval, length=nobs), nobs, replace=FALSE)
  } else if(length(xval) == nrow(Y)) {
    xgroups <- xval
    xval <- length(unique(xgroups))
  } else {
    ## Check to see if observations were removed due to missing
    if(!is.null(fit$na.action)) {
      ## if na.rpart was used, then na.action will be a vector
      temp <- as.integer(fit$na.action)
      xval <- xval[-temp]
      if(length(xval) == nobs) {
        xgroups <- xval
        xval <- length(unique(xgroups))
      } else stop("Wrong length for xval")
    }
    else stop("Wrong length for xval")
  }
  costs <- fit$call$costs
  if(is.null(costs))
    costs <- rep(1.0, nvar)
  parms <- fit$parms
  if(method=="user") {
    mlist <- fit$functions
    if (length(parms) == 0) {
      init <- mlist$init(Y, offset, wt=wt)
    } else init <- mlist$init(Y, offset, parms, wt)
    ## assign this to avoid garbage collection
    keep <- rpartcallback(mlist, nobs, init)
  }
  .C("s_xpred",
     n = as.integer(nobs),
     nvarx = as.integer(nvar),
     ncat = as.integer(cats * !fit$ordered),
     method= as.integer(method.int),
     as.double(unlist(controls)),
     parms = as.double(unlist(parms)),
     as.integer(xval),
     as.integer(xgroups),
     as.double(t(Y)),
     as.double(X),
     as.integer(is.na(X)),
     pred = double(ncp* nobs),
     as.integer(ncp),
     as.double(cp * fit$frame[1,"dev"]),
     error = character(1),
     wt = as.double(wt),
     as.integer(numy),
     as.double(costs),
     NAOK=TRUE,
     PACKAGE = "mvpartCLEAN") -> rpfit
  if(rpfit$n == -1)
    stop(rpfit$error)
  matrix(rpfit$pred, ncol=ncp, byrow=TRUE,
         dimnames=list(dimnames(X)[[1]], format(cp)))
}
