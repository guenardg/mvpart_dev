#'
#' Formula Constructor
#' 
#' This function constructs a formula from a data frame given the locations of
#' the response and other variables. It is useful for long or repetitive
#' formulas.
#' 
#' @name makeform
#' 
#' @param data The data frame or matrix for which the formula is to be
#' constructed.
#' @param ycol The locations (or names) of the response variables in
#' \code{data}.
#' @param xcol The locations (or names) of the explanatory variables in
#' \code{data}.
#' @param zcol The locations (or names) of the conditioning variables in
#' \code{data}; for use in the \code{vegan} package.
#' @param FUN A function to apply to the response.
#' @param maxy The number of multivariate responses before abbreviated notation
#' is used. See examples below.
#' @param extra Extra term(s) for the RHS of the formula.
#' 
#' 
#' 
#' @details makeform constructs a formula and is useful for long and/or
#' repetitive formulae. See examples below.
#' 
#' @returns Returns a formula.
#' 
#' @examples
#' data(spider)
#' makeform(spider,1,13:18)
#' # arct.lute ~ water + sand + moss + reft + twigs + herbs
#' 
#' makeform(spider,1:12,13:18)
#' # cbind(arct.lute, pard.lugu, zora.spin, pard.nigr, pard.pull, 
#' # aulo.albi, troc.terr, alop.cune, pard.mont, alop.acce, alop.fabr, 
#' # arct.peri) ~ water + sand + moss + reft + twigs + herbs
#' 
#' makeform(spider,1:12,13:15,16:18)
#' # cbind(arct.lute, pard.lugu, zora.spin, pard.nigr, pard.pull, 
#' # aulo.albi, troc.terr, alop.cune, pard.mont, alop.acce, alop.fabr, 
#' # arct.peri) ~ water + sand + moss + Condition(reft + twigs + herbs)
#' 
#' makeform(spider,1:12,13:15,maxy=6)
#' # as.matrix(spider[, 1:12]) ~ water + sand + moss
#' 
#' makeform(spider,1:3,13:15,FUN=sqrt)
#' # sqrt(cbind(arct.lute, pard.lugu, zora.spin)) ~ water + sand + moss
#' 
#' makeform(spider,1:3,13:15,FUN=sqrt,extra="I(water^2)+")
#' # sqrt(cbind(arct.lute, pard.lugu, zora.spin)) ~ I(water^2) + water + 
#' # sand + moss
#' 
#' @keywords models
#' 
#' @importFrom stats formula
#' 
#' @export
makeform <- function (data, ycol, xcol, zcol, FUN, maxy = 20, extra) {
  ny <- length(ycol)
  nx <- ifelse(!missing(xcol), length(xcol), 0L)
  nz <- ifelse(!missing(zcol), length(zcol), 0L)
  dnames <- colnames(data)
  if(ny > maxy) {
    yy <- deparse(substitute(ycol))
    ty <- paste("as.matrix(", deparse(substitute(data)), 
                "[,", yy, "])", collapse = "", sep = "")
    if(!missing(FUN)) 
      ty <- paste(deparse(substitute(FUN)), "(", ty, ")", sep = "")
    if (nx > 1) {
      tx <- paste(dnames[xcol], collapse = "+")
    } else if (nx == 1) {
      tx <- dnames[xcol]
    }  else tx <- "1"
  } else {
    if (ny > 1) {
      ty <- paste(dnames[ycol], collapse = ",")
      ty <- paste("cbind(", ty, ")", collapse = "", sep = "")
    } else if (ny == 1) {
      ty <- dnames[ycol]
    } else ty <- ""
    if (!missing(FUN)) 
      ty <- paste(deparse(substitute(FUN)), "(", ty, ")", sep = "")
    if (nx > 1) {
      tx <- paste(dnames[xcol], collapse = "+")
    } else if (nx == 1) {
      tx <- dnames[xcol]
    } else tx <- "1"
  }
  if(missing(extra)) {
    form <- paste(ty, "~", tx, collapse = "", sep = "")
  } else form <- paste(ty, "~", extra, tx, collapse = "", sep = "")
  if(nz > 1) {
    tz <- paste(dnames[zcol], collapse = "+")
  } else if (nz == 1) tz <- dnames[zcol]
  if(nz > 0)
    form <- paste(form, "+ Condition(", tz, ")", sep = "")
  formula(form)
}
