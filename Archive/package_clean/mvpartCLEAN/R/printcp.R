#'
#' Displays CP table for Fitted Rpart Object 
#' 
#' Displays the \code{cp} table for fitted \code{rpart} object.
#' 
#' @name printcp
#' 
#' @param x Fitted model object of class \code{rpart}. This is assumed to be the
#' result of some function that produces an object with the same named
#' components as that returned by the \code{rpart} function.
#' @param digits The number of digits of numbers to print.
#' 
## @returns \code{NULL} (invisibly). ?????
#' 
#' @details Prints a table of optimal pruning based on a complexity parameter.
#'
#' @seealso \code{summary.rpart}, \code{rpart.object}
#' 
#' @keywords tree
#' 
#' @examples
#' data(car.test.frame)
#' z.auto <- rpart(Mileage ~ Weight, car.test.frame)
#' printcp(z.auto)
#' \dontrun{
#'   Regression tree:
#'     rpart(formula = Mileage ~ Weight, data = car.test.frame)
#'   
#'   Variables actually used in tree construction:
#'     [1] Weight
#'   
#'   Root node error: 1354.6/60 = 22.576
#'   
#'   CP nsplit rel error  xerror     xstd 
#'   1 0.595349      0   1.00000 1.03436 0.178526
#'   2 0.134528      1   0.40465 0.60508 0.105217
#'   3 0.012828      2   0.27012 0.45153 0.083330
#'   4 0.010000      3   0.25729 0.44826 0.076998
#' }
#' 
#' @importFrom stats naprint
#' 
#' @export
printcp <- function(x, digits = getOption("digits") - 2L) {
  if (!inherits(x, 'rpart'))
    stop("Must be an rpart x")
  cat(
    switch(
      x$method,
      anova = "\nRegression tree:\n" ,
      class = "\nClassification tree:\n" ,
      poisson="\nRates regression tree:\n",
      exp = "\nSurvival regression tree:\n"
    )
  )
  if(!is.null(cl <- x$call)) {
    dput(cl)
    cat("\n")
  }
  frame <- x$frame
  leaves <- frame$var == "<leaf>"
  used <- unique(frame$var[!leaves])
  if(!is.null(used)) {
    cat("Variables actually used in tree construction:\n")
    print(sort(as.character(used)), quote=FALSE)
    cat("\n")
  }
  cat(
    "Root node error: ", format(frame$dev[1], digits=digits), '/',
    frame$n[1], ' = ',
    format(frame$dev[1]/frame$n[1], digits=digits),
    '\n\n', sep=''
  )
  n <- x$frame$n
  omit <- x$na.action
  if (length(omit)) {
    cat("n=", n[1], " (", naprint(omit), ")\n\n", sep="")
  } else cat("n=", n[1], "\n\n")
  print(x$cptable, digits=digits)
  invisible(x$cptable)
}
