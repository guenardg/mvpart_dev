#' 
#' PostScript Presentation Plot of an Rpart Object
#' 
#' Generates a PostScript presentation plot of an \code{rpart} object.
#' 
#' @param tree Fitted model object of class \code{rpart}.  This is assumed to be
#' the result of some function that produces an object with the same named
#' components as that returned by the \code{rpart} function. 
#' @param title A title which appears at the top of the plot.  By default, the 
#' name of the \code{rpart} endpoint is printed out.
#' @param filename ASCII file to contain the output.  By default, the name of
#' the file is the name of the object given by \code{rpart} (with the suffix
#' \code{.ps} added). If \code{filename = ""}, the plot appears on the current
#' graphical device.
#' @param digits Number of significant digits to include in numerical data.
#' @param pretty An integer denoting the extent to which factor levels will be
#' abbreviated in the character strings defining the splits; (0) signifies no
#' abbreviation of levels.  A \code{NULL} signifies using elements of letters to
#' represent the different factor levels.  The default (\code{TRUE}) indicates
#' the maximum possible abbreviation. 
#' @param use.n Logical. If \code{TRUE} (default), adds to label (\#events
#' level1/ \#events level2/etc. for method \code{class}, \code{n} for method
#' \code{anova}, and \#events/n for methods \code{poisson} and \code{exp}).
#' @param horizontal Logical. If \code{TRUE} (default), plot is horizontal. If
#' \code{FALSE}, plot appears as landscape.
#' @param ... Other arguments to the \code{postscript} function.
#' 
#' @details The plot created uses the functions \code{plot.rpart} and
#' \code{text.rpart} (with the \code{fancy} option).  The settings were chosen
#' because they looked good to us, but other options may be better, depending on
#' the \code{rpart} object. Users are encouraged to write their own function
#' containing favorite options.
#' 
#' A plot of \code{rpart} is created using the \code{postscript} driver, or the
#' current device if \code{filename = ""}.
#' 
#' @seealso \code{plot.rpart},  \code{rpart},  \code{text.rpart}
#' 
#' @importFrom grDevices dev.off postscript
#' 
#' @examples
#' data(car.test.frame)
#' z.auto <- rpart(Mileage ~ Weight, car.test.frame)
## post.rpart(z.auto, file = "")   # display tree on active device
#' # now construct postscript version on file "pretty.ps"
#' # with no title
## post.rpart(z.auto, file = "pretty.ps", title = " ")
#' z.hp <- rpart(Mileage ~ Weight + HP, car.test.frame)
## post.rpart(z.hp)
#' 
#' @export
post.rpart <- function(tree, title,
                       filename = paste(deparse(substitute(tree)),".ps",sep=""),
                       digits = getOption("digits") - 3L, pretty = TRUE,
                       use.n = TRUE,  horizontal = TRUE, ...) {
  if(filename !="") {
    postscript(file = filename, horizontal=horizontal, ...)
    par(mar=c(2,2,4,2)+.1)
    on.exit(dev.off())
  } else {
    oldpar <- par(mar=c(2,2,4,2)+.1)
    on.exit(invisible(par(oldpar)))
  }
  plot(tree, uniform=TRUE, branch=.2, compress=TRUE, margin=.1)
  text(tree, all=TRUE, use.n=use.n, digits=digits, pretty=pretty)
  method <- tree$method

  if(missing(title)) {
    temp  <- attr(tree$terms,'variables')[2L]
    title(paste("Endpoint =",temp),cex=.8)
  } else if (title !="") title(title,cex=.8)
  
  invisible(NULL)
}
