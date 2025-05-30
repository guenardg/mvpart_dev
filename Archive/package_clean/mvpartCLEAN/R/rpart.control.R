#'
#' Control for Rpart Models.
#' 
#' Various parameters that control aspects of the \code{rpart} fit.
#' 
#' @name rpart.control
#' 
#' @param minsplit The minimum number of observations that must exist in a node,
#' in order for a split to be attempted.
#' @param minbucket The minimum number of observations in any terminal
#' \code{<leaf>} node. If only one of \code{minbucket} or \code{minsplit} is
#' specified, the code either sets \code{minsplit} to \code{minbucket*3} or
#' \code{minbucket} to \code{minsplit/3}, as appropriate.
#' @param cp Complexity parameter.  Any split that does not decrease the overall
#' lack of fit by a factor of \code{cp} is not attempted. For instance, with
#' \code{anova} splitting, this means that the overall Rsquare must increase by
#' \code{cp} at each step. The main role of this parameter is to save computing
#' time by pruning off splits that are obviously not worthwhile. Essentially,
#' the user informs the program that any split which does not improve the fit by
#' \code{cp} will likely be pruned off by cross-validation, and that hence the
#' program need not pursue it.
#' @param maxcompete The number of competitor splits retained in the output. It
#' is useful to know not just which split was chosen, but which variable came in
#' second, third, etc.
#' @param maxsurrogate The number of surrogate splits retained in the output. If
#' this is set to zero the compute time will be shortened, since approximately
#' half of the computational time (other than setup) is used in the search for
#' surrogate splits.
#' @param usesurrogate How to use surrogates in the splitting process.
#' 0 = display only; an observation with a missing value for the primary split
#' rule is not sent further down the tree.  1= use surrogates, in order, to
#' split subjects missing the primary variable; if all surrogates are missing
#' the observation is not split.  2= if all surrogates are missing, then send
#' the observation in the majority direction.  A value of 0 corresponds to the
#' action of tree, and 2 to the recommendations of Breiman, et.al.
#' @param xval Number of cross-validations.
#' @param surrogatestyle Controls the selection of a best surrogate. If set to 0
#' (default), the program uses the total number of correct classification for a
#' potential surrogate variable, if set to 1 it uses the percent correct,
#' calculated over the non-missing values of the surrogate. The first option
#' more severely penalizes covariates with a large number of missing values.
#' @param maxdepth Sets the maximum depth of any node of the final tree, with
#' the root node counted as depth 0 (past 30 \code{rpart} will give nonsense
#' results on 32-bit machines.
#' @param ... Mop-up of any other arguments.
#' 
#' @returns A list containing the options.
#' 
#' @seealso \code{rpart}
#' 
#' @keywords tree
#' 
#' @export
rpart.control <- function(minsplit = 5L, minbucket = round(minsplit/3),
                          cp = 0.01, maxcompete = 4L, maxsurrogate = 0L,
                          usesurrogate = 2L, xval = 10L, surrogatestyle = 0L,
                          maxdepth = 30L, ...) {
  if(maxcompete < 0L) {
    warning(
      "The value of maxcompete supplied is <0; the value 0 was used instead"
    )
    maxcompete <- 0L
  }
  if (any(xval < 0L)) {
    warning("The value of xval supplied is <0; the value 0 was used instead")
    xval <- 0L
  }
  if(maxdepth > 30L) 
    stop("Maximum depth is 30")
  if(maxdepth < 1L) 
    stop("Maximum depth must be at least 1")
  if(missing(minsplit) && !missing(minbucket)) 
    minsplit <- minbucket * 3L
  if((usesurrogate < 0L) || (usesurrogate > 2L)) {
    warning("The value of usesurrogate supplied was out of range,", 
            "the default value of 2 is used instead.")
    usesurrogate <- 2
  }
  if((surrogatestyle < 0L) || (surrogatestyle > 1L)) {
    warning("The value of surrogatestyle supplied was out of range,", 
            "the default value of 0 is used instead.")
    surrogatestyle <- 0
  }
  list(minsplit = minsplit, minbucket = minbucket, cp = cp,
       maxcompete = maxcompete, maxsurrogate = maxsurrogate,
       usesurrogate = usesurrogate, surrogatestyle = surrogatestyle,
       maxdepth = maxdepth, xval = xval)
}
