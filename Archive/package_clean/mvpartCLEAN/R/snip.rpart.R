#'
#' Snip Subtrees of an Rpart Object
#' 
#' Creates a "snipped" rpart object, containing the nodes that remain after
#' selected subtrees have been snipped off.  The user can snip nodes using the
#' toss arguement, or interactively by clicking the mouse button on specified
#' nodes within the graphics window.
#' 
#' @name snip.rpart
#' 
#' @param x An object of class \code{rpart}. This is assumed to be the result
#' of some function that produces an object with the same named components as
#' that returned by the \code{rpart} function.
#' @param toss An integer vector containing indices (node numbers) of all
#' subtrees to be snipped off.  If missing, user selects branches to snip off as
#' described below.
#' 
#' @returns A \code{rpart} object containing the nodes that remain after
#' specified or selected subtrees have been snipped off.
#' 
#' @details A dendrogram of \code{rpart} is expected to be visible on the
#' graphics device, and a graphics input device (e.g., a mouse) is required.
#' Clicking (the selection button) on a node displays the node number, sample
#' size, response yvalue, and Error (dev). Clicking a second time on the same
#' node snips that subtree off and visually erases the subtree. This process may
#' be repeated an number of times.  Warnings result from selecting the root or
#' leaf nodes.  Clicking the exit button will stop the snipping process and
#' return the resulting \code{rpart} object.
#' 
#' Warning: visually erasing the plot is done by over-plotting with the
#' background colour.  This will do nothing if the background is transparent
#' (often true for screen devices).
#' 
#' See the documentation for the specific graphics device for details on
#' graphical input techniques.
#' 
#' 
#' @seealso \code{plot.rpart}
#' 
#' @keywords tree
#' 
#' @export
snip.rpart <- function(x, toss) {
  if(!inherits(x, 'rpart'))
    stop("Not an rpart object")
  if(missing(toss) || length(toss)==0) {
    toss <- snip.rpart.mouse(x)
    if (length(toss)==0) return(x)
  }
  where <- x$where
  ff   <- x$frame
  id    <- as.numeric(row.names(ff))
  index <- ff$index
  ff.n  <- length(id)
  toss <- unique(toss)
  toss.idx <- match(toss, id, nomatch=0) #the rows of the named nodes
  if(any(toss.idx ==0)) {
    warning(paste("Nodes", toss[toss.idx==0], "are not in this tree"))
    toss <- toss[toss.idx>0]
    toss.idx <- toss.idx[toss.idx>0]
  }
  #    if (any(toss==1))  {
  #   # a special case that causes grief later
  #   warning("Can't prune away the root node and still have a tree!")
  #        return(NULL)
  #   }
  # Now add all of the descendants of the selected nodes
  #   We do this be finding all node's parents.
  #        (Division by 2 gives the parent of any node.)
  #   At each step we make id2 <- parent(id2), and augment 'toss' with
  #     found children.  The loop should take <  log_2(maxdepth)/2 steps
  id2 <- id
  while(any(id2>1)) {
    id2 <- floor(id2/2)
    xx <- (match(id2, toss, nomatch=0) >0)
    toss <- c(toss, id[xx])
    id2[xx] <- 0
  }
  # Now "toss" contains all of the nodes that should not be splits
  temp <- match(floor(toss/2) , toss, nomatch=0)  #which are leaves?
  newleaf <- match(toss[temp==0], id)             # row numbers, leaves
  keepit <- (1:ff.n)[is.na(match(id,toss))]  # row numbers to be let be
  # Compute the parent row for each row in the splits structure
  #  Then "thin out" the splits and csplit components
  n.split <- rep((1:ff.n), ff$ncompete + ff$nsurrogate+ 1*(ff$var!='<leaf>'))
  split <- x$splits[match(n.split, keepit, nomatch=0) >0, ,drop=FALSE]
  temp <- split[,2] >1      #which rows point to categoricals?
  if(any(temp)) {
    x$csplit <- x$csplit[split[temp,4], , drop=FALSE]
    split[temp,4] <- 1
    if(is.matrix(x$csplit)) split[temp,4] <- 1:nrow(x$csplit)
  } else x$csplit <- NULL
  x$splits <- split
  # Thin out unneeded rows in the frame component
  ff$ncompete[newleaf] <- ff$nsurrogate[newleaf] <- 0
  ff$var[newleaf]     <- "<leaf>"
  x$frame <- ff[sort(c(keepit, newleaf)),]
  # Now do the 'parents' loop one more time, to fix up the "where"
  #   vector
  # This pass requires log_2(depth) iterations
  #
  id2 <- id[x$where]         #the list of old leaf nodes
  id3 <- id[sort(c(keepit, newleaf))]
  temp <- match(id2, id3, nomatch=0)
  while (any(temp==0)) {
    id2[temp==0] <- floor(id2[temp==0]/2)
    temp <- match(id2, id3, nomatch=0)
  }
  x$where <- match(id2, id3)
  x
}
