#'
#' Follow Paths to Selected Nodes of an Rpart Object
#' 
#' Returns a names list where each element contains the splits on the path from
#' the root to the selected nodes.
#' 
#' @name path.rpart
#' 
#' @param tree A fitted model object of class \code{rpart}. This is assumed to
#' be the result of some function that produces an object with the same named
#' components as that returned by the \code{rpart} function.
#' @param nodes An integer vector containing indices (node numbers) of all nodes
#' for which paths are desired.  If missing, user selects nodes as described
#' below.
#' @param pretty An integer denoting the extent to which factor levels in split
#' labels will be abbreviated.  A value of (0) signifies no abbreviation.  A
#' \code{NULL}, the default, signifies using elements of letters to represent
#' the different factor levels.
#' @param print.it Logical. Denotes whether paths will be printed out as nodes
#' are interactively selected. Irrelevant if \code{nodes} argument is supplied.
#' 
#' @returns A named (by node) list, each element of which contains all the
#' splits  on the path from the root to the specified or selected nodes.
#' 
#' @details The function has a required argument as an \code{rpart} object and
#' a list of nodes as optional arguments. Omitting a list of nodes will cause
#' the function to wait for the user to select nodes from the dendogram. It
#' will return a list, with one component for each node specified or selected.
#' The component contains the sequence of splits leading to that node. In the
#' graphical interaction, the individual paths are printed out as nodes are
#' selected. This function was modified from \code{path.tree} in S.
#' 
#' Graphical Interaction
#' A dendrogram of the \code{rpart} object is expected to be visible on the
#' graphics device, and a graphics input device (e.g., a mouse) is required.
#' Clicking (the selection button) on a node selects that node. This process may
#' be repeated any number of times. Clicking the exit button will stop the
#' selection process and return the list of paths.
#' 
#' @seealso \code{\link{rpart}}
#' 
#' @examples
#' data(kyphosis)
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
## summary(fit)
#' path.rpart(fit, node=c(11, 22))
#' 
#' @keywords tree
#' 
#' @export
path.rpart <- function(tree, nodes, pretty = 0, print.it = TRUE) {
  if(!inherits(tree, "rpart"))
    stop("Not legitimate tree")
  splits <- labels.rpart(tree, pretty=pretty)
  frame <- tree$frame
  n <- row.names(frame)
  node <- as.numeric(n)
  which <- descendants(node)      #ancestors are columns
  path <- list()
  if(missing(nodes)) {
    xy <- rpartco(tree)
    while(length(i <- identify(xy, n = 1, plot = FALSE)) > 0) {
      path[[n[i]]] <- path.i <- splits[which[, i]]
      if(print.it) {
        cat("\n", "node number:", n[i], "\n")
        cat(paste("  ", path.i), sep = "\n")
      }
    }
  } else {
    if(length(nodes <- node.match(nodes, node)) == 0)
      return(invisible())
    for(i in nodes) {
      path[[n[i]]] <- path.i <- splits[which[, i]]
      if(print.it) {
        cat("\n", "node number:", n[i], "\n")
        cat(paste("  ", path.i), sep = "\n")
      }
    }
  }
  invisible(path)
}
