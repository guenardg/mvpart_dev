
## Function for internal use only.

node.match <- function(nodes, nodelist, leaves, print.it = TRUE) {
  node.index <- match(nodes, nodelist, nomatch = 0)
  bad <- nodes[node.index == 0L]
  if(length(bad) > 0 & print.it)
    warning(
      paste(
        "supplied nodes",
        paste(bad, collapse = ","),
        "are not in this tree"
      )
    )
  good <- nodes[node.index > 0L]
  if(!missing(leaves) && any(leaves <- leaves[node.index])) {
    warning(
      paste(
        "supplied nodes",
        paste(good[leaves], collapse = ","),
        "are leaves"
      )
    )
    node.index[node.index > 0L][!leaves]
  } else node.index[node.index > 0L]
}
