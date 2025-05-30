
## Function for internal use only.

rpart.branch <- function(x, y, node, branch) {
  # Draw a series of horseshoes, left son, up, over, down to right son
  #   NA's in the vector cause lines() to "lift the pen"
  is.left <- (node%%2 == 0)        #left hand sons
  node.left <- node[is.left]
  parent <- match(node.left/2, node)
  sibling <- match(node.left + 1, node)
  temp <- (x[sibling] - x[is.left])*(1 - branch)/2
  xx <- rbind(x[is.left], x[is.left] + temp, x[sibling] - temp, x[sibling], NA)
  yy <- rbind(y[is.left], y[parent], y[parent], y[sibling], NA)
  list(x=xx, y=yy)
}
