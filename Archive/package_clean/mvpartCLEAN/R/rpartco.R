
## Function for internal use only.

rpartco <- function(tree, parms) {
  uniform <- parms$uniform
  nspace <- parms$nspace
  nbranch <- parms$nbranch
  minbranch <- parms$minbranch
  frame <- tree$frame
  method <- tree$method
  node <- as.numeric(row.names(frame))
  depth <- tree.depth(node)
  is.leaf <- (frame$var == '<leaf>')
  if(uniform) {
    y <- (1 + max(depth) -depth) / max(depth,4)
  } else {                    #make y- (parent y) = change in deviance
    y <- dev <- frame$dev
    temp <- split(seq(node), depth)     #depth 0 nodes, then 1, then ...
    parent <- match(floor(node/2), node)
    sibling <- match(ifelse(node %% 2, node - 1, node + 1), node)
    # assign the depths
    for(i in temp[-1]) {
      temp2 <- dev[parent[i]] - (dev[i] + dev[sibling[i]])
      y[i] <- y[parent[i]] - temp2
    }
    #
    # For some problems, classification & loss matrices in particular
    #   the gain from a split may be 0.  This is ugly on the plot.
    # Hence the "fudge" factor of  .3* the average step
    #
    fudge <-  minbranch * diff(range(y)) / max(depth)
      for(i in temp[-1]) {
      temp2 <- dev[parent[i]] - (dev[i] + dev[sibling[i]])
      haskids <- !(is.leaf[i] & is.leaf[sibling[i]])
      y[i] <- y[parent[i]] - ifelse(temp2<=fudge & haskids, fudge, temp2)
      }
    y <- y / (max(y))
  }
  # Now compute the x coordinates, by spacing out the leaves and then
  #   filling in
  x <- double(length(node))            # allocate, then fill it in below
  x[is.leaf] <- seq(sum(is.leaf))      # leaves at 1, 2, 3, ....
  left.child <- match(node * 2, node)
  right.child <- match(node * 2 + 1, node)
  # temp is a list of non-is.leaf, by depth
  temp <- split(seq(node)[!is.leaf], depth[!is.leaf])
  for(i in rev(temp))
    x[i] <- 0.5 * (x[left.child[i]] + x[right.child[i]])
  if(nspace < 0) return(list(x=x, y=y))
  #
  # Now we get fancy, and try to do overlapping
  #
  #  The basic algorithm is, at each node:
  #      1: get the left & right edges, by depth, for the left and
  #           right sons, of the x-coordinate spacing.
  #      2: find the minimal free spacing.  If this is >0, slide the
  #           right hand son over to the left
  #      3: report the left & right extents of the new tree up to the
  #           parent
  #   A way to visualize steps 1 and 2 is to imagine, for a given node,
  #      that the left son, with all its descendants, is drawn on a
  #      slab of wood.  The left & right edges, per level, give the
  #      width of this board.  (The board is not a rectangle, it has
  #      'stair step' edges). Do the same for the right son.  Now
  #      insert some spacers, one per level, and slide right hand
  #      board over until they touch.  Glue the boards and spacer
  #      together at that point.
  #
  #  If a node has children, its 'space' is considered to extend left
  #    and right by the amount "nspace", which accounts for space
  #    used by the arcs from this node to its children.  For
  #    horseshoe connections nspace usually is 1.
  #
  #  To make it global for a recursive function, the x coordinate list
  #    is written into frame 0.
  #
  compress <- function(me, depth) {
    lson <- me + 1
    x <- x
    if(is.leaf[lson]) {
      left <- list(left=x[lson], right=x[lson], depth=depth + 1, sons=lson)
    } else left <- compress(me + 1, depth + 1)
    rson <- me + 1 + length(left$sons)        #index of right son
    if(is.leaf[rson]) {
      right <- list(left=x[rson], right=x[rson], depth=depth + 1, sons=rson)
    } else right <- compress(rson, depth + 1)
    maxd <- max(left$depth, right$depth) - depth
    mind <- min(left$depth, right$depth) - depth
    # Find the smallest distance between the two subtrees
    #   But only over depths that they have in common
    # 1 is a minimum distance allowed
    slide <- min(right$left[1:mind] - left$right[1:mind]) -1
    if(slide >0) { # slide the right hand node to the left
      x[right$sons] <- x[right$sons] - slide;
      x[me] <- (x[right$sons[1]] + x[left$sons[1]])/2
      # assign("x", x)
      x <<- x
    } else slide <- 0
    # report back
    if(left$depth > right$depth) {
      templ <- left$left
      tempr <- left$right
      tempr[1:mind] <- pmax(tempr[1:mind], right$right -slide)
    } else {
      templ <- right$left  - slide
      tempr <- right$right - slide
      templ[1:mind] <- pmin(templ[1:mind], left$left)
    }
    list(
      left = c(x[me] - nspace*(x[me] -x[lson]), templ),
      right = c(x[me] - nspace*(x[me] -x[rson]), tempr),
      depth = maxd + depth, sons=c(me, left$sons, right$sons)
    )
  }
  compress(1, 1)
  list(x = x, y = y)
}

