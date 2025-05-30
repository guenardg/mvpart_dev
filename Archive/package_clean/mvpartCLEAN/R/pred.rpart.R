
## Function for internal use only.

pred.rpart <- function(fit, x) {
  frame <- fit$frame
  if(nrow(frame) == 1) { # handle root-only tree separately
      temp <- rep(1, nrow(x))
  } else {
    nc <- frame[, c("ncompete","nsurrogate")]
    frame$index <- 1 +
      c(0, cumsum((frame$var != "<leaf>") + nc[[1]] + nc[[2]]))[-(nrow(frame) + 1)]
    frame$index[frame$var == "<leaf>"] <- 0
    vnum <- match(dimnames(fit$split)[[1]], dimnames(x)[[2]])
    if(any(is.na(vnum)))
      stop("Tree has variables not found in new data")
    .C("pred_rpart",
      as.integer(dim(x)),
      as.integer(dim(frame)[1]),
      as.integer(dim(fit$splits)),
      as.integer(if(is.null(fit$csplit)) rep(0,2) else dim(fit$csplit)),
      as.integer(row.names(frame)),
      as.integer(unlist(frame[,c("n","ncompete","nsurrogate","index")])),
      as.integer(vnum),
      as.double(fit$splits),
      as.integer(fit$csplit -2),
      as.integer((fit$control)$usesurrogate),
      as.double(x),
      as.integer(is.na(x)),
      where = integer(dim(x)[1]),
      NAOK = TRUE,
      PACKAGE = "mvpartCLEAN")$where -> temp
  }
  names(temp) <- rownames(x)
  temp
}
