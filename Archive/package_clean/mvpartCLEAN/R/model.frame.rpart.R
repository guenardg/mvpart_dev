
## Function for internal use only.

model.frame.rpart <- function(formula, ...) {
  m <- formula$model
  if(!is.null(m))
    return(m)
  oc <- formula$call
  if(substring(deparse(oc[[1]]), 1, 7) == "predict") {
    m <- eval(oc$newdata)
    if(is.null(attr(m, "terms"))) {
        object <- eval(oc$object)
        m <- model.frame(object$terms, m, na.rpart)
    }
    return(m)
  }
  while(deparse(oc[[1]]) != "rpart")
    oc <- eval(oc[[2]])$call
  oc$subset <- names(formula$where)
  oc$method <- formula$method
  eval(oc)
}
