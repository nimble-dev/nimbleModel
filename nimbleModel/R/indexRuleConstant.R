## This rule handles constant indices in the output, such as
## `y[2] <- ...` or `y[2:4]` <- ...` or `y[c(2,4)] <- ...`

## Note that constant indices in the input are handed by `indexConstraint`s.

indexRuleConstantClass <- R6Class(
  classname = "indexRuleConstantClass",
  inherit = indexRuleClass,
  portable = FALSE,
  public = list(
    setupResults = NULL,
    initialize = function(toIndexExprList,
                          fromIndexExprList,
                          context,
                          constants = list()) {
      setupResults <<-
        indexRuleConstant_setup(
          toIndexExprList,
          fromIndexExprList,
          context,
          constants
        )
    },
    apply = function(indexRange, collapse = FALSE) {
      return(setupResults$constant)
    },
    getMax = function() {
      return(NULL)
    }
  )
)


indexRuleConstant_setup <- function(toIndexExprList,
                                    fromIndexExprList,
                                    context,
                                    constants = list()) {
  ## Only valid if no indexing, and a single `to` index slot.
  if (length(fromIndexExprList) || length(context$singleContexts) ||
    length(toIndexExprList) > 1) {
    return(NULL)
  }

  if (is.list(constants)) {
    constants <- list2env(constants, parent = getDefaultNamespace())
  }

  if (!length(toIndexExprList)) { # No indexing; handled by indexRuleNone.
    return(NULL)
  } else {
    toIndexExpr <- toIndexExprList[[1]]
    if (length(toIndexExpr) == 1) {
      toConstant <- indexRangeScalarClass$new(eval(toIndexExpr, envir = constants))
    } else if (length(toIndexExpr) == 3 && toIndexExpr[[1]] == ":") {
      toConstant <- indexRangeSequenceClass$new(
        eval(toIndexExpr[[2]], envir = constants),
        eval(toIndexExpr[[3]], envir = constants)
      )
    } else if (length(toIndexExpr) > 1 && toIndexExpr[[1]] == "nimC") {
      toIndexExpr[[1]] <- quote(c) # `nimC` not readily available in series of environments.
      toConstant <- indexRangeMatrixClass$new(as.matrix(eval(toIndexExpr, envir = constants)))
    } else {
      stop("input error in `", safeDeparse(toIndexExprList), "`.")
    }
  }
  return(list(constant = toConstant))
}
