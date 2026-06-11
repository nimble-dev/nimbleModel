## Class for indexRules, which are individual pieces of graphRules.

## An `indexRule` determines the mapping between index values (for one more
## more index slots) for a single `indexSet`.

indexRuleClass <- R6Class(
  classname = "indexRuleClass",
  portable = FALSE,
  public = list()
)

## `getOffset` looks for index value +/- offset.

## FUTURE: we might be able to use some old code to partially evaluate
## more complicated expressions, or possibly use Ryacas.
## However, it is not clear what more complicated expressions we
## want to handle. Perhaps something like `i+3+7`.
## Something like i*3 could possibly be handled to avoid
## full unrolling, but result can't be handled except as indexRangeMatrix anyway

getOffset <- function(indexExpr,
                      indexVarName,
                      constants = new.env(parent = getDefaultNamespace())) {
  offset <- 0

  if (is.name(indexExpr)) { # `i`
    indexNameInExpr <- as.character(indexExpr)
  } else {
    if (!as.character(indexExpr[[1]]) %in% c("+", "-")) {
      return(NULL)
    }
    indexSlot <- NULL
    ## e.g., `3+i`, `foo(k) + i`
    if (is.name(indexExpr[[3]]) && as.character(indexExpr[[1]]) == "+") {
      indexSlot <- 3
    }
    ## e.g., `i+3`, `i-3`, `i + foo(k)`
    if (is.name(indexExpr[[2]])) {
      indexSlot <- 2
    }

    if (is.null(indexSlot)) {
      return(NULL)
    }
    indexNameInExpr <- as.character(indexExpr[[indexSlot]])
    offsetExpr <- indexExpr
    offsetExpr[[indexSlot]] <- 0
    offset <- try(eval(offsetExpr, envir = constants), silent = TRUE)
    ## Check whether can resolve offset (will fail when there is another index
    ## in the expression).
    if (inherits(offset, "try-error")) {
      return(NULL)
    }
  }
  if (indexNameInExpr != indexVarName) {
    return(NULL)
  }
  list(offset = offset)
}
