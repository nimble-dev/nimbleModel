## This is for a case like y[i] <- foo(x) or y[i] <- foo(x[]).
## It is possible x is non-scalar, but the relationships
## does not use any indices in x.
indexRuleClass_all <- R6Class(
    classname = "indexRuleClass_all",
    inherit = indexRuleClass,
    portable = FALSE,
    public = list(
        setupResults = NULL,
        constantAnswer = NULL,
        initialize = function(toIndexExprList,
                              fromIndexExprList,
                              context,
                              constants = list()
                              ) {
            ## This rule is only valid if the index is not on RHS,
            ## so return NULL is it will not be valid.
            if(length(fromIndexExprList) != 0) {
                setupResults <<- NULL
                return
            } else {
                setupResults <<-
                    indexRule_all_setup(toIndexExprList,
                                        fromIndexExprList,
                                        context,
                                        constants
                                        )
                constantAnswer <<- indexRange_block(as.list(setupResults))
            }
        },
        applyOne = function(fromIndices) {
            constantAnswer
        },
        apply_indexRange = function(fromIndexRange,
                                    ...) {
            constantAnswer
        },
        apply = function(from, ...) {
            constantAnswer
        }        
    )
)

indexRule_all_setup <- function(toIndexExprList,
                                fromIndexExprList,
                                context,
                                constants = list()) {
    toIndexExpr <- toIndexExprList[[1]]
    ## May need to generalize which indexVarName is used:
    indexVarName <- context$indexVarNames[1]
    ## May need to fail cleanly if RHS is like y[k[i]] or other complication.
    toSignAndOffset <- getSignAndOffset(toIndexExpr,
                                        indexVarName,
                                        constants)
    indexRangeExpr <- context$singleContexts[[1]]$indexRangeExpr

    index_range <- 
        c(eval(indexRangeExpr[[2]], envir = constants),
          eval(indexRangeExpr[[3]], envir = constants))

    toSignAndOffset$offset + index_range
}
