## This is for a case like y[i] <- foo(x).
## It is possible x is non-scalar, but the relationships
## does not use any indices in x.
indexRuleClass_any <- R6Class(
    classname = "indexRuleClass_any",
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
            setupResults <<-
                indexRule_any_setup(toIndexExprList,
                                    fromIndexExprList,
                                    context,
                                    constants
                                    )
            constantAnswer <<- setupResults
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

indexRule_any_setup <- function(toIndexExprList,
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
     ## We rely on eval here, but we could instead pick out
    ## arguments of `:`
    index_range <- 
        range(eval(indexRangeExpr, envir = constants))

    indexRange_block(as.list(index_range + toSignAndOffset$offset))
    
    toSignAndOffset$offset + index_range
}
