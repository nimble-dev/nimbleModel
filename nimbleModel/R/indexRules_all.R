## This is for a case like y[i] <- foo(x) or y[i] <- foo(x[])
## or y[i] <- x[2].
## It is possible x is non-scalar, but the relationships
## does not use any indices in x.
indexRuleClass_all <- R6Class(
    classname = "indexRuleClass_all",
    inherit = indexRuleClass,
    portable = FALSE,
    public = list(
        setupResults = NULL,
        initialize = function(toIndexExprList,
                              fromIndexExprList,
                              context,
                              constants = list()
                              ) {
            ## Rule only applicable if no RHS indexing.
            if(length(fromIndexExprList) != 0) {
                setupResults <<- NULL
                return()
            } else 
                setupResults <<-
                    indexRule_all_setup(toIndexExprList,
                                        fromIndexExprList,
                                        context,
                                        constants
                                        )
        },
        apply_indexRange = function(fromIndexRange,
                                    ...) {
            if(!is.null(fromIndexRange))
                stop("Input to constant indexRule is not NULL.")
            return(setupResults$all)
        },
        apply = function(from, ...) {
            if(inherits(from, 'varRangeClass'))
                ##apply_varRange(from, ...)
                stop('an index rule should be applied to an indexRange')
            else
                apply_indexRange(from, ...)
         }        
    )
)

indexRule_all_setup <- function(toIndexExprList,
                                fromIndexExprList,
                                context,
                                constants = list()) {
    if(is.list(constants))
        constants <- list2env(constants)

    ##  only allow a single index slot 
    if(length(toIndexExprList) != 1)
        return(NULL)

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

    list(all = indexRange_block(as.list(toSignAndOffset$offset + index_range)))
}
