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
        constantAnswer = NULL,
        initialize = function(toIndexExprList,
                              fromIndexExprList,
                              context,
                              constants = list()
                              ) {
            ## Need check that RHS indexing is constant if present
            ## and then return NULL if not the case
            if(FALSE) {
                setupResults <<- NULL
            } else {
                setupResults <<-
                    indexRule_all_setup(toIndexExprList,
                                        fromIndexExprList,
                                        context,
                                        constants
                                        )
            }
        },
        apply_one = function(fromIndices) {
            indexRule_all_apply_single(
                fromIndices,
                setupResults
            )
        },
        apply_indexRange = function(fromIndexRange,
                                    ...) {
            indexRule_all_apply_block(
                fromIndexRange,
                setupResults,
                ...
            )
        },
        apply = function(from, ...) {
            if(inherits(from, 'varRangeClass'))
                ##apply_varRange(from, ...)
                stop('an index rule should be applied to an indexRange')
            else {
                if(is(from, 'indexRange')) 
                    apply_indexRange(from, ...)
                else apply_one(from)
            }
        }        
    )
)

indexRule_all_apply_single <- function(fromIndices,
                                         setupResults) {
    if(!is.null(fromIndices) && (fromIndices < setupResults$from_min ||
       fromIndices > setupResults$from_max))
        return(matrix(data = numeric(), nrow = 0, ncol = 1))
    return(setupResults$all)
}


indexRule_all_apply_block <- function(fromIR,
                                        setupResults,
                                        collapse = TRUE,
                                      ...) {
    start <- fromIR[[1]][[1]]
    end <- fromIR[[1]][[2]]
    if(start > setupResults$from_max || end < setupResults$from_min)
        return(matrix(data = numeric(), nrow = 0, ncol = 1))
    return(setupResults$all)
}


indexRule_all_setup <- function(toIndexExprList,
                                fromIndexExprList,
                                context,
                                constants = list()) {
    toIndexExpr <- toIndexExprList[[1]]
    if(length(fromIndexExprList))
        fromIndexExpr <- fromIndexExprList[[1]] else fromIndexExpr <- NULL
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

    ## Need to figure out when we would know full extent of the variable dimension
    ## and how to access that information.
    if(is.null(fromIndexExpr)) {
        warning("Not yet checking extent of variable dimension in blank case.")
        from_range <- c(1, Inf)
    } else {
        from_range <- eval(fromIndexExpr, envir = constants)
        if(length(from_range) == 1)
            from_range <- rep(from_range, 2)
    }
    
    list(from_min = from_range[1],
         from_max = from_range[2],
        all = indexRange_block(as.list(toSignAndOffset$offset + index_range)))
}
