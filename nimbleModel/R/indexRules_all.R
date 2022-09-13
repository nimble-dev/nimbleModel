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
            if(length(fromIndexExprList) || !length(toIndexExprList) ||
               !length(context$singleContexts)) {
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
            ## Presumably checking of from indexing should be done in graphRule processing
            ## not here.
            ## if(!is.null(fromIndexRange))
            ##    stop("Input to constant indexRule is not NULL.")
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

    ## Try to handle single index, simple increment case expeditiously.
    if(length(toIndexExprList) == 1) {
        toIndexExpr <- toIndexExprList[[1]]
        
        ## May need to generalize which indexVarName is used:
        indexVarName <- context$indexVarNames[1]
        ## May need to fail cleanly if RHS is like x[k[i]] or other complication.
        
        toSignAndOffset <- try(getSignAndOffset(toIndexExpr,
                                            indexVarName,
                                            constants),
                               silent = TRUE)
        ## TODO: when does that error out vs. returning NULL?
        if(!is.null(toSignAndOffset) && !inherits(toSignAndOffset, 'try-error')) {
            indexRangeExpr <- context$singleContexts[[1]]$indexRangeExpr
            index_range <- 
                c(eval(indexRangeExpr[[2]], envir = constants),
                  eval(indexRangeExpr[[3]], envir = constants))
            
            return(list(all = indexRange_sequence(as.list(toSignAndOffset$offset + index_range))))
        }
    }
    ## Otherwise, do unrolling and handle like arbitrary case, but without 'from' information
    toIndexNames <- lapply(names(toIndexExprList),
                           as.name)
    ## Run the for loops in an environment
    ## where all the results are created.
    unrolledIndicesEnv <-
        expandContextAndReplacements(
            allReplacements = toIndexExprList,
            allReplacementNameExpr = toIndexNames,
            context = context,
            constantsEnv = constants
        )
    unrolledResults <-
        lapply(names(toIndexExprList),
               function(x) unrolledIndicesEnv[[x]])

    if(any(is.na(unlist(unrolledResults))))
        stop("Missing values found in setting up indexRule: are constants the correct size?")
    
    unrolledSize <- unrolledIndicesEnv$outputSize
    
    if(length(unrolledResults) > 1) {
        allIndices <- do.call("cbind",
                              unrolledResults)
    } else allIndices <- matrix(unrolledResults[[1]], nrow = unrolledSize)
    
    return(list(all = indexRange(allIndices)))
}


