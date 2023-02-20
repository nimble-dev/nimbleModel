## This rule handles cases where all of the `to` elements are returned, because
## they are not tied to the `from` elements.
## E.g., `y[i] <- x[2]`, `y[i] <- sum(x[c(2,3,5)])`.
indexRuleAllClass <- R6Class(
    classname = "indexRuleAllClass",
    inherit = indexRuleClass,
    portable = FALSE,
    public = list(
        setupResults = NULL,
        initialize = function(toIndexExprList,
                              fromIndexExprList,
                              context,
                              constants = list()
                              ) {
            setupResults <<-
                indexRuleAll_setup(toIndexExprList,
                                    fromIndexExprList,
                                    context,
                                    constants
                                    )
        },
        
        apply = function(indexRange) {
            return(setupResults$all)
        },

        getMax = function() {
            ## Index is being dropped, so any index will be fine.
            return(1)
        }
    )
)

indexRuleAll_setup <- function(toIndexExprList,
                               fromIndexExprList,
                               context,
                               constants = list()) {
    ## Only valid if no `from` indexing using the `to` index.
    if(length(fromIndexExprList) || !length(toIndexExprList) ||
       !length(context$singleContexts)) 
        return(NULL)
    
    if(is.list(constants))
        constants <- list2env(constants)
    
    ## Try to handle single index, simple increment case expeditiously.
    if(length(toIndexExprList) == 1) {
        toIndexExpr <- toIndexExprList[[1]]
        
        indexVarName <- context$indexVarNames[1]
        
        toOffset <- getOffset(toIndexExpr, indexVarName,  constants)
        if(!is.null(toOffset)) {
            indexRangeExpr <- context$singleContexts[[1]]$indexRangeExpr
            indexingRange <- 
                c(eval(indexRangeExpr[[2]], envir = constants),
                  eval(indexRangeExpr[[3]], envir = constants))
            
            return(list(all = indexRangeSequenceClass$new(indexingRange[1] + toOffset$offset,
                                                          indexingRange[2] + toOffset$offset)))
        }
    }
        
    ## Otherwise, do unrolling and handle like arbitrary case, but without 'from' information.
    toIndexNames <- lapply(names(toIndexExprList), as.name)
        
    ## Run the for loops in an environment where all the results are created.
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
        stop("indexRuleAll_setup: Missing values found in setting up rule. Check if `constants` are the correct size?")
    
    unrolledSize <- unrolledIndicesEnv$outputSize
    
    if(length(unrolledResults) > 1) {
        allIndices <- do.call("cbind", unrolledResults)
    } else allIndices <- matrix(unrolledResults[[1]], nrow = unrolledSize)
    
    return(list(all = indexRangeMatrixClass$new(allIndices)))
}


