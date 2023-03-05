## This rule handles simple offset translations, such as
## `y[i] <- x[i+2]`.
indexRuleBlockClass <- R6Class(
    classname = "indexRuleBlockClass",
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
                indexRuleBlock_setup(toIndexExprList,
                                     fromIndexExprList,
                                     context,
                                     constants
                                     )
        },
        
        apply = function(indexRange, collapse = TRUE) {
            ## A bit awkward to use `switch` but otherwise hard to dispatch on input type,
            ## given we need to cross the indexRule type with the input indexRange type.
            switch(class(indexRange)[1],
                   indexRangeScalarClass = indexRuleBlock_applyToScalar(indexRange$value,
                                                                        setupResults),
                   indexRangeSequenceClass = indexRuleBlock_applyToSequence(indexRange$start, indexRange$end,
                                                                            setupResults),
                   indexRangeMatrixClass = indexRuleBlock_applyToMatrix(indexRange$values,
                                                                        setupResults,
                                                                        collapse = collapse),
                   stop('indexRuleBlockClass$apply: an index rule must be applied to an `indexRange`.')
                   )
        },
        
        getMax = function() {
            return(setupResults$fromMax)
        }
    )
)

indexRuleBlock_setup <- function(toIndexExprList,
                                 fromIndexExprList,
                                 context,
                                 constants = list()) {

    ## Only valid for single index slot in 'from' and 'to'.
    if(length(toIndexExprList) != 1 || length(fromIndexExprList) != 1 ||
       length(context$singleContexts) != 1)
        return(NULL)

    if(is.list(constants))
        constants <- list2env(constants)
    toIndexExpr <- toIndexExprList[[1]]
    fromIndexExpr <- fromIndexExprList[[1]]
    indexVarName <- context$indexVarNames[1]

    toOffset <- getOffset(toIndexExpr, indexVarName, constants)
    fromOffset <- getOffset(fromIndexExpr, indexVarName, constants)
    
    if(is.null(toOffset) || is.null(fromOffset))
        return(NULL)

    offset <- toOffset$offset - fromOffset$offset

    indexRangeExpr <- context$singleContexts[[1]]$indexRangeExpr

    ## We rely on eval here, but we could instead pick out arguments of `:`
    ## fromRange <- range(eval(indexRangeExpr, envir = constants))
    if(indexRangeExpr[[1]] != ":")
        return(NULL)
    fromRange <- 
        c(eval(indexRangeExpr[[2]], envir = constants),
          eval(indexRangeExpr[[3]], envir = constants))
    
    return(
        list(offset = offset,
             fromMin = fromRange[1] + fromOffset$offset,
             fromMax = fromRange[2] + fromOffset$offset
             )
    )
}


indexRuleBlock_applyToScalar <- function(fromValue,
                                       setupResults) {
    if(fromValue < setupResults$fromMin || fromValue > setupResults$fromMax)
        return(indexRangeEmptyClass$new())
    toValue <- fromValue + setupResults$offset
    return(indexRangeScalarClass$new(toValue))
}

indexRuleBlock_applyToMatrix <- function(fromValues,
                                         setupResults,
                                         collapse = TRUE) {
    if(ncol(fromValues) != 1)
        stop("indexRuleBlock_applyMatrix: a block rule can only be applied to a one-column indexRangeMatrix.")
    valid <-
        fromValues >= setupResults$fromMin &
        fromValues <= setupResults$fromMax
    toValues <- fromValues + setupResults$offset

    ## CHECK: Presumably NAs needed to preserve input length when combining results later.
    toValues[!valid] <- NA

    ## `applyGraphRules` will use `collapse = FALSE` because
    ## one could have something like `y[i,j] <- x[k[i],j]` applied to an input
    ## indexRangeMatrix with two columns. In that case the `i` rule can
    ## produce variable number of outputs for each input index, and these
    ## need to be crossed with the output of the `j` rule, and it's easiest to
    ## do that if the output of the `j` rule is also a list.
    ## CHECK: check this reasoning
    if(collapse)  
        return(indexRangeMatrixClass$new(toValues))
    else  
        return(indexRangeMatrixListClass$new(lapply(toValues, as.matrix)))
}

indexRuleBlock_applyToSequence <- function(fromStart, fromEnd,
                                        setupResults) {
    if(fromStart > fromEnd ||
       fromStart > setupResults$fromMax ||
       fromEnd < setupResults$fromMin)
        return(indexRangeEmptyClass$new())

    toStart <- if(fromStart < setupResults$fromMin)
                   setupResults$fromMin + setupResults$offset
               else
                   fromStart + setupResults$offset
    
    toEnd <- if(fromEnd > setupResults$fromMax)
                 setupResults$fromMax + setupResults$offset
             else
                 fromEnd + setupResults$offset
    
    if(toStart == toEnd) {
        return(indexRangeScalarClass$new(toStart))
    }
    
    return(indexRangeSequenceClass$new(toStart, toEnd))
}

