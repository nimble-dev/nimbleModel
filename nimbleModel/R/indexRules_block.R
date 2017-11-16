indexRuleClass_block <- R6Class(
    classname = "indexRuleClass_block",
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
                indexRule_block_setup(toIndexExprList,
                                      fromIndexExprList,
                                      context,
                                      constants
                                      )
        },
        applyOne = function(fromIndices) {
            indexRule_block_apply_single(
                fromIndices,
                setupResults
            )
        },
        apply = function(fromIndexRange) {
            indexRule_block_apply(
                fromIndexRange,
                setupResults
            )
        }
    )
)

indexRule_block_apply_single <- function(fromIndices,
                                         setupResults,
                                         make.matrix = TRUE) {
    if(fromIndices < setupResults$from_min |
       fromIndices > setupResults$from_max)
        return(matrix(nrow = 0, ncol = 1))
    toIndices <- fromIndices + setupResults$offset
    if(make.matrix) as.matrix(toIndices)
    else toIndices
}

indexRule_block_apply_matrix <- function(fromIndices,
                                         setupResults) {
    valid <-
        fromIndices >= setupResults$from_min &
        fromIndices <= setupResults$from_max
    if(sum(valid) == 0)
        return(matrix(nrow = 0, ncol = 1))
    toIndices <- fromIndices[valid] + setupResults$offset
    as.matrix(toIndices)
}

indexRule_block_apply_block <- function(fromIR,
                                        setupResults) {
    ## fromIR should be an indexRange
    toIR <-
        indexRange_block(
            lapply(fromIR[[1]],
                   indexRule_block_apply_single,
                   setupResults = setupResults,
                   make.matrix = FALSE)
        )
    toIR
}

indexRule_block_apply <- function(fromIR,
                                  setupResults) {
    ## fromIR should be an indexRange.
    ## This is essentially dispatching on types,
    ## which often a class hierarchy would manage.
    ## In this case it makes sense to do via switch().
    switch(attr(fromIR, "rangeType"),
           scalar = indexRange_scalar(
               indexRule_block_apply_single(fromIR[[1]],
                                            setupResults)
           ),
           block = indexRule_block_apply_block(fromIR,
                                               setupResults),
           matrix = indexRange_matrix(
               indexRule_block_apply_matrix(fromIR[[1]],
                                            setupResults)
           )
           )
}

## returns NULL if a block cannot be set up
indexRule_block_setup <- function(toIndexExprList,
                                  fromIndexExprList,
                                  context,
                                  constants = list()) {
    if(is.list(constants))
        constants <- list2env(constants)

    ##  only allow a single index slot 
    if(length(toIndexExprList) != 1 | length(fromIndexExprList) != 1)
        return(NULL)

    if(length(context$singleContexts) != 1)
        return(NULL)
    
    toIndexExpr <- toIndexExprList[[1]]
    fromIndexExpr <- fromIndexExprList[[1]]
    indexVarName <- context$indexVarNames[1]
    ##
    toSignAndOffset <- getSignAndOffset(toIndexExpr,
                                        indexVarName,
                                        constants)
    if(is.null(toSignAndOffset))
        return(NULL)

    ## ignore sign
    fromSignAndOffset <- getSignAndOffset(fromIndexExpr,
                                          indexVarName,
                                          constants)
    if(is.null(fromSignAndOffset))
        return(NULL)

    offset_from2to <-
        toSignAndOffset$offset - fromSignAndOffset$offset
        
    sign_from2to <- 1

    indexRangeExpr <- context$singleContexts[[1]]$indexRangeExpr

    ## We rely on eval here, but we could instead pick out
    ## arguments of `:`
    from_range <- range(eval(indexRangeExpr, envir = constants))
    
    list(offset_from2to = offset_from2to,
         sign_from2to = sign_from2to,
         from_min = from_range[1] + fromSignAndOffset$offset,
         from_max = from_range[2] + fromSignAndOffset$offset
        )
}

## placeholder functionalitiy:
## Look only for i +/- offset.
## I think we can use some old code to
## partially evaluate more complicated expressions.
## And/or we can try using Ryacas
getSignAndOffset <- function(indexExpr,
                             indexVarName,
                             constantsEnv = new.env()) {
    indexSign <- 1
    offset <- 0

    if(is.name(indexExpr)) {
        indexNameInExpr <- as.character(indexExpr)
    } else {
        indexNameInExpr <- as.character(indexExpr[[2]])
        offsetExpr <- indexExpr
        offsetExpr[[2]] <- 0
        offset <- eval(offsetExpr, envir = constantsEnv)
    }
    if(indexNameInExpr != indexVarName) return(NULL)
    list(sign = indexSign, offset = offset)
}
