indexRuleClass_any <- R6Class(
    classname = "indexRuleClass_any",
    inherit = indexRuleClass,
    portable = FALSE,
    public = list(
        setupResults = NULL,
        initialize = function(toIndexExprList,
                              fromIndexExprList,
                              context,
                              constants = list()
                              ) {
            if(!length(context$singleContexts) || length(toIndexExprList)) {
                return()
            } else 
                setupResults <<-
                    indexRule_any_setup(fromIndexExprList,
                                          context,
                                          constants
                                          )
        },
        apply_one = function(fromIndices) {
            indexRule_any_apply_single(
                fromIndices,
                setupResults
            )
        },
        ## `...` allows arguments (like "collapse") from generic calls
        ## to be absorbed.
        ## apply_varRange = function(fromVarRange,
        ##                           indices,
        ##                           ...) {
        ##     thisIndexRange <- fromVarRange$getSingleIndexRange(indices)
        ##     indexRangeResult <- apply_indexRange(thisIndexRange)
        ##     result <- varRangeClass$new(
        ##         list(indexRangeResult)
        ##     )
        ##     result
        ## },
        apply_indexRange = function(fromIndexRange,
                                    ...) {
            indexRule_any_apply(
                fromIndexRange,
                setupResults,
                ...
            )
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

indexRule_any_apply_single <- function(fromIndices,
                                         setupResults,
                                         ...) {

    if(setupResults$useArbitrary) {
        from_flat <- setupResults$from2indicesFunctions$rawIndex2flatIndex(fromIndices)
        if(!length(from_flat) || is.null(setupResults$from_flat2iRow[from_flat][[1]]))
            return(FALSE) else return(TRUE)
    } else {
        if(fromIndices < setupResults$from_min ||
           fromIndices > setupResults$from_max)
            return(FALSE) else return(TRUE)
    }
}

indexRule_any_apply_matrix <- function(fromIndices,
                                       setupResults,
                                       ...) {
    if(setupResults$useArbitrary) {
        from_flat <- setupResults$from2indicesFunctions$rawIndex2flatIndex_multi(fromIndices)
        return(sapply(from_flat, function(x) length(x) && !is.null(setupResults$from_flat2iRow[x][[1]])))
    } else {
        warning("Treating matrix indexRange as arbitrary case even if only a single column.")
        valid <-
            fromIndices >= setupResults$from_min &
            fromIndices <= setupResults$from_max
        return(valid)
    }
}

indexRule_any_apply_sequence <- function(fromIR,
                                        setupResults,
                                        ...) {
    ## fromIR should be an indexRange
    start <- fromIR[[1]][[1]]
    end <- fromIR[[1]][[2]]
    if(setupResults$useArbitrary) {
        from_flat <- setupResults$from2indicesFunctions$rawIndex2flatIndex_multi(matrix(start:end, ncol = 1))
        return(any(sapply(from_flat, function(x) length(x) &&
                                                 !is.null(setupResults$from_flat2iRow[x][[1]]))))
    } else {
        if(start > end |
           start > setupResults$from_max |
           end < setupResults$from_min)
            return(FALSE) else return(TRUE)
    }
}

indexRule_any_apply <- function(fromIR,
                                  setupResults,
                                  ...) {
    ## fromIR should be an indexRange.
    ## This is essentially dispatching on types,
    ## which often a class hierarchy would manage.
    ## In this case it makes sense to do via switch().
    switch(attr(fromIR, "rangeType"),
           scalar = indexRule_any_apply_single(fromIR[[1]],
                                            setupResults,
                                            ...
                                            ),
           sequence = indexRule_any_apply_sequence(fromIR,
                                               setupResults,
                                               ...),
           matrix = indexRule_any_apply_matrix(fromIR[[1]],
                                                      setupResults,
                                                      ...)
           )
}

indexRule_any_setup <- function(fromIndexExprList,
                                  context,
                                  constants = list()) {
    ans <- try(
        indexRule_any_setup_sequence_internal(fromIndexExprList,
                                       context,
                                       constants),
        silent = TRUE
    )
    if(is.null(ans) || inherits(ans, 'try-error'))
        ans <- indexRule_any_setup_arbitrary_internal(fromIndexExprList,
                                               context,
                                               constants)
    ans
}

indexRule_any_setup_sequence_internal <- function(fromIndexExprList,
                                           context,
                                         constants = list()) {
    if(is.list(constants))
        constants <- list2env(constants)

    ##  only allow a single index slot 
    if(length(fromIndexExprList) != 1 || length(context$singleContexts) != 1)
        return(NULL)
    
    fromIndexExpr <- fromIndexExprList[[1]]
    ## I'm not sure this will always need 1st indexVarName.
    ## It might need a different element.
    indexVarName <- context$indexVarNames[1]
    
    ## ignore sign
    fromSignAndOffset <- getSignAndOffset(fromIndexExpr,
                                          indexVarName,
                                          constants)
    offset <-
        fromSignAndOffset$offset
    
    sign <- 1
    
    indexRangeExpr <- context$singleContexts[[1]]$indexRangeExpr
    
    ## We rely on eval here, but we could instead pick out
    ## arguments of `:`
    ## from_range <- 
    ##    range(eval(indexRangeExpr, envir = constants))
    from_range <- 
        c(eval(indexRangeExpr[[2]], envir = constants),
          eval(indexRangeExpr[[3]], envir = constants))
    
    return(list(offset = offset,
                sign = sign,
                from_min = from_range[1] + fromSignAndOffset$offset,
                from_max = from_range[2] + fromSignAndOffset$offset,
                useArbitrary = FALSE
                ))
}

indexRule_any_setup_arbitrary_internal <- function(fromIndexExprList,
                                           context,
                                         constants = list()) {
    if(is.list(constants))
        constants <- list2env(constants)

    allReplacements <- fromIndexExprList
    fromIndexNames <- lapply(names(fromIndexExprList),
                             as.name)
    allIndexNames <- fromIndexNames
    
    unrolledIndicesEnv <-
        expandContextAndReplacements(
            allReplacements = allReplacements,
            allReplacementNameExpr = allIndexNames,
            context = context,
            constantsEnv = constants
        )
    
    fromUnrolledResults <-
        lapply(names(fromIndexExprList),
               function(x) unrolledIndicesEnv[[x]])
    unrolledSize <- unrolledIndicesEnv$outputSize

    ## Helper function to determine if results are scalar or not
    isScalarIndex <- function(unrolledResult) {
        !is.list(unrolledResult)
    }
    from_allScalar <- all(unlist(
        lapply(fromUnrolledResults,
               isScalarIndex)))
    
    makeInfo <-
        function(indexName) {
            indexValues <- unlist(unrolledIndicesEnv[[indexName]])
            frange <- range(indexValues)
            foffset <- frange[1]-1
            fsize <- diff(frange)+1
            list(offset = foffset,
                 size = fsize)
        }
    
    fromInfo <-
        lapply(names(fromIndexExprList),
               makeInfo)
    from_flatMax <- prod(unlist(lapply(fromInfo, `[[`, 'size')))
    
    
    make2IndicesFunctions <- function(info) {
        sizes <- unlist(lapply(info, `[[`, 'size'))
        totSize <- prod(sizes)
        strides <- c(1, cumprod(sizes[-length(sizes)]))
        offsets <- unlist(lapply(info, `[[`, 'offset'))
        convertSingle <- function(F) {
            shiftedF <- F-offsets
            valid <- all(shiftedF >= 1 & shiftedF <= sizes)
            if(!valid) {
                matrix(data = numeric(),
                       nrow = 0, ncol = length(sizes))
            } else
                1 + (sum((shiftedF - 1) * strides))
        }
        invertSingle <- function(flat) {
            flat <- flat - 1
            shiftedF <- integer()
            for(i in rev(seq_along(strides))) {
                if(i == 1)
                    shiftedF <- c(flat + 1, shiftedF)
                else {
                    thisIndex <- flat %/% strides[i]
                    shiftedF <- c(thisIndex + 1, shiftedF)
                    flat <- flat - thisIndex*strides[i]
                }
            }
            shiftedF + offsets
        }
        convertMany <- function(F) {
            apply(F, 1, convertSingle)
        }
        list(rawIndex2flatIndex = convertSingle,
             rawIndex2flatIndex_multi = convertMany,
             flatIndex2rawIndex = invertSingle)
    }
    from2indicesFunctions <- make2IndicesFunctions(fromInfo)

    if(from_allScalar) {
        ## Case of all scalar indices can be handled more efficiently
        ## fromNamesList <-
        ##     as.call(c(list(quote(list)),
        ##               fromIndexNames))
        ## allIndices <- eval(substitute(
        ##     with(unrolledIndicesEnv,
        ##          do.call("cbind", fromNamesList)),
        ##     list(fromNamesList = fromNamesList)))
        allIndices <- do.call("cbind",
                              fromUnrolledResults)
        from_flat <- from2indicesFunctions$rawIndex2flatIndex_multi(allIndices)
        ## split() works when there is no raggedness to the declarations
        ## from_flat2iRow <- split(1:unrolledSize, from_flat)
        ## The following is slower than split() but more general.
        ## Eventually this will be in C++ anyway.
        from_flat2iRow <- vector('list', length = from_flatMax)
        for(i in 1:unrolledSize) {
            from_flat2iRow[[from_flat[i]]] <-
                c(from_flat2iRow[[from_flat[i]]], i)
        }
    } else {
        ## Case with some vector indices require more care and
        ## will be less efficient.
        ## argList <- c(list(as.name("expand.grid")),
        ##              fromIndexNames,
        ##              list(SIMPLIFY = FALSE))
        ## allIndicesList <- eval(substitute(
        ##     with(unrolledIndicesEnv,
        ##          do.call("mapply", argList)),
        ##     list(argList = argList)))
        allIndicesList <- do.call("mapply",
                                  c(list(as.name("expand.grid")),
                                    fromUnrolledResults,
                                    list(SIMPLIFY = FALSE))
                                  )
        iRows <- rep(1:unrolledSize,
                     times = unlist(lapply(allIndicesList, nrow))
                     )
        allIndices <- do.call("rbind", allIndicesList)
        from_flat <- from2indicesFunctions$rawIndex2flatIndex_multi(allIndices)
        ## Again, split() would work if there is no raggedness in the
        ## the loop ranges.
        ##from_flat2iRow <- split(iRows, from_flat)

        from_flat2iRow <- vector('list', length = from_flatMax)
        for(i in seq_along(iRows)) {
            from_flat2iRow[[from_flat[i]]] <-
                c(from_flat2iRow[[from_flat[i]]], iRows[i])
        }
    }

    return(list(from2indicesFunctions = from2indicesFunctions,
                from_flat2iRow = from_flat2iRow,
                useArbitrary = TRUE))
    
}



