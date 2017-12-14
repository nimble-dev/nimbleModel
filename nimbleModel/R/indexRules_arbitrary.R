
indexRuleClass_arbitrary <- R6Class(
    classname = "indexRuleClass_arbitrary",
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
                indexRule_arbitrary_setup(toIndexExprList,
                                              fromIndexExprList,
                                              context,
                                              constants
                                              )
        },
        applyOne = function(fromIndices) {
            indexRule_arbitrary_apply_single(
                fromIndices,
                setupResults
            )
        },
        ## Should conversion from mulitple ranges be done before
        ## calling this, or as part of this?
        ## apply_varRange = function(fromVarRange,
        ##                           indices,
        ##                           collapse = TRUE) {
        ##     thisIndexRange <- fromVarRange$getIndexRangeMatrix(indices)
        ##     toIndices <-
        ##         indexRule_arbitrary_apply_matrix(
        ##             ##fromIndicesMatrix,
        ##             thisIndexRange,
        ##             setupResults,
        ##             collapse = collapse
        ##         )
        ##     result <- varRangeClass$new(
        ##         list(indexRange_matrix(toIndices))
        ##     )
        ##     result
        ## },
        apply_indexRange = function(fromIndexRange,
                                    collapse = TRUE) {
            fromIndicesMatrix <- indexRange2matrix(fromIndexRange)[[1]]
            toIndices <-
                indexRule_arbitrary_apply_matrix(
                    fromIndicesMatrix,
                    ##fromIndexRange,
                    setupResults,
                    collapse = collapse
                )
            toIndices <- if(collapse)
                             indexRange_matrix(toIndices)
                         else
                             indexRange_matrixList(toIndices)
            toIndices
        },
        apply = function(from, ...) {
            if(inherits(from, 'varRangeClass'))
                ##apply_varRange(from, ...)
                stop('an indexRule should be applied to an indexRange')
            else
                apply_indexRange(from)
        }
    )
)

indexRule_arbitrary_setup <- function(toIndexExprList,
                                      fromIndexExprList,
                                      context,
                                      constants = list()
                                      ) {
    if(is.list(constants))
        constants <- list2env(constants)
    allReplacements <- c(toIndexExprList,
                         fromIndexExprList)
    toIndexNames <- lapply(names(toIndexExprList),
                           as.name)
    fromIndexNames <- lapply(names(fromIndexExprList),
                             as.name)
    allIndexNames <- c(toIndexNames, fromIndexNames)

    ## Run the for loops in an environment
    ## where all the results are created.
    unrolledIndicesEnv <-
        expandContextAndReplacements(
            allReplacements = allReplacements,
            allReplacementNameExpr = allIndexNames,
            context = context,
            constantsEnv = constants
        )

    ## Extract the results from the environment.
    toUnrolledResults <-
        lapply(names(toIndexExprList),
               function(x) unrolledIndicesEnv[[x]])
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
    to_allScalar <- all(unlist(
        lapply(toUnrolledResults,
               isScalarIndex)))
    
    ## A helper function to extract range, offset and size information
    ## from a set of index results.  Note these do not need to be
    ## relevant for the entire variable in the model.  They only
    ## need to be relevant for the block of the variable touched
    ## in this declaration with its context.
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

    toInfo <-
        lapply(names(toIndexExprList),
               makeInfo)
    ## not needed:
    ## to_flatMax <- prod(unlist(lapply(toInfo, `[[`, 'size')))

    ## A helper function that returns functions for
    ## converting from a set of "real" indices to a
    ## flat index defined for the part of the variable used
    ## in the declaration with its context.
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
    to2indicesFunctions <- make2IndicesFunctions(toInfo)

    ## Set up from_flat2iRow
    ## iRow refers to the "row" of the unrolledIndicesEnv
    ## It is a cumulative index of all for-loop unrolling.
    ## from_flat2iRow will be a list.  The ith element of the
    ## list gives the iRow values for which flat index i of the
    ## "from" variable is involved.
    ## Example:
    ## for(i in 1:5)
    ##   for(j in 1:3)
    ##      y[i, j] <- foo(x[j+1, i+2]
    ##
    ## The flat indices for x will be for x[2:4, 3:7],
    ## starting at x[2, 3].
    ## So x[2,5] will have flat index 7, e.g.
    ## from_flat2iRow[[7]] will have 11, because the 11th iteration
    ## of the nested for loops will touch x[2,5].
    ## There can be multiple iterations that touch x[2,5] if one
    ## of the indices is sub-indexed.
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

    ## set up iRow2toIndices:
    ## iRow2toIndices is a list whose ith element
    ## gives the indices (as a matrix) of the "to" variable
    ## touched by unrolled iRow=i.
    ##
    ## Example:
    ##    for(i in 1:5)
    ##     x[i+1, 1:3] <- foo(y[i])
    ##
    ## iRow2toIndices[[2]] will be the matrix
    ##   3 1
    ##   3 2
    ##   3 3
    ## because the 2nd iteration of the loop unrolling
    ## creates x[3, 1:3].
    ## N.B. (We aim for this example to be handled more efficiently,
    ##   by a separate index inverter, but the current one should
    ##   handle any case.)
    if(to_allScalar) {
        ## toNamesList <- as.call(c(list(quote(list)),
        ##                          toIndexNames))
        ## allIndices <- eval(substitute(
        ##     with(unrolledIndicesEnv,
        ##          do.call("cbind", toNamesList)),
        ##     list(toNamesList = toNamesList)))

        allIndices <- do.call("cbind",
                              toUnrolledResults)
        
        iRow2toIndices <- split(allIndices,
                                1:unrolledSize) ## makes it a list
    } else {
        ## mapply(expand.grid,...)
        ## will return a list in the right form.
        allIndicesList <- do.call("mapply",
                                  c(list(as.name("expand.grid")),
                                    toUnrolledResults,
                                    list(SIMPLIFY = FALSE))
                                  )
        iRows <- rep(1:unrolledSize,
                     times = unlist(lapply(allIndicesList, nrow)))
        allIndices <- do.call("rbind", allIndicesList)
        iRow2toIndices <- split(allIndices,
                                iRows)
    }

    names(iRow2toIndices) <- NULL
    
    list(from2indicesFunctions = from2indicesFunctions,
         to2indicesFunctions = to2indicesFunctions,
         from_flat2iRow = from_flat2iRow,
         iRow2toIndices = iRow2toIndices,
         fromInfo = fromInfo,
         toInfo = toInfo,
         unrolledSize = unrolledSize,
         from_flatMax = from_flatMax
         )
}

indexRule_arbitrary_apply_single <- function(fromIndices,
                                             setupResults) {
    with(setupResults, {
        from_flat <- from2indicesFunctions$rawIndex2flatIndex(fromIndices)
        iRows <- unlist(from_flat2iRow[from_flat])
### unique???
        toIndices <<- do.call("rbind", iRow2toIndices[iRows])
    })
    if(is.null(toIndices))
        matrix(data = numeric(),
               nrow = 0, ncol = length(setupResults$toInfo))
    else
        as.matrix(toIndices)
}

indexRule_arbitrary_apply_matrix <- function(fromIndices,
                                             setupResults,
                                             collapse = TRUE) {
    with(setupResults, {
        ## from_flat is the flat index of each row of "from" indices
        from_flat <-
            unlist(
                from2indicesFunctions$rawIndex2flatIndex_multi(fromIndices)
            )
        ## iRowsList has the declaration iRows for each from_flat
        iRowsList <- from_flat2iRow[from_flat]
### unique???
        ## toIndicesList has the matrix of "to" indices for each from_flat 
        toIndicesList <<- lapply(iRowsList,
                                 function(x)
                                     do.call('rbind',
                                             iRow2toIndices[x])
                                 )
    })
    if(collapse) {
        if(length(toIndicesList) > 0)
            toIndicesList <-
                toIndicesList[!unlist(lapply(toIndicesList, is.null))]
        if(length(toIndicesList) == 0)
            matrix(data = numeric(),
                   nrow = 0, ncol = length(setupResults$toInfo))
        else
            as.matrix(do.call('rbind', toIndicesList))
    }
    else
        toIndicesList
}
