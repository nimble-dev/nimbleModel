## This is a global object for the "blank" index in x[].
## We save an instance of it because coding it is very finicky
globalIndexBlank <- quote(x[])[3]
## Here is how to check for it:
## identical(globalIndexBlank, as.call(indexRange(quote(x[])[[3]])))
isBlank <- function(expr) {
    identical(globalIndexBlank, as.call(list(expr)))
}

## indexRange converts one index component into a
## list of information.  The component could be blank, scalar,
## an arbitrary vector, or a block indicated using ':'
##
## expr: a call containing one index component such as 1:5, 1, or blank
## return:
##   if it is a blank (from x[]): list()
##   if it is a single index: list(1)
##   if it is a sequence: list(list(1, 5))
##   if it is a matrix: list(<matrix>)
## In every case there is an extra list() layer that may
## be useful in the future for packing additional information.
## It seems useful to have the result be a list,
## even when it only has one element
indexRange <- function(expr) {
    ## if expr is simply a number,
    ## it will get treated as a scalar, not a 1-row matrix
    if(length(expr) > 1) {
        ## input expr is not just a name or number
        if(identical(expr[[1]], as.name(":")))
            ## index expr is a:b
            structure(list(as.list(expr[-1])),
                      class = "indexRange",
                      rangeType = "sequence")
        else
            ## index expr must be something like c(2, 4, 6)
            ## or matrix(...)
            ## An expression that returns a vector
            ## is assumed to be a set of 1D indices.
            ## An expression that returns a matrix is assumed to
            ## be a rows of indices (1D or higher-dimensional).
            ## Creating a single row of indices for nDim > 1
            ## requires an expression that returns a 1-row matrix.
            structure(list(as.matrix(eval(expr))),
                      class = "indexRange",
                      rangeType = "matrix")
            ## structure(list(eval(expr)),
            ##           class = "indexRange",
            ##           rangeType = "vector")

        ## Note: "vector" is the only type that might need
        ## to handle multiple dimensions.  Thus it might be a matrix,
        ## representing a vector of index vectors.
    } else {
        ## input expr is a single name, number, or blank
        
        ## Not clear we need blank indexRange type
        ## Not currently used internally and when would a user provide it in querying model structure?
        if(isBlank(expr))   
            structure(list(expr),
                      class = "indexRange",
                      rangeType = "blank")
        else {
            if(is.matrix(expr)) {
                structure(list(expr),
                          class = "indexRange",
                          rangeType = "matrix")
            } else {
                if(length(expr)) {
                    structure(list(expr),
                              class = "indexRange",
                              rangeType = "scalar")
                } else
                    indexRange_none()
            }
        }
    }
}

## kludge until we make indexRanges proper R6 classes.
indexRange_init <- function(indexRange, delay = 0) {
    indexRange$current <- 1
    indexRange$local <- 1
    indexRange$length <- indexRange_numRows(indexRange)
    indexRange$delay <- delay
    return(indexRange)
}

    
## Notes:
##
## We may need to distinguish vector from matrix
##

## "Nothing" type returned when result of applying a rule is empty.
indexRange_empty <- function() {
    structure(list(numeric(0)),
    class = "indexRange",
    rangeType = "empty")
}

## No indexing on a variable, e.g., 'y'.
indexRange_none <- function() {
    structure(list(numeric(0)),
    class = "indexRange",
    rangeType = "none")
}


indexRange_scalar <- function(rangeList) {
    structure(if(is.list(rangeList))
                  rangeList
              else
                  list(rangeList),
              class = "indexRange",
              rangeType = "scalar")
}

indexRange_sequence <- function(rangeList) {
    ## Need to check if rangeList is a list or nested list
    if(!is.list(rangeList))
        stop("rangeList must be a list for rangeType sequence")
    if(!is.list(rangeList[[1]]))
        rangeList <- list(rangeList)
    structure(rangeList,
              class = "indexRange",
              rangeType = "sequence")
}

indexRange_matrix <- function(rangeList) {
    structure(if(is.list(rangeList))
                  rangeList
              else
                  list(rangeList),
              class = "indexRange",
              rangeType = "matrix")
}

indexRange_matrixList <- function(rangeList) {
    structure(if(is.list(rangeList))
                  rangeList
              else
                  list(rangeList),
              class = "indexRange",
              rangeType = "matrixList")
}

indexRange_numCols <- function(inputIndexRange) {
   switch(attr(inputIndexRange, 'rangeType'),
           matrix = ncol(inputIndexRange[[1]])
          ,
           sequence = 1,
           scalar = 1,
           blank = 1,
           empty = 0,
           none = 0,
           stop("In inputRange_numCols: invalid type of inputIndexRange.")
          )
}

indexRange_numRows <- function(inputIndexRange,
                                 indices = NULL) {
    switch(attr(inputIndexRange, 'rangeType'),
           matrix = nrow(inputIndexRange[[1]])
          ,
           sequence = inputIndexRange[[1]][[2]] - inputIndexRange[[1]][[1]] + 1,
           scalar = 1,
           blank = NA,
           none = 0,
           stop("In inputRange_numRows: invalid type of inputIndexRange.")
           )
}

indexRange_getCols <- function(inputIndexRange,
                              indices = NULL) {
    switch(attr(inputIndexRange, 'rangeType'),
           matrix = 
               if(is.null(indices))
                   inputIndexRange
               else
                   indexRange_matrix(
                       list(inputIndexRange[[1]][, indices, drop = FALSE])
                   )
          ,
           sequence = inputIndexRange,
           scalar = inputIndexRange,
           blank = inputIndexRange,
           stop("In inputRange_getCols: invalid type of inputIndexRange.")
           )
}

indexRange2matrix <- function(inputIndexRange) {
    switch(attr(inputIndexRange, 'rangeType'),
           matrix = inputIndexRange,
           sequence = indexRange_matrix(
               list(matrix(seq.int(inputIndexRange[[1]][[1]],
                                   inputIndexRange[[1]][[2]])))
           ),
           scalar = indexRange_matrix(
               list(matrix(inputIndexRange[[1]]))
           ),
           matrixList = indexRange_matrix(
               do.call("rbind",
                       inputIndexRange)
           ),
           blank = stop("Can't convert from a blank indexRange to a matrix."),
           stop(paste0("Converting from ",
                       inputIndexRange$rangetype,
                       " to matrix is not supported."))
           )
}

indexRange_matrix2sequence <- function(inputIndexRange) {
    if(ncol(inputIndexRange[[1]]) > 1)
        return(inputIndexRange)
    mn <- min(inputIndexRange[[1]])
    mx <- max(inputIndexRange[[1]])
    ## Convert sequential indexing to sequence.
    if(length(inputIndexRange[[1]]) == mx - mn + 1 &&
       identical(as.numeric(diff(inputIndexRange[[1]][,1])),
                 rep(1, length(inputIndexRange[[1]]) - 1)))
       return(indexRange_sequence(list(mn,mx))) else return(inputIndexRange)
}

expandIndexRangeMatrices <- function(inputIndexRange) {
    switch(attr(inputIndexRange, 'rangeType'),
           matrix = stop('expandIndexRangeMatrices on a matrix indexRange not expected'),
           matrixList = inputIndexRange,
           sequence = lapply(seq.int(inputIndexRange[[1]][[1]],
                                  inputIndexRange[[1]][[2]]),
                          matrix)
           )
}

matrix_expand_grid <- function(...) {
    matrixList <- list(...)
    indexVectors <- lapply(matrixList, 
                           function(x) seq_len(nrow(x))
                           )
    indexGrid <- as.list(do.call("expand.grid",
                                 c(indexVectors,
                                   list(KEEP.OUT.ATTRS = FALSE))))
    unfoldedMatrices <- mapply(
        function(m, indices) m[indices,, drop = FALSE],
        matrixList,
        indexGrid,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    result <- do.call("cbind", unfoldedMatrices)
    result
}

indexRangeList2matrix <- function(indexRangeList) {
    ## For use in applyGraphRules for getMatrixIndexRange output to be consistent with output of
    ## getSingleIndexRange and for output to be consistent with indexRange2matrix,
    ## I think we want output to be an indexRange_matrix, not a matrix // CP
    indexRange_matrix(
        do.call("matrix_expand_grid",
            lapply(indexRangeList,
                   function(x) indexRange2matrix(x)[[1]]))
    )
}

collapse_indexRangeMatrices <- function(indexRangeMatrices) {
    expandedMatrices <- lapply(indexRangeMatrices,
                               expandIndexRangeMatrices)
    ## empty <- which(sapply(expandedMatrices, is.null))
    ## for(i in seq_along(empty))
    ##    expandedMatrices[[i]] <- indexRange_matrixList(matrix(0))
    if(length(unique(sapply(indexRangeMatrices, length))) > 1)
        warning("collapse_indexRangeMatrices: Inconsistent number of entries in components of indexRangeMatrices.")
    result <- indexRange_matrix(
        do.call("rbind",
                do.call("mapply", c(list(as.name("matrix_expand_grid")),
                                    expandedMatrices,
                                    list(SIMPLIFY = FALSE,
                                         USE.NAMES = FALSE)
                                    )
                        )
                )
    )
    return(result)
}

indexRange_getItem <- function(inputIndexRange) {
    item <- inputIndexRange$current

    ## Increment internal indexing
    if(inputIndexRange$local < inputIndexRange$delay) {
        inputIndexRange$local <- inputIndexRange$local + 1
    } else {
        inputIndexRange$local <- 1
        inputIndexRange$current <- inputIndexRange$current + 1
        if(inputIndexRange$current > inputIndexRange$length)
            inputIndexRange$current <- 1
    }

    ## Return original index values
    ## TODO remove kludge that has to return modified indexRange as well
    result <- switch(attr(inputIndexRange, 'rangeType'),
           matrix = inputIndexRange[[1]][item, ],
           sequence = inputIndexRange[[1]][[1]] + item - 1,
           scalar = inputIndexRange[[1]],
           stop("In inputRange_getItem: invalid type of inputIndexRange.")
           )
    return(list(result = result, range = inputIndexRange))
}

getRangeType <- function(IRL) {
    attr(IRL, 'rangeType')
}

indexRange_isSequence <- function(IRL) {
     attr(indexRange, 'rangeType') == "sequence"
}

indexRange_isBlank <- function(IRL) {
    attr(indexRange, 'rangeType') == "blank"
}

indexRange_isMatrix <- function(IRL) {
    attr(indexRange, 'rangeType') == "matrix"
}

indexRange_isScalar <- function(IRL) {
    attr(indexRange, 'rangeType') == "scalar"
}

## indexRange2expr is the inverse of indexRange
## Note that if the original expr was say c(2, 4, 6)
## then indexRange2expr( indexRange( quote(c(2, 4, 6))))
##  returns the *evaluated* vector, not the expression for the vector
indexRange2expr <- function(IRL) {
    ## length(IRL) > 2 should be impossible
    switch(getRangeType(IRL),
           sequence = substitute(A:B,
                              list(A = IRL[[1]][[1]],
                                   B = IRL[[1]][[2]])),
           matrix = IRL[[1]],
           blank = IRL[[1]],
           scalar = IRL[[1]],
           stop("Unknown indexRange list"))
}
