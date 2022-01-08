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
                      rangeType = "block")
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
                structure(list(expr),
                          class = "indexRange",
                          rangeType = "scalar")
            }
        }
    }
}

## Notes:
##
## We may need to distinguish vector from matrix
##

## "Nothing" type returned when result of applying a rule is empty.
## Could also just define as NULL.
indexRange_empty <- function() {
    structure(list(numeric(0)),
    class = "indexRange",
    rangeType = "empty")
}

indexRange_scalar <- function(rangeList) {
    structure(if(is.list(rangeList))
                  rangeList
              else
                  list(rangeList),
              class = "indexRange",
              rangeType = "scalar")
}

indexRange_block <- function(rangeList) {
    ## Need to check if rangeList is a list or nested list
    if(!is.list(rangeList))
        stop("rangeList must be a list for rangeType block")
    if(!is.list(rangeList[[1]]))
        rangeList <- list(rangeList)
    structure(rangeList,
              class = "indexRange",
              rangeType = "block")
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
           block = 1,
           scalar = 1,
           blank = 1,
           empty = 0,
           stop("In inputRange_numCols: invalid type of inputIndexRange.")
          )
}

indexRange_numRows <- function(inputIndexRange,
                                 indices = NULL) {
    switch(attr(inputIndexRange, 'rangeType'),
           matrix = nrow(inputIndexRange[[1]])
          ,
           block = inputIndexRange[[1]][[2]] - inputIndexRange[[1]][[1]] + 1,
           scalar = 1,
           blank = NA,
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
           block = inputIndexRange,
           scalar = inputIndexRange,
           blank = inputIndexRange,
           stop("In inputRange_getCols: invalid type of inputIndexRange.")
           )
}

indexRange2matrix <- function(inputIndexRange) {
    switch(attr(inputIndexRange, 'rangeType'),
           matrix = inputIndexRange,
           block = indexRange_matrix(
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

expandIndexRangeMatrices <- function(inputIndexRange) {
    switch(attr(inputIndexRange, 'rangeType'),
           matrix = stop('expandIndexRangeMatrices on a matrix indexRange not expected'),
           matrixList = inputIndexRange,
           block = lapply(seq.int(inputIndexRange[[1]][[1]],
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
    do.call("matrix_expand_grid",
            lapply(indexRangeList,
                   function(x) indexRange2matrix(x)[[1]]))
}

collapse_indexRangeMatrices <- function(indexRangeMatrices) {
    expandedMatrices <- lapply(indexRangeMatrices,
                               expandIndexRangeMatrices)
    ## empty <- which(sapply(expandedMatrices, is.null))
    ## for(i in seq_along(empty))
    ##    expandedMatrices[[i]] <- indexRange_matrixList(matrix(0))
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

getRangeType <- function(IRL) {
    attr(IRL, 'rangeType')
}

indexRange_isBlock <- function(IRL) {
     attr(indexRange, 'rangeType') == "block"
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
           block = substitute(A:B,
                              list(A = IRL[[1]][[1]],
                                   B = IRL[[1]][[2]])),
           matrix = IRL[[1]],
           blank = IRL[[1]],
           scalar = IRL[[1]],
           stop("Unknown indexRange list"))
}
