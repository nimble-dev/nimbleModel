## CHECK: do we want to handle blank cases: y[], y[,].

## An indexRange stores the indices for one or more index slots
## (positions) of a variable.
## When multiple slots are included, the indexRange type is necessarily
## an `indexRangeMatrix`, which keeps the index values that are tied together
## as individual rows.

## Indices that can be crossed (e.g. `y[1:3, 1:4]` has all pairs of first
## and second indices), will generally be stored as two single index slot
## indexRanges (two `indexRangeSequence`s in this case).

## NOTE: this uses subclass-specific variables for storing index information.
## Previously we did this with a list for each kind of `indexRange`.

intToNumeric <- function(x) {
    if(is.integer(x)) {
        dm <- dim(x)
        x <- as.numeric(x)
        dim(x) <- dm
    }
    return(x)
}

##### `indexRange` constructor #####

## Function to create `indexRange` objects of particular subclasses.
## This needs to be a function as we can't initialize a subclass from
## the constructor of the base class, and we want to be able to
## dispatch `indexRange` creation based on form of input expression or value.
newIndexRange <- function(expr) {
    ## Note that we do various checking and  conversion to numeric here,
    ## as calls to class-specific `initialize` will be done repeatedly and
    ## with only internal values (which should be guaranteed valid).
    if(length(expr) > 1) {
        ## Input expr is not just a name or number.
        if(identical(expr[[1]], as.name(":"))) {
            start <- intToNumeric(expr[[2]])
            end <- intToNumeric(expr[[3]])
            if(is.numeric(start) && is.numeric(end) &&
               length(start) == 1 && length(end) == 1 &&
               end >= start && start >= 1 && 
               identical(start, round(start)) && identical(end, round(end))) {
                return(indexRangeSequenceClass$new(start, end))
            } else 
                stop("newIndexRange: an indexRange sequence must involve two positive, non-decreasing, integer-valued endpoints.")
        } else {
            ## An expression like c(2,4,6) or matrix(...).

            ## Notes:
            ## (1) An expression that returns a vector is assumed to be a set of 1D indices.
            ## (2) An expression that returns a matrix is assumed to be a rows of indices
            ## (1D or higher-dimensional).
            ## (3) Creating a single row of indices for nDim > 1
            ## requires an expression that returns a 1-row matrix.
            mat <- intToNumeric(as.matrix(eval(expr)))
            vals <- c(mat)
            vals <- vals[!is.na(mat)]
            if(isTRUE(all(vals >= 1)) && isTRUE(all(vals < Inf)) && identical(vals, round(vals))) {
                dimnames(mat) <- NULL
                return(indexRangeMatrixClass$new(mat))
            } else
                stop("newIndexRange: an indexRange matrix must involve positive, integer-valued indices.")
        }
    } else {
        if(length(expr)) {
            expr <- intToNumeric(expr)
            if(is.numeric(expr) && expr >= 1 && identical(expr, round(expr))) {
                if(is.null(dim(expr))) {
                    names(expr) <- NULL
                    return(indexRangeScalarClass$new(expr))
                }
                ## 1x1 matrix
                ## FUTURE: not clear we need to handle this case and/or might convert to scalar.
                if(length(dim(expr)) == 2) {
                    dimnames(expr) <- NULL
                    return(indexRangeMatrixClass$new(expr, sort = FALSE))
                }
                stop("newIndexRange: an indexRange cannot be an array.")
            } else
                stop("newIndexRange: an indexRange with a single index must be a positive, integer-valued number.")
        } else {
            return(NULL)
        }
    }
}

##### `indexRange` base class and subclasses #####

indexRangeClass <- R6Class(
    classname = 'indexRangeClass',
    portable = FALSE,
    public = list(
        numElements = numeric(),
        numColumns = numeric(),
        current = numeric(),
        local = numeric(),
        delay = numeric(),
        
        setDelay = function(delay = 0) {
            current <<- 1
            local <<- 1
            delay <<- delay
        },
        
        getNext = function() {
            item <- current
            if(local < delay) {
                ## Continue giving same index value.
                ## (Other index slot(s) are incrementing.)
                local <<- local + 1
            } else {
                ## Move on to next index value.
                local <<- 1
                current <<- current + 1
                if(current > numElements)
                    current <<- 1
            }
            
            ## Return original index value.
            return(self$getItem(item))
        }, 
        
        getColumns = function(indices) {
            return(self)
        },

        toSequence = function() {
            stop("toSequence: not valid for '", class(self)[1], "' objects.")
        },

        toMatrix = function() {
            stop("toMatrix: not valid for '", class(self)[1], "' objects.")
        },

        toMatrixList = function() {
            stop("toMatrixList: not valid for '", class(self)[1], "' objects.")
        },

        toExpr = function() {
            stop("toExpr: not valid for '", class(self)[1], "' objects.")
        },

        getItem = function(item) {
            stop("getItem: not valid for '", class(self)[1], "' objects.")
        }
    )
)


## (Deprecated) A class representing no elements.
## Replaced by use of `NULL`.
indexRangeEmptyClass <- R6Class(
    classname = 'indexRangeEmptyClass',
    inherit = indexRangeClass,
    portable = FALSE,
    public = list(
        numElements = NULL,
        numColumns = NULL
    )
)


## A class representing a single 'constant' index value, e.g., the `2` in `y[2,i]`.
indexRangeScalarClass <- R6Class(
    classname = 'indexRangeScalarClass',
    inherit = indexRangeClass,
    portable = FALSE,

    public = list(
        numElements = 1,
        numColumns = 1,
        value = NULL,

        initialize = function(value) {
            value <<- value
        },

        getItem = function(item) {
            return(value)
        },
        
        toMatrix = function() {
            return(indexRangeMatrixClass$new(matrix(value), sort = FALSE))
        },

        getMinMax = function() {
            return(c(value, value))
        },

        toExpr = function() {
            return(value)
        }
    )
)

## A class representing a full sequence, e.g., the `2:5` in `y[2:5,i]`.
indexRangeSequenceClass <- R6Class(
    classname = 'indexRangeSequenceClass',
    inherit = indexRangeClass,
    portable = FALSE,
    public = list(
        start = numeric(),
        end = numeric(),
        numColumns = 1,
        
        initialize = function(start, end) {
            ## CHECK: any need to check start <= end?
            start <<- start
            end <<- end
            numElements <<- end - start + 1
        },

        getItem = function(item) {
            ## FUTURE: this repeats arithmetic repeatedly.
            ## Could have specialized `getNext` for Sequence
            return(start + item - 1)
        },

        toScalar = function() {
            if(start == end)
                return(indexRangeScalarClass$new(start))
            return(self)
        },

        toMatrix = function() {
            return(indexRangeMatrixClass$new(matrix(as.numeric(seq.int(start, end))), sort = FALSE))
        },

        toMatrixList = function() {
            return(indexRangeMatrixListClass$new(
                lapply(as.numeric(seq.int(start, end)), matrix)))
        },

        getMinMax = function() {
            return(c(start, end))
        },

        toExpr = function() {
            return(substitute(A:B, list(A = start, B = end)))
        }
    )
)

## A class representing an arbitrary number of one or more indices.
## E.g., (2,4,5) for the second index of y[2,i], y[4,i], y[5,i]
## or ((2,3), (3,4), (7,8)) for the first and third indices of y[i, 3, i+1]
indexRangeMatrixClass <- R6Class(
    classname = 'indexRangeMatrixClass',
    inherit = indexRangeClass,
    portable = FALSE,
    public = list(
        values = numeric(),
        numElements = numeric(),
        numColumns = numeric(),
        
        initialize = function(values, sort = TRUE) {
            if(sort) {
                ord <- do.call(order, lapply(seq_len(ncol(values)), function(i) values[, i]))
                values <<- values[ord, , drop = FALSE]
            } else values <<- values
            numElements <<- as.numeric(nrow(self$values))
            numColumns <<- as.numeric(ncol(self$values))
        },

        getItem = function(item) {
            return(values[item, ])
        },

        getColumns = function(indices = NULL) {
            if(is.null(indices)) {
                return(.self)
            } else
                return(indexRangeMatrixClass$new(values[ , indices, drop = FALSE], sort = FALSE))
        },

        getRows = function(indices = NULL) {
            if(is.null(indices)) {
                return(.self)
            } else
                return(indexRangeMatrixClass$new(values[indices, , drop = FALSE], sort = FALSE))
        },

        getMinMax = function() {
            return(t(apply(values, 2, range)))  # Each row is a dimension.
        },

        removeDuplicates = function() {
            if(anyDuplicated(values)) { # Much faster than `unique`.
                values <<- unique(values)
                numElements <<- as.numeric(nrow(self$values))
            }
        },

        toMatrix = function() {
            return(self)
        },

        ## Convert sequential indexing to sequence class.
        toSequence = function() {
            if(numColumns == 1) {
                rg <- range(values)
                if(length(values) == rg[2] - rg[1] + 1 &&
                   all(diff(values) == 1))
                    return(indexRangeSequenceClass$new(rg[1], rg[2]))
            }
            return(self)
        },

        toExpr = function(maxPrint = 3) {
            if(numColumns > 1) {
                expr <- quote(c(...))
                return(expr[[2]])
            }
            if(numElements > maxPrint) {
                expr <- quote(c(0,0,...,0))
                expr[2:3] <- values[1:2]
                expr[5] <- values[numElements]
                return(expr)
            }
            expr <- quote(c(0))
            expr[2:(length(values)+1)] <- values
            return(expr)
        }
    )
)

## A class representing a list of matrices.
indexRangeMatrixListClass <- R6Class(
    classname = 'indexRangeMatrixListClass',
    inherit = indexRangeClass,
    portable = FALSE,
    public = list(
        rangeList = list(),
        numElements = numeric(),

        initialize = function(rangeList) {
            if(!all(sapply(rangeList, is.matrix)))
                stop("indexRangeMatrixList: input must be a list of matrices.")
            rangeList <<- rangeList
            numElements <<- length(rangeList)
        },

        toMatrix = function() {
            return(indexRangeMatrixClass$new(do.call("rbind", rangeList)))
        },

        ## Naming is a bit tricky - 'row's are really `rangeList` elements
        getRows = function(indices = NULL) {
            if(is.null(indices)) {
                return(.self)
            } else
                return(indexRangeMatrixListClass$new(rangeList[indices]))
        }
    )
)

##### Additional functions #####

## Take a list of `indexRange`s and cross (expand) to give fully-expanded
## `indexRangeMatrix`. E.g. c(3,5) with 1:3 to give (3,1),(5,1),(3,2),(5,2),(3,3),(5,3).
crossIndexRanges <- function(indexRangesList, order) {
    matrixResult <- matrixExpandGrid(lapply(indexRangesList, function(x) x$toMatrix()$values))
    if(!missing(order))
        matrixResult <- matrixResult[ , order, drop = FALSE]
    return(newIndexRange(matrixResult))
}


## Take a list of `indexRangeMatrixList`s (possibly including `indexRangeSequence`s),
## cross by element, and then collapse to a single `indexRangeMatrix`.
indexRangeMatrixListsToMatrix <- function(indexRangesList) {    
    ## Convert any sequences to matrixLists and extract the `rangeList` list of indices.
    rangeListsList <- lapply(indexRangesList,
                  function(x) 
                      if(inherits(x, "indexRangeSequenceClass")) x$toMatrixList()$rangeList else x$rangeList
                  )

    lengths <- sapply(rangeListsList, function(x) length(x))
    if(length(unique(lengths)) > 1)
        stop("indexRangeMatrixListsToMatrix: Inconsistent number of elements in matrixLists to be collapsed to a single matrix.")
    
    ## Cross each element of the rangeLists with corresponding elements of other rangeList(s).
    result <- lapply(seq_len(lengths[1]), function(i)
        matrixExpandGrid(lapply(rangeListsList, function(x) x[[i]])))
    ## Collapse via `rbind` to produce a single `indexRangeMatrix`.
    return(newIndexRange(do.call("rbind", result)))
}

## Cross elements of two or more matrices of indexes, returning a matrix of indices.
matrixExpandGrid <- function(matrixList) {
    indexVectors <- lapply(matrixList, function(x) seq_len(nrow(x)))
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
    return(do.call("cbind", unfoldedMatrices))
}

    
