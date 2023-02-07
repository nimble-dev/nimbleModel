## A varRangeClass object represents a variable with some set of indices,
## for the purpose of graph queries.

## It does not represent the *values* of the variable.  Instead it symbolically
## represents some subset of the variable to be manipulated and passed.

## It can represent
## -  entire variables: x
## -  variable blocks: x[1:6, 2:5]
## -  arbitrary single-index subsets in each index:
##         x[c(2, 4, 6), c(3, 5, 7)]
## -  combinations of index blocks and single-index subsets:
##         x[c(2, 4, 6), 3:7]
## -  arbitrary indices given as rows of a matrix:
##         x[ matrix(c(1, 2, 10, 12, 5, 2), ncol = 2) ]
## -  combinations of matrices, index blocks, and single-index subsets
##         x[ matrix(...), c(2, 4, 6), 3:5 ]
## Internally, it manages multiple representations of the indices that are
## useful in different ways.

## TODO: 'varName' -> name?

varRangeClass <- R6Class(
    'varRangeClass',
    portable = FALSE,
    public = list(
        ## Two examples: x[1:10]; y[ c(2, 4, 6) ]
        ##
        ## name: 'x'; 'y'
        varName = character(),
        ## indexRangeExprs: list(quote(1:10)); list(quote(c(2, 4, 6)))
        indexRangeExprs = list(),
        ## indexRanges: list(list(1, 10)); list(list(c(2, 4, 6)))
        ## created by indexRange for the indexRangeExprs
        indexRanges = list(),
        rangeID_2_indexID = list(),
        indexID_2_rangeID = numeric(),
        ## a list indexed by order of indexRanges
        ## each element is a vector of the "columns" (indexID)
        ## managed by that index Range.
        ## This is needed for matrix indexRanges that tie together
        ## some arbitrary set of index slots.
        ## Example: x[3, 1, 11], x[3, 2, 11], x[10, 1, 8], x[10, 2, 8]
        ##  This has a matrix ([3, 11]; [10, 8]) of arbitrary indices
        ## for indexIDs 1 and 3 (first and third indices).  It has a
        ## block (1:2) for the second index.
        ## This would have rangeID_2_indexID[[1]]: c(1, 3)
        ##                 rangeID_2_indexID[[2]]: 2
        fromStochRule = character(),  # when returned as result of graphRule
        initialize = function(indexInfo,
                              indexOrders = NULL,
                              varName = NULL,
                              fromStochRule = NULL) {
            ## initialization from an expression
            ## does not support some of the complicated cases.
            ##
            ## We will need some way to initialize more complex
            ## cases returned from graph queries.
            ##
            ## It seems that we can initialize using indexOrders arg to varRangeClass$new()  // CJP
            fromStochRule <<- fromStochRule
            if(is(indexInfo, "indexRange"))
                stop("varRange must be initialized from a list of indexRanges not a single indexRange.")
            if(is.character(indexInfo))
                indexInfo <- parse(text = indexInfo,
                                   keep.source = FALSE)[[1]]
            ## input is an expression
            if(is.call(indexInfo) || is.name(indexInfo)) {
                if(length(indexInfo)==1) {
                    ## The expression is just a name
                    nameFromExpr <- as.character(indexInfo)
                    indexRanges <<- list(indexRange_none())
                } else {
                    ## The expression must have some indexing.
                    ## Check that it starts with `[`:               
                    if(!identical(indexInfo[[1]], as.name("[")))
                        stop(paste(deparse(indexInfo),
                                   ' is not valid variable or variable range.'),
                             call. = TRUE)
                    nameFromExpr <- deparse(indexInfo[[2]])
                    indexRangeExprs <<- as.list(indexInfo[-c(1,2)])
                    indexRanges <<- lapply(
                        indexRangeExprs,
                        indexRange
                    )
                    ## TODO: check for too-long matrices and truncate.
                    rangeID_2_indexID <<-
                        as.list(seq_along(indexRanges))
                    rangeID <- rep(seq_along(rangeID_2_indexID), times = sapply(rangeID_2_indexID, length))
                    indexID_2_rangeID <<- rangeID[order(unlist(rangeID_2_indexID))]
                }
                if(is.null(varName))
                    varName <<- nameFromExpr
                else
                    varName <<- varName
                return(self)
            }
            ## input is a list that should be of indexRanges
            if(is.list(indexInfo)) {
                varName <<- varName
                setIndexRanges(indexInfo, indexOrders)
                indexRangeExprs <<- lapply(indexRanges, function(x) x$toExpr())
            }
            if(length(rangeID_2_indexID)) { 
                rangeID <- rep(seq_along(rangeID_2_indexID), times = sapply(rangeID_2_indexID, length))
                indexID_2_rangeID <<- rangeID[order(unlist(rangeID_2_indexID))]
            } else indexID_2_rangeID <<- numeric(0) ## no indexing of the variable
        },
        getSingleIndexRange = function(index, ...) {
            ## Iterate over indexRanges rather than indices
            ## so that any matrices can be kept together if possible.
            varRange_getSingleIndexRange(self,
                                         index,
                                         ...)
        },
        getIndexRangeMatrix = function(indices, ...) {
            varRange_getIndexRangeMatrix(self,
                                         indices,
                                         ...)
        },
        setIndexRanges = function(indexRanges,
                                  indexOrders = NULL) {
            ## expects a list input, as returned by indexRange
            self$indexRanges <- indexRanges
            self$indexRangeExprs <- list()
            ## self$indexRangeExprs <- lapply(
            ##     indexRanges,
            ##     indexRange2expr
            ## )
            if(identical(attr(indexRanges[[1]], "rangeType"), "none")) {
                    self$rangeID_2_indexID <- list()
            } else {
                if(!is.null(indexOrders))
                    self$rangeID_2_indexID <- indexOrders
                else {
                    nextID <- 1
                    self$rangeID_2_indexID <-
                        lapply(indexRanges,
                               function(x) {
                                   numCols <- x$numColumns
                                   if(!numCols) numCols <- 1  # empty IR - this handling might need to be modified
                                   ans <- nextID-1 + (1:numCols)
                                   nextID <<- nextID + numCols
                                   as.integer(ans)
                               })
                }
            }
            self
        },
        isEmpty = function() {
            return(any(sapply(self$indexRanges,
                              function(x) is(x, "indexRangeEmptyClass"))))
        },
        isNone = function() {
            return(length(indexRanges) == 1 &&
                   is(indexRanges[[1]], "indexRangeNoneClass"))
        }
        
    )
)


varRange_isEqual <- function(vr1, vr2) {
    identical(vr1$indexID_2_rangeID, vr2$indexID_2_rangeID) &&
        identical(vr1$rangeID_2_indexID, vr2$rangeID_2_indexID) &&
        identical(vr1$indexRanges, vr2$indexRanges)
}


## extract an indexRange for a single column of a varRange
varRange_getSingleIndexRange <- function(varRange,
                                         index,
                                         details = FALSE) {
    done <- FALSE
    iRange <- 1
    result <- NULL
    usedRanges <- integer()
    while(!done) {
        boolIndex <- index == varRange$rangeID_2_indexID[[iRange]]
        if(any(boolIndex)) {
            innerIndex <- which(boolIndex)
            result <- varRange$indexRanges[[iRange]]$getIndices(innerIndex)
            usedRanges <- append(usedRanges, iRange) 
            done <- TRUE
        }
        iRange <- iRange + 1
        if(iRange > length(varRange$indexRanges)) done <- TRUE
    }
    if(!details)
        result
    else
        list(result = result,
             usedRanges = usedRanges)
}

## extract multiple columns of a varRange expanded as
## an index matrix.
varRange_getIndexRangeMatrix <- function(varRange,
                                         indices,
                                         details = FALSE) {
    done <- FALSE
    iRange <- 1
    numRequestedIndices <- length(indices)
    indexRangeResults <- list()
    iResult <- 1
    usedRanges <- integer()
    usedIndices <- NULL
    while(!done) {
        boolIndex <- varRange$rangeID_2_indexID[[iRange]] %in% indices
        usedIndices <- c(usedIndices, varRange$rangeID_2_indexID[[iRange]][boolIndex])
        if(any(boolIndex)) {
            innerIndices <- which(boolIndex)
            indexRangeResults[[iResult]] <-
                varRange$indexRanges[[iRange]]$getColumns(innerIndices)
            iResult <- iResult + 1
            usedRanges <- append(usedRanges, iRange)
        }
        iRange <- iRange + 1
        if(iRange > length(varRange$indexRanges)) done <- TRUE
    }
    result <- crossIndexRanges(indexRangeResults)
    ## Extract requested indices in correct order.
    result[[1]] <- result[[1]][ , match(indices, usedIndices), drop = FALSE]
    if(!details)
        result
    else
        list(result = result,
             usedRanges = usedRanges)
}

## The following could sensibly be class methods, but
## we are going to try to keep classes small.

## TODO: make methods?

## varRange2char takes a varRange object and returns the corresponding
## character string of the original expression (or the imputed expression
## when initialized from a list of `indexRange`s.
varRange2char <- function(VR) {
    deparse(varRange2expr(VR))
}

## `varRange2expr` takes a `varRange` object and returns the corresponding
## expression.  This inverts the initialize function of `varRangeClass` for
## an expression input and imputes the index values in the case of a
## a `varRange` initialized from a list of `indexRange`s.
## Example 1: varRange2expr(varRangeClass$new(quote(x[1:10]))) ==> "x[1:10]"
## EXample 2: varRange2expr(varRangeClass$new(list(indexRange(quote(1:10), varName = 'x'))))
##  ==> "x[1:10]"
varRange2expr <- function(VR) {
    do.call("call",
            c(list("[",
                   as.name(VR$varName)),
              VR$indexRangeExprs[VR$indexID_2_rangeID]),
            quote = TRUE)
}


getVarName <- function(x) {
    if(is(x, 'varRangeClass'))
        return(x$varName)
    if(is.character(x)) {
        expr <- parse(text = x)[[1]]
        if(length(expr) == 1) return(x) else return(deparse(expr[[2]]))
    }
    if(is.null(x)) return(NULL)
    stop("getVarName: unexpected input.")
}
               
