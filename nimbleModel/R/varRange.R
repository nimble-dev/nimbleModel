## A varRangeClass object represents a variable with some set of indices.
## It does not represent the *values* of the variable.  Instead it symbolically
## represents some subset of the variable to be manipulated and passed.

## Eventually this will all be in C++ for performance.
## R implementation is just to refine concepts.

## see test-varRangeClass.R


## varRangeClass is nimble's canonical representation of a variable
## and a set of indices for purposes of graph queries.
##
## It can represent
## -  entire variables: x
## -  variable blocks: x[1:6, 2:5]
## -  arbitrary single-index subsets in each index:
##         x[ c(2, 4, 6), c(3, 5, 7)]
## -  combinations of index blocks and single-index subsets:
##         x[ c(2, 4, 6), 3:7 ]
## -  arbitrary indices given as rows of a matrix:
##         x[ matrix(c(1, 2, 10, 12, 5, 2), ncol = 2) ]
## -  combinations of matrices, index blocks, and single-index subsets
##         x[ matrix(...), c(2, 4, 6), 3:5 ]
## Internally, it manages multiple representations of the indices that are
## useful in different ways.
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
        indexID_2_rangeID = list(),
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
        initialize = function(indexInfo,
                              indexOrders = NULL,
                              varName = NULL) {
            ## initialization from an expression
            ## does not support some of the complicated cases.
            ##
            ## We will need some way to initialize more complex
            ## cases returned from graph queries.
            ##
            ## It seems that we can initialize using indexOrders arg to varRangeClass$new()  // CJP 
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
                    rangeID_2_indexID <<-
                        as.list(seq_along(indexRanges))
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
            }
            rangeID <- rep(seq_along(rangeID_2_indexID), times = sapply(rangeID_2_indexID, length))
            indexID_2_rangeID <<- rangeID[order(unlist(rangeID_2_indexID))]
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
            ## self$indexRangeExprs <- lapply(
            ##     indexRanges,
            ##     indexRange2expr
            ## )
            if(identical(attr(indexRanges[[1]], "rangeType"), "none")) {
                    self$rangeID_2_indexID <- integer(0)
            } else {
                if(!is.null(indexOrders))
                    self$rangeID_2_indexID <- indexOrders
                else {
                    nextID <- 1
                    self$rangeID_2_indexID <-
                        lapply(indexRanges,
                               function(x) {
                                   numCols <- indexRange_numCols(x)
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
                              function(x) identical(attr(x, 'rangeType'), 'empty'))))
        },
        isNone = function() {
            return(length(indexRanges) == 1 && identical(indexRanges[[1]], indexRange_none()))
        
    )
)


varRange_isEqual <- function(vr1, vr2) {
    identical(vr1$indexID_2_rangeID, vr2$indexID_2_rangeID) &&
        identical(vr1$rangeID_2_indexID, vr2$rangeID_2_indexID) &&
        identical(vr1$indexRanges, vr2$indexRanges)
}

## 2022-07-25: this is apparently never used.
invertIndexList <- function(indexList) {
    browser()
    inputLengths <- lapply(indexList, length)
    inputEntries <- unlist(indexList)
    inputLabels <- rep(seq_along(indexList), times = inputLengths)
    s <- split(inputLabels, inputEntries)
    indices <- as.integer(names(s))
    ans <- vector('list', length = max(indices))
    ans[indices] <- s
    ans
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
            result <- indexRange_getCols(
                varRange$indexRanges[[iRange]],
                innerIndex)
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
    while(!done) {
        boolIndex <- varRange$rangeID_2_indexID[[iRange]] %in% indices
        if(any(boolIndex)) {
            innerIndices <- which(boolIndex)
            indexRangeResults[[iResult]] <-
                indexRange_getCols(
                    varRange$indexRanges[[iRange]],
                    innerIndices)
            iResult <- iResult + 1
            usedRanges <- append(usedRanges, iRange)
        }
        iRange <- iRange + 1
        if(iRange > length(varRange$indexRanges)) done <- TRUE
    }
    result <- indexRangeList2matrix(indexRangeResults)
    if(!details)
        result
    else
        list(result = result,
             usedRanges = usedRanges)
}

## extract multiple columns of a varRange, keeping
## indexRanges intact.  This is returned as a new varRange.
varRange_getIndexRangeColumns <- function(varRange,
                                          indices) {

}

## The following could sensibly be class methods, but
## we are going to try to keep classes small.


## TODO: varRange2{char,expr} only work if varRange created
## using an expression. If created with a list of indexRanges, it
## doesn't show the indexing.
## Rework this in conjunction with nodeRangeClass$expandNames(),
## allowing user to request compact or expanded representations and element vs block,
## e.g., y[1:5,2] vs. y[i,2] for i=1,...5 vs y[1],y[2],y[3],y[4],y[5]
## or y[i] for i=c(3,5,7,9,12) vs. y[i] for i=c(3,5,...,12) vs y[3],y[5],y[7],y[9],y[12]

## varRange2char takes a varRange object and returns the corresponding
## character string of the original expression.
## This inverts the initialize function of varRangeClass for
## character input.
## Example: varRange2expr(varRangeClass$new("x[1:10]"))) ==> "x[1:10]"

varRange2char <- function(VR) {
    deparse(varRange2expr(VR))
}

## varRange2expr takes a varRange object and returns the corresponding
## expression.  This inverts the initialize function of varRangeClass for
## an expression input.
## Example: varRange2expr(varRangeClass$new(quote(x[1:1]))) ==> "x[1:10]"
varRange2expr <- function(VR) {
    do.call("call",
            c(list("[",
                   as.name(VR$varName)),
              VR$indexRangeExprs),
            quote = TRUE)
}

## How many indices are there in a varRange?
numIndices <- function(VR) {
    length(VR$indexRanges)
}

## ## need to deal with each kind of combination
## mergeVarRanges <- function(VR1, VR2) {
##     ## This won't work for arbitrary index rows
##     if(length(VR1$indexRangeExprs) != length(VR2$indexRangeExprs))
##         stop( paste0(printVarRange(VR1),
##                      ' has different number of dimensions than ',
##                      printVarRange(VR2)),
##              call. = FALSE)
##     if(length(VR1$indexRangeExprs) == 0) return(VR1)
## }

## eval range extracts from one variable the indices
## of a varRangeClass object
##
## Example: x may be a model variabe that is a 5x10 matrix
##          A graph query may involve x[2:3, 3:5]
##          The x[2:3, 3:5] is represented as a varRangeClass object
##          One may need to look up indices or IDs from a matrix
##                  with the same shape as x, say xIndices.
##          To do so: evalIndexRange(xIndices, varRangeClass$new(quote(x[2:3, 3:5]))
##
## This will not work if varRange has any matrix indexRanges
evalIndexRange <- function(x, varRange) {
    xExpr = substitute(x)
    if(length(varRange$indexRanges)==0)
        x
    else {
        do.call("[", c(list(xExpr),
                       varRange$indexRangeExprs),
                envir = parent.frame())
    }
}
