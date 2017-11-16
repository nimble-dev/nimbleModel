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
        name = character(),
        ## indexRangeExprs: list(quote(1:10)); list(quote(c(2, 4, 6)))
        indexRangeExprs = list(),
        ## indexRanges: list(list(1, 10)); list(list(c(2, 4, 6)))
        ## created by indexRange for the indexRangeExprs
        indexRanges = list(),
        rangeID_2_indexID = list(),
        ## a list indexed by order of indexRanges
        ## each element is a vector of the "columns" (indexID)
        ## managed by that index Range.
        ## This is needed for matrix indexRanges that tie together
        ## some arbitrary set of index slots.
        ## Example: x[3, 1, 11], x[3, 2, 11], x[10, 1, 8], x[10, 2, 8]
        ##  This has a matrix ([3, 11]; [10, 8]) of arbitrary indices
        ## for indexIDs 1 and 3 (first and third indices).  It has a
        ## block (1:2) for the second index.
        initialize = function(varRange) {
            ## initialization from an expression
            ## does not support some of the complicated cases.
            ##
            ## We will need some way to initialize more complex
            ## cases returned from graph queries.
            ##
            if(is.character(varRange))
                varRange <- parse(text = varRange,
                                  keep.source = FALSE)[[1]]
            if(length(varRange)==1) {
                ## The expression is just a name
                name <<- as.character(varRange)
            } else {
                ## The expression must have some indexing.
                ## Check that it starts with `[`:               
                if(!identical(varRange[[1]], as.name("[")))
                    stop(paste(deparse(varRange),
                               ' is not valid variable or variable range.'),
                         call. = TRUE)
                name <<- deparse(varRange[[2]])
                indexRangeExprs <<- as.list(varRange[-c(1,2)])
                indexRanges <<- lapply(
                    indexRangeExprs,
                    indexRange
                )
                rangeID_2_indexID <<-
                    as.list(seq_along(indexRanges))
            }
            self
        },
        getIndexRanges = function(indices) {
            ## Iterate over indexRanges rather than indices
            ## so that any matrices can be kept together if possible.
            
        },
        setIndexRanges = function(indexRanges) {
            ## expects a list input, as returned by indexRange
            self$indexRanges <- indexRanges
            self$indexRangeExprs <- lapply(
                indexRanges,
                indexRange2expr
            )
        }
    )
)

## The following could sensibly be class methods, but
## we are going to try to keep classes small.

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
                   as.name(VR$name)),
              VR$indexRangeExprs),
            quote = TRUE)
}

## How many indices are there in a varRange?
numIndices <- function(VR) {
    length(VR$indexRanges)
}

## need to deal with each kind of combination
mergeVarRanges <- function(VR1, VR2) {
    ## This won't work for arbitrary index rows
    if(length(VR1$indexRangeExprs) != length(VR2$indexRangeExprs))
        stop( paste0(printVarRange(VR1),
                     ' has different number of dimensions than ',
                     printVarRange(VR2)),
             call. = FALSE)

    if(length(VR1$indexRangeExprs) == 0) return(VR1)

}

## eval range extracts from one variable the indices
## of a varRangeClass object
##
## Example: x may be a model variabe that is a 5x10 matrix
##          A graph query may involve x[2:3, 3:5]
##          The x[2:3, 3:5] is represented as a varRangeClass object
##          One may need to look up indices or IDs from a matrix
##                  with the same shape as x, say xIndices.
##          To do so: evalIndexRange(xIndices, varRangeClass$new(quote(x[2:3, 3:5]))
evalIndexRange <- function(var2output, varRange) {
    var2output = substitute(var2output)
    if(length(varRange$indexRanges)==0)
        eval(var2output, envir = parent.frame())
    else {
        do.call("[", c(list(var2output),
                       varRange$indexRangeExprs),
                envir = parent.frame())
    }
}



## TO DO: combine and difference varRange objects...?
##        What are the needs here?
