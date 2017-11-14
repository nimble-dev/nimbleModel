## A varRangeClass object represents a variable with some set of indices.
## It does not represent the *values* of the variable.  Instead it symbolically
## represents some subset of the variable to be manipulated and passed.

## Eventually this will all be in C++ for performance.
## R implementation is just to refine concepts.

## see test-varRangeClass.R

library(R6)

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
##   if it is a sequence: list(1, 5)
##   if it is a vector: list(<vector>)
## It seems useful to have the result be a list,
## even when it only has one element
indexRange <- function(expr) {
    if(length(expr) > 1) {
        if(identical(expr[[1]], as.name(":")))
            structure(as.list(expr[-1]),
                      class = "indexRange",
                      rangeType = "block")
        else
            structure(list(eval(expr)),
                      class = "indexRange",
                      rangeType = "vector")
    } else {
        if(isBlank(expr))
            structure(list(expr),
                      class = "indexRange",
                      rangeType = "blank")
        else
            structure(list(expr),
                      class = "indexRange",
                      rangeType = "scalar")
    }
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

indexRange_isVector <- function(IRL) {
    attr(indexRange, 'rangeType') == "vector"
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
                              list(A = IRL[[1]],
                                   B = IRL[[2]])),
           vector = IRL[[1]],
           blank = IRL[[1]],
           scalar = IRL[[1]],
           stop("Unknown indexRange list"))
}

## varRangeClass is nimble's canonical representation of a variable
## and a set of indices for purposes of graph queries.
##
## It can represent
## -  entire variables: x
## -  variable blocks: x[1:6, 2:5]
## -  arbitrary index subsets in each index: x[ c(2, 4, 6), c(3, 5, 7)]
## -  combinations of index blocks and subsets: x[ c(2, 4, 6), 3;7 ]
## -  arbitrary indices given as rows of a matrix: x[ matrix(c(1, 2, 10, 12, 5, 2), ncol = 2) ]
## -  
##
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
        
        initialize = function(varRange) {
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
            }
            self
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
