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
        varName = character(),
        fromStochRule = logical(),  # used when returned as result of graphRule
        indexRangeExprs = list(),   # e.g., `1:10`, `c(2,4,6)`
        indexRanges = list(),

        ## These next two should be integer type (arbitrary but good to do for ease of testing).
        ## They contain the information indicating how index positions relate to indexRanges.
        ## Example: x[3, 1, 11], x[3, 2, 11], x[10, 1, 8], x[10, 2, 8]
        ## An indexRangeMatrix handles 1st/3rd index positions, while a sequence covers second.
        rangeID_2_indexID = list(),  # e.g., list(c(1,3), 2) for a matrix covering 1st/3rd index positions
        indexID_2_rangeID = integer(),  # e.g., c(1,2,1)

        initialize = function(indexInfo,
                              indexOrders = NULL,
                              varName = NULL,
                              fromStochRule = NULL) {

            fromStochRule <<- fromStochRule
            if(is(indexInfo, "indexRange"))
                stop("varRange must be initialized from a list of indexRanges not a single indexRange.")
            if(is.character(indexInfo))
                indexInfo <- parse(text = indexInfo, keep.source = FALSE)[[1]]
            
            ## Input is an expression.
            if(is.call(indexInfo) || is.name(indexInfo)) {
                if(length(indexInfo) == 1) {
                    ## The expression is just a name.
                    nameFromExpr <- as.character(indexInfo)
                    indexRanges <<- list(indexRangeNoneClass$new())
                    rangeID_2_indexID <<- list()
                } else {
                    ## The expression must have some indexing so it must start with `[`.
                    if(!identical(indexInfo[[1]], as.name("[")))
                        stop("varRange: input is not a valid variable or variable range.")
                    nameFromExpr <- deparse(indexInfo[[2]])
                    indexRangeExprs <<- as.list(indexInfo[-c(1,2)])
                    indexRanges <<- lapply(indexRangeExprs, indexRange)
                    
                    ## Truncate indexRangeExprs for matrices for nicer printing.
                    if(any(sapply(indexRanges, function(x) is(x, "indexRangeMatrixClass"))))
                        indexRangeExprs <<- lapply(indexRanges, function(x) x$toExpr())
                    
                    rangeID_2_indexID <<- as.list(seq_along(indexRanges))
                }
                if(is.null(varName)) {
                    varName <<- nameFromExpr
                } else {
                    if(!identical(varName, nameFromExpr)) {
                        message("varRange: `varName` does not match variable name in input expression. Using name from expression.")
                        varName <<- nameFromExpr
                    } else varName <<- varName
                }
            } else {
                ## Input is a list that should be of `indexRange`s.
                if(is.list(indexInfo)) {
                    if(!all(sapply(indexInfo, function(x) is(x, "indexRangeClass"))))
                        stop("varRange: `indexInfo` should be a list of `indexRange`s.")
                    varName <<- varName
                    setIndexRanges(indexInfo, indexOrders)
                    indexRangeExprs <<- lapply(indexRanges, function(x) x$toExpr())
                } else stop("varRange: unexpected input.")
            }
            if(length(rangeID_2_indexID)) { 
                rangeID <- rep(seq_along(rangeID_2_indexID), times = sapply(rangeID_2_indexID, length))
                indexID_2_rangeID <<- rangeID[order(unlist(rangeID_2_indexID))]
            } else indexID_2_rangeID <<- integer(0) ## no indexing of the variable
        },

        setIndexRanges = function(indexRanges, indexOrders = NULL) {
            ## Helper method for `initialize` to set `indexRanges` and `rangeID_2_indexID`
            ## based on list input, each element as returned by `indexRange` and possibly
            ## an `indexOrders` list relating each `indexRange` to one or more index positions.
            indexRanges <<- indexRanges
            if(is(indexRanges[[1]], "indexRangeNoneClass")) {
                rangeID_2_indexID <<- list()
            } else {
                if(!is.null(indexOrders))
                    rangeID_2_indexID <<- lapply(indexOrders, as.integer)
                else {
                    ## Assign elements of `rangeID_2_indexID` sequentially based on number of columns.
                    nextID <- 1
                    rangeID_2_indexID <<-
                        lapply(indexRanges,
                               function(x) {
                                   numCols <- x$numColumns
                                   if(is.null(numCols)) numCols <- 1  # indexRangeEmpty
                                   ans <- nextID-1 + (1:numCols)
                                   nextID <<- nextID + numCols
                                   as.integer(ans)
                               })
                }
            }
            return(NULL)
        },

        ## Extract one or more columns of a varRange.
        ## If multiple columns, result is expanded as a matrix of indices.
        extractIndexRange = function(indices, returnUsedRanges = FALSE) {
            
            usedIndices <- lapply(rangeID_2_indexID, function(x) x %in% indices)
            usedRanges <- which(sapply(usedIndices, any))

            if(!length(usedRanges)) {
                indexRangeResult <-indexRange(NULL)
            } else {            
                indexRangesList <- lapply(usedRanges, function(i) {
                    innerIndices <- which(usedIndices[[i]])
                    return(indexRanges[[i]]$getColumns(innerIndices))
                })
                
                if(length(indexRangesList == 1)) {
                    indexRangeResult <- indexRangesList[[1]]
                } else {
                    indexRangeResult <- crossIndexRanges(indexRangesList, order = match(indices, usedIndices))  ## result is an indexRangeMatrix
                }
            }
            if(!returnUsedRanges) {
                return(indexRangeResult)
            } else return(list(indexRange = indexRange, usedRanges = usedRanges))
        },

        isEmpty = function() {
            return(any(sapply(self$indexRanges,
                              function(x) is(x, "indexRangeEmptyClass"))))
        },
        
        isNone = function() {
            return(length(indexRanges) == 1 &&
                   is(indexRanges[[1]], "indexRangeNoneClass"))
        },

        ## `toExpr` takes a `varRange` object and returns the corresponding
        ## expression.  This inverts the initialize function of `varRangeClass` for
        ## an expression input and imputes the index values in the case of a
        ## a `varRange` initialized from a list of `indexRange`s.
        ## Example 1: varRange2expr(varRangeClass$new(quote(x[1:10]))) ==> "x[1:10]"
        ## EXample 2: varRange2expr(varRangeClass$new(list(indexRange(quote(1:10), varName = 'x'))))
        ##  ==> "x[1:10]"
        toExpr = function() {
            if(isNone()) {
                return(as.name(varName))
            } else 
                do.call("call",
                        c(list("[",
                               as.name(varName)),
                          indexRangeExprs[indexID_2_rangeID]),
                        quote = TRUE)
        },

        ## `toChar` takes a varRange object and returns the corresponding
        ## character string of the original expression (or the imputed expression
        ## when initialized from a list of `indexRange`s.
        toChar = function() {
            deparse(toExpr())
        },

        print = function() {
            cat("variable range for `", toChar(), "`.\n", sep = '')
        }
        
    )
)


varRange_isEqual <- function(vr1, vr2) {
    identical(vr1$indexID_2_rangeID, vr2$indexID_2_rangeID) &&
        identical(vr1$rangeID_2_indexID, vr2$rangeID_2_indexID) &&
        identical(vr1$indexRanges, vr2$indexRanges)
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
               
