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
        rangeToIndexSlot = list(),  # e.g., list(c(1,3), 2) for a matrix covering 1st/3rd index positions
        indexSlotToRange = integer(),  # e.g., c(1,2,1)

        initialize = function(indexInfo,
                              rangeToIndexSlot = NULL,
                              varName = NULL,
                              fromStochRule = NULL) {

            fromStochRule <<- fromStochRule
            if(is(indexInfo, "indexRangeClass"))
                stop("varRange must be initialized from a list of indexRanges not a single indexRange.")
            if(is.character(indexInfo))
                indexInfo <- parse(text = indexInfo, keep.source = FALSE)[[1]]
            
            ## Input is an expression.
            if(is.call(indexInfo) || is.name(indexInfo)) {
                if(length(indexInfo) == 1) {
                    ## The expression is just a name.
                    nameFromExpr <- as.character(indexInfo)
                } else {
                    ## The expression must have some indexing so it must start with `[`.
                    if(!identical(indexInfo[[1]], as.name("[")))
                        stop("varRange: input is not a valid variable or variable range.")
                    nameFromExpr <- deparse(indexInfo[[2]])
                    indexRangeExprs <<- as.list(indexInfo[-c(1,2)])
                    indexRanges <<- lapply(indexRangeExprs, newIndexRange)
                    
                    ## Truncate indexRangeExprs for matrices for nicer printing.
                    if(any(sapply(indexRanges, function(x) is(x, "indexRangeMatrixClass"))))
                        indexRangeExprs <<- lapply(indexRanges, function(x) x$toExpr())
                    
                    rangeToIndexSlot <<- as.list(seq_along(indexRanges))
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
                    if(length(indexInfo)) {
                        if(!all(sapply(indexInfo, function(x) is(x, "indexRangeClass"))))
                            stop("varRange: `indexInfo` should be a list of `indexRange`s.")
                        setIndexRanges(indexInfo, rangeToIndexSlot)
                        indexRangeExprs <<- lapply(indexRanges, function(x) x$toExpr())
                    }
                    varName <<- varName
                } else stop("varRange: unexpected input.")
            }
            if(length(self$rangeToIndexSlot)) { 
                rangeID <- rep(seq_along(self$rangeToIndexSlot), times = sapply(self$rangeToIndexSlot, length))
                indexSlotToRange <<- rangeID[order(unlist(self$rangeToIndexSlot))]
            } 
        },

        setIndexRanges = function(indexRanges, rangeToIndexSlot = NULL) {
            ## Helper method for `initialize` to set `indexRanges` and `rangeToIndexSlot`
            ## based on list input, each element as returned by `indexRange` and possibly
            ## an `rangeToIndexSlot` list relating each `indexRange` to one or more index positions.
            indexRanges <<- indexRanges
            if(!is.null(rangeToIndexSlot)) {
                rangeToIndexSlot <<- lapply(rangeToIndexSlot, as.integer)
            } else {
                ## Assign elements of `rangeToIndexSlot` sequentially based on number of columns.
                nextID <- 1
                rangeToIndexSlot <<-
                    lapply(indexRanges,
                           function(x) {
                               numCols <- x$numColumns
                               if(is.null(numCols)) numCols <- 1  # indexRangeEmpty
                               ans <- nextID-1 + (1:numCols)
                               nextID <<- nextID + numCols
                               as.integer(ans)
                           })
            }
            return(NULL)
        },

        ## Extract one or more columns of a varRange.
        ## If multiple columns, result is expanded as a matrix of indices.
        ## TODO: `returnUsedRanges` is never used, so could remove.
        extractIndexRange = function(indices, returnUsedRanges = FALSE) {
            
            usedIndices <- unlist(lapply(rangeToIndexSlot, function(x) x[x %in% indices]))
            usedIndicesBool <- lapply(rangeToIndexSlot, function(x) x %in% indices)
            usedRanges <- which(sapply(usedIndicesBool, any))
            
            if(!length(usedRanges)) {
                indexRangeResult <- newIndexRange(NULL)
            } else {            
                indexRangesList <- lapply(usedRanges, function(i) {
                    innerIndices <- which(usedIndicesBool[[i]])
                    return(indexRanges[[i]]$getColumns(innerIndices))
                })
                
                if(length(indexRangesList) == 1) {
                    if(is(indexRangesList[[1]], "indexRangeMatrixClass") &&
                       indexRangesList[[1]]$numColumns > 1) {
                        indexRangeResult <- newIndexRange(indexRangesList[[1]]$values[ , match(indices, usedIndices)])
                    } else indexRangeResult <- indexRangesList[[1]]
                } else {
                    indexRangeResult <- crossIndexRanges(indexRangesList, order = match(indices, usedIndices))  ## result is an indexRangeMatrix
                }
            }
            if(!returnUsedRanges) {
                return(indexRangeResult)
            } else return(list(indexRange = indexRangeResult, usedRanges = usedRanges))
        },

        isEmpty = function() {
            return(any(sapply(self$indexRanges,
                              function(x) is(x, "indexRangeEmptyClass"))))
        },
        
        isNone = function() {
            return(!length(indexRanges))
        },

        ## `toExpr` takes a `varRange` object and returns the corresponding
        ## expression.  This inverts the initialize function of `varRangeClass` for
        ## an expression input and imputes the index values in the case of a
        ## a `varRange` initialized from a list of `indexRange`s.
        ## Example 1: varRange2expr(varRangeClass$new(quote(x[1:10]))) ==> "x[1:10]"
        ## EXample 2: varRange2expr(varRangeClass$new(list(indexRange(quote(1:10), varName = 'x'))))
        ##  ==> "x[1:10]"
        toExpr = function() {
            if(is.null(varName)) {
                nm <- as.name("no_name")
            } else nm <- as.name(varName)
            if(isNone()) {
                return(nm)
            } else {
                
                do.call("call",
                        c(list("[", nm),
                          indexRangeExprs[indexSlotToRange]),
                        quote = TRUE)
            }
        },

        ## `toChar` takes a varRange object and returns the corresponding
        ## character string of the original expression (or the imputed expression
        ## when initialized from a list of `indexRange`s).
        toChar = function() {
            deparse(toExpr())
        },

        print = function() {
            cat("variable range for `", toChar(), "`.\n", sep = '')
        }
        
    )
)

## NOTE: This will not catch cases where the order of the indexRanges
## is permuted consistent with `rangeToIndexSlot` and `indexSlotToRange`.
varRange_isEqual <- function(vr1, vr2) {
    identical(vr1$indexSlotToRange, vr2$indexSlotToRange) &&
        identical(vr1$rangeToIndexSlot, vr2$rangeToIndexSlot) &&
        isTRUE(all.equal(vr1$indexRanges, vr2$indexRanges))
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
               
