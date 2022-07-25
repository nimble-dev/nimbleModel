calcRangeClass <- R6Class(
    classname = "calcRangeClass",
    portable = FALSE,
    public = list(
        varName = NULL,
        indexingRange = NULL,
        sortID = NULL,
        initialize = function(varName, indexingRange, calcFun, sortID) {
            varName <<- varName
            indexingRange <<- indexingRange
            calcFun <<- calcFun  ## note that calcFun itself is not vectorized
            sortID <<- sortID
        },

        ## Generic calculate function that crosses the indexRanges in the indexingRange (a varRange)
        ## and extracts the original indice to feed into calculate nodeFunction
        ## that operates on set of scalar indices.
        
        ## Will need to figure out how this is going to get compiled.
        ## Will there be a permanent C++ version?
        ## How will indexingRange be compiled?
        ## This is a sketch and hasn't been debugged...
        calculate = function() {
            numRanges <- length(indexingRange$indexRanges)
            index <- numeric(length(indexingRange$indexID_2_rangeID))  ## vector to hold the original index values
            indexRange_lengths <- sapply(indexingRange$indexRanges, indexRange_numRows)
            indexPositions <- indexingRange$rangeID_2_indexID
            nestedLengths <- sapply(seq_len(numRanges), function(i) prod(indexRange_lengths[(i+1):numRanges]))
                                    
            for(item in prod(indexRange_lengths)) {
                for(irIndex in seq_len(numRanges)) {
                    ## Determine nested indexing from unrolled indexing
                    if(irIndex == seq_len(numRanges)) {
                        elementIdx <- 1 + (item-1) %% indexRange_lengths[irIndex]
                    } else {
                        elementIdx <- 1 + (item-1) %/% nestedLengths[irIndex]
                    }
                    index[indexPositions[irIndex]] <- indexRange_getItem(indexingRange$indexRanges[[irIndex]], elementIdx)
                }
                self$calcFun(index)  ## scalar calculation
            }

        }
    )
)


if(FALSE) {

    calcRule <- calcRuleClass$new(quote(y[j,i+1,k]), NULL, context_ijk)
    calcRange <- calcRule$apply(varRangeClass$new(list(
                                                  indexRange(quote(3:5)),
                                                  indexRange(matrix(c(1,2,2,3), ncol = 2))
                                              ), indexOrders = list(2, c(1,3))))
    
    calcRule <- calcRuleClass$new(quote(y[j,i+1,3, k]), NULL, context_ijk)
    calcRange <- calcRule$apply(varRangeClass$new(list(
                                                  indexRange(quote(3:5)),
                                                  indexRange(matrix(c(1,2,2,3), ncol = 2)),
                                                  indexRange(3)
                                              ), indexOrders = list(2, c(1,4), 3)))
}
