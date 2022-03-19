## Note: still unclear on when nodeFuns will be generated and where stored.

calcRangeClass <- R6Class(
    classname = "calcRangeClass",
    portable = FALSE,
    public = list(
        varName = NULL,
        indexingRange = NULL,
        nodeFun = NULL,
        sortID = NULL,
        initialize = function(varName, indexingRange, context, decl, sortID) {
            varName <<- varName
            ## But, could have calcRange for y[1:3] and y[2:5]; so don't need
            ## separate nodeFuns? is the actual indexing internal to the nodeFun
            ## or passed in?
            indexingRange <<- indexingRange
            
            ## check type of indexingRange to see if need to generate?
            if(FALSE) 
                nodeFun <<- genNodeFun(varName, indexingRange, context, decl)
            sortID <<- sortID
        }
    )
)

## have list of nodeFuns and generate as needed; check first if it exists?
