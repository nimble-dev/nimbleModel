## holds a collection of like nodes (same declaration, same calcRule, possibly same graph role (e.g., latent, top) and possibly same sort ID (need to think about state space case)

## example: y[i, 1:5] ~ dmnorm() has:
## varRange: first index, over selected nodes
## internalRange: 1:5

## Q: should we also have a nodeElementClass that can hold collection of scalar elements? or is it just a special type of nodeRangeClass?

nodeRangeClass <- R6Class(
    classname = "nodeRangeClass",
    portable = FALSE,
    public = list(
        varName = NULL,
        nodeRange = NULL,       # a varRange
        internalRange = NULL,  # a varRange
        indexID_2_rangeID = NULL,    
        rangeID_2_indexID = NULL,    
        nodeType = NULL,        # 'latent', 'top', etc.
        calcRule = NULL,  ## useful?
        nodeFunType = NULL,  ## useful?

        initialize = function(varName,
                              nodeRange,
                              internalRange,
                              index2setID) {
            varName <<- varName
            nodeRange <<- nodeRange
            internalRange <<- internalRange

            ## These apply to the combination of the nodeRange and internalRange;
            ## internalRange indexRanges are considered to be last.
            indexID_2_rangeID <<- index2setID
            indexID_2_rangeID[indexID_2_rangeID != 0] <<- nodeRange$indexID_2_rangeID
            indexID_2_rangeID[indexID_2_rangeID == 0] <<- internalRange$indexID_2_rangeID +
                length(nodeRange$indexRanges)
            rangeID_2_indexID <<- lapply(seq_len(max(indexID_2_rangeID)),
                                         function(x) which(indexID_2_rangeID == x))
        },

        getVarRange = function() {
            ## Extract varRange (i.e., ignoring node structure) for use with methods that apply
            ## to varRanges.
            varRangeClass$new(indexInfo = c(nodeRange$indexRanges, internalRange$indexRanges),
                              indexOrders = rangeID_2_indexID)
            
        },
        
        expandNames = function() {
            ## Expand nodeRange into full matrix (crossed if necessary), keeping full internal
            ## indexing for each individual node.
            nc <- length(index2setID)  # might be, e.g., 0 1 0 2 3 or 0 1 0 2 1
            str <- paste0(varName, "[")

            nodeInfo <- lapply(nodeRange$indexRanges, function(x) {
                if(identical(attr(x, 'rangeType'), 'sequence'))
                    result <- seq(x[[1]][[1]], x[[1]][[2]]) else result <- x[[1]]
                if(!is.matrix(result)) result <- matrix(result, ncol = 1)
                return(result)
            })
            expanded <- do.call(expand.grid, lapply(nodeInfo, function(x) {
                if(is.matrix(x)) 1:nrow(x) else 1:length(x)
            }))

            internalInfo <- lapply(internalRange$indexRanges, function(x) {
                if(identical(attr(x, 'rangeType'), 'sequence')) return(deparse(substitute(X:Y, list(X = x[[1]][[1]], Y = x[[1]][[2]]))))
                return(x)
            })

            ## Mark column position within indexRanges of the nodeRange.
            colID <- as.list(rep(1, length(nodeInfo)))
            ## Mark position within internal- and node-related indexes.
            idxInternal <- 1
            idxNode <- 1
            
            for(i in 1:nc) {
                if(i > 1)
                    str <- paste0(str, ", ")
                if(index2setID[i] == 0) {
                    str <- paste0(str, internalInfo[[idxInternal]])
                    idxInternal <- idxInternal + 1
                } else {
                    rangeIdx <-  nodeRange$indexID_2_rangeID[idxNode]  ## which indexRange is being used
                    str <- paste0(str, nodeInfo[[rangeIdx]][expanded[ , rangeIdx], colID[[rangeIdx]]])
                    colID[[rangeIdx]] <- colID[[rangeIdx]] + 1  ## index through columns of the indexRange_matrix 
                    idxNode <- idxNode + 1
                }
            }
            str <- paste0(str, "]")
            return(str)
        }
    )
)
        
  
