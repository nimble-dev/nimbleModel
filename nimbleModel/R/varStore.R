## TODO: do we plan to use this?

## Basic class for storing information about variables, such as sortID, isData, etc.
## The idea is to expand functionality so this can store 'sparse' information
## of various sorts values are repeated in various patterns (e.g., blocks or
## sets of rows or columns or subblocks), but for now only the case of all values
## being the same or full dense storage are handled.

## Expected usage of the class is to ask for a subset of values. Currently if
## all values in the object are equal then the result is a singleton.

## https://stackoverflow.com/questions/37462850/r-r6-operator-overloading

## For now we assume that when user assigns entire object, user wants in form of a
## varStoreClass not the equivalent vector/matrix/array.
varStoreClass <- R6Class(
    classname = "varStoreClass",
    ## R6 documentation seems to indicate a move toward using portable classes
    portable = TRUE,  
    public = list(
        type = NULL,
        dim = NULL,
        allEqual = NULL,
        value = NULL,
        initialize = function(value, dim) {
            self$type <- typeof(value)
            if(length(value) == 1) {
                ## input can sparse, i.e., a single value and dimension
                if(missing(dim)) self$dim <- 1 else self$dim <- dim 
                self$allEqual <- TRUE
                self$value <- value
            } else {
                self$dim <- dimOrLength(value)
                if(!missing(dim) && dim != self$dim)
                    warning("varStoreClass: dimension of input not consistent with 'dim'; ignoring 'dim'.")
                if(nimAllEqual(value)) {  
                    self$allEqual <- TRUE
                    self$value <- value[1]
                } else {
                    self$allEqual <- FALSE
                    self$value <- value
                }
            }
        }
    )
)


## helper functions

# FIXME: remove when we pull this in from current nimble
dimOrLength <- function (obj, scalarize = FALSE) 
{
    if (scalarize) 
        if (length(obj) == 1) 
            return(numeric(0))
    if (is.null(dim(obj))) 
        return(length(obj))
    return(dim(obj))
}

getLength <- function(index) {
    ## find the number of values represented by a single index
    if(is.numeric(index)) return(1)
    if(length(index) > 1 && index[1] == ':') 
        return(eval(index[3]) - eval(index[2]) + 1)
    return(length(eval(index)))
}
    
getMaxIndex <- function(index) {
    ## find the largest value represented by a single index
    if(is.numeric(index)) return(index)
    if(length(index) > 1 && index[1] == ':') return(eval(index[3]))
    return(max(eval(index)))
}

insertSequence <- function(dimExtent)
    ## creates expressions 1:n where `n` is the size of a dimension
    return(parse(text = paste0("1:", dimExtent)))

expandValues <- function(value, dim) {
    if(length(dim) == 1)
        return(rep(value, dim))
    if(length(dim) == 2)
        return(matrix(value, nrow = dim[1], ncol = dim[2]))
    return(array(value, dim))
}

nimAllEqual <- function(value) {
    if(length(value) == 1) return(TRUE)
    ## First condition is quick check of a necessary condition.
    if(value[1] == value[2]) 
        ## need c() for matrix/array cases
        return(length(unique(c(value))) == 1) else return(FALSE)
}


## Note that it's tricky to deal with missing indices when these are elements of ...
## can explicitly do missing(..1) but not sure how to do that programmatically
## with missing elements, using list(...) fails. Therefore code here manipulates
## the argument list as code.

## FIXME: 'drop' argument not implemented
`[.varStoreClass` = function(self, ..., expand = FALSE) {
    if(self$allEqual) {
        args <- match.call(expand.dots = TRUE)[-1]
        indices <- as.list(args[names(args) == ""])  # unnamed arguments are indices from ...
        indices[indices == ""] <- sapply(self$dim, insertSequence)[indices == ""] # insert 1:n for missing indices
        ## FIXME: deparse(match.call()) prints out as function call not as user-friendly [ operator
        if(length(indices) > 1 && length(indices) != length(self$dim))
            stop("Error in ", deparse(match.call()), ": incorrect number of dimensions")
        if(any(sapply(indices, getMaxIndex) > self$dim))
            stop("Error in ", deparse(match.call()), ": subscript out of bounds")
        if(!expand) {
            return(self$value)
        } else {
            dim <- sapply(indices, getLength) # determine dimension of requested subset
            return(expandValues(self$value, dim))
        }
    } else {  # in simple case of fully-expanded structure, just use base R
        tmp <- `[`(self$value, ...)
        if(nimAllEqual(tmp)) return(tmp[1]) else return(tmp)
    }
}


`[<-.varStoreClass` = function(self, ..., value) {
    if(typeof(value) != self$type)
        stop("varStoreClass: input type is not the same as the stored type.")

    args <- match.call(expand.dots = TRUE)[-1]
    indices <- as.list(args[names(args) == ""])  # unnamed arguments are indices from ...
    indices[indices == ""] <- sapply(self$dim, insertSequence)[indices == ""] # insert 1:n for missing indices
    if(length(indices) > 1 && length(indices) != length(self$dim))
       stop("Error in ", deparse(match.call()), ": incorrect number of dimensions")
    if(any(sapply(indices, getMaxIndex) > self$dim))
        stop("Error in ", deparse(match.call()), ": subscript out of bounds")

    if(!(self$allEqual && value[1] == self$value && length(unique(c(value))) == 1)) {
        if(self$allEqual) {
            self$allEqual <- FALSE
            self$value <- expandValues(self$value, self$dim)
        }
        self$value <- `[<-`(self$value, ..., value)
        ## We may not want to check if new version is allEqual given computational time
        ## but for now we include it. First condition is quick necessary condition.
        if((length(value) == 1 || value[1] == value[2]) && nimAllEqual(self$value)) {
            self$allEqual <- TRUE
            self$value <- value[1]
        }
    }
    return(self)
}

