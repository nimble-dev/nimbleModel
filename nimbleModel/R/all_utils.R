dimOrLength <- function(obj, scalarize = FALSE) {
    if(scalarize) if(length(obj) == 1) return(integer(0))
    if(is.null(dim(obj))) return(length(obj))
    return(dim(obj))
}

## Sequential label generation system:
## labelFunctionMetaCreator returns a function that returns a function.
## labelFunctionMetaCreator is only called once, immediately below, to create labelFunctionCreator
## The outer layer allows allLabelFunctionCreators to be in the closure of every function returned
## by labelFunctionCreator.  Each of those functions is registered as an element of allLableFunctionCreators.
## 
## This scheme allows the function resetLabelFunctionCreators below to work simply,
## resetting the count to 1 for all of the label generators.
##
## The motivation for resetLabelFunctionCreators is for testing: If we want to check
## that two pathways to code generation (one existing, one experimental) create identical
## code, it is helpful to have identical generated labels.  Resetting all label generators
## supports this goal.
labelFunctionMetaCreator <- function() {
    allLabelFunctionCreators <- list()

    creatorFun <- function(lead, start = 1) {
        nextIndex <- start
        force(lead)
        labelGenerator <- function(reset = FALSE, count = 1, envName = "") {
            if(reset) {
                nextIndex <<- 1
                return(invisible(NULL))
            }
            lead <- paste(lead, envName , sep = '_')
            ans <- paste0(lead, nextIndex - 1 + (1:count))
            nextIndex <<- nextIndex + count
            ans
        }
        allLabelFunctionCreators[[ length(allLabelFunctionCreators) + 1 ]] <<- labelGenerator
        labelGenerator
    }
    creatorFun
}

labelFunctionCreator <- labelFunctionMetaCreator()

resetLabelFunctionCreators <- function() {
    allLabelFunctionCreators <- environment(labelFunctionCreator)$allLabelFunctionCreators
    for(i in allLabelFunctionCreators) {
        i(reset = TRUE)
    }
}

#' @export
messageIfVerbose <- function(...) 
    if(getNimbleModelOption('verbose')) message(...)

nimbleUniqueID <- labelFunctionCreator("UID")
nimbleModelID  <- labelFunctionCreator("MID")
