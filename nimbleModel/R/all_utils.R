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

Rname2CppName <- function(rName, colonsOK = TRUE, maxLength = 250) {
    ## This will serve to replace and combine our former `Rname2CppName` and `nameMashupFromExpr`,
    ## which were largely redundant
    if (!is.character(rName)) 
        rName <- safeDeparse(rName)

    if( colonsOK) {
        # Substitute single colons but preserve double colons.
        rName <- gsub('::', '_DOUBLE_COLON_', rName)
        rName <- gsub(':', 'to', rName)  # replace colons with 'to'
        rName <- gsub('_DOUBLE_COLON_', '::', rName)
    } else if(grepl(':', rName)) {
        stop("Rname2CppName: cannot generate name from expression with colon (\':\') in `", rName, "`.")
    }
    rName <- gsub(' ', '', rName)
    rName <- gsub('\\.', '_dot_', rName) 
    rName <- gsub("\"", "_quote_", rName)
    rName <- gsub(',', '_comma_', rName)   
    rName <- gsub("`", "_backtick_" , rName)
    rName <- gsub('\\[', '_oB', rName)
    rName <- gsub('\\]', '_cB', rName)
    rName <- gsub('\\(', '_oP', rName)
    rName <- gsub('\\)', '_cP', rName)
    rName <- gsub('\\{', '_oC', rName)
    rName <- gsub('\\}', '_cC', rName)
    rName <- gsub("\\$", "_" , rName)
    rName <- gsub(">=", "_gte_", rName)
    rName <- gsub("<=", "_lte_", rName)
    rName <- gsub("<=", "_eq_", rName)
    rName <- gsub("!=", "_neq_", rName)
    rName <- gsub(">", "_gt_", rName)
    rName <- gsub("<", "_lt_", rName)
    rName <- gsub("!", "_not_", rName)
    rName <- gsub("\\|\\|", "_or2_", rName)
    rName <- gsub("&&", "_and2_", rName)
    rName <- gsub("\\|", "_or_", rName)
    rName <- gsub("&", "_and_", rName)
    rName <- gsub("%%", "_mod_", rName)
    rName <- gsub("%\\*%", "_matmult_", rName)
    rName <- gsub("=", "_eq_" , rName)
    rName <- gsub("\\(", "_" , rName)
    rName <- gsub("\\+", "_plus_" , rName)
    rName <- gsub("-", "_minus_" , rName)
    rName <- gsub("\\*", "_times_" , rName)
    rName <- gsub("/", "_over_" , rName)
    rName <- gsub('\\^', '_tothe_', rName)
    rName <- gsub('^_+', '', rName) # Remove leading underscores, which can arise from, e.g., `(a+b)`.
    rName <- gsub('^([[:digit:]])', 'd\\1', rName)   # If begins with a digit, add 'd' in front.
    rName <- sapply(rName,
                    function(x) {
                        if(nchar(x) > maxLength &&
                           !length(grep("___TRUNC___", x)) &&
                           !length(grep("_Vec$", x))) ## When we add _Vec on we need it to stay on (issue #1216).
                            ## Note this could break if a user has long syntax that ends in _Vec,
                            ## but deal with that if it arises.
                            x <- paste0(substring(x, 1, maxLength), CppNameLabelMaker())
                        return(x)
                    })
    return(rName)    
}


## Simply adds width.cutoff = 500 as the default to deal with creation of long variable names from expressions.
deparse <- function(...) {
    if("width.cutoff" %in% names(list(...))) {
        base::deparse(..., control = "digits17")
    } else {
        base::deparse(..., width.cutoff = 500L, control = "digits17")
    }
}

## This version of deparse avoids splitting into multiple lines, which generally would lead to
## problems. We keep the original nimble:::deparse above as deparse is widely used and there
## are cases where not modifying the nlines behavior may be best. 
safeDeparse <- function(..., warn = FALSE) {
    out <- deparse(...)
    if(TRUE) { ## TODO: nimbleModelOptions('useSafeDeparse')) {
        dotArgs <- list(...)
        if("nlines" %in% names(dotArgs))
            nlines <- dotArgs$nlines else nlines <- 1L
        if(nlines != -1L && length(out) > nlines) {
            if(warn)
                messageIfVerbose("  [Note] safeDeparse: truncating deparse output to ", nlines, " lines.")
            out <- out[1:nlines]
        }
    }
    return(out)
}

#' @export
messageIfVerbose <- function(...) 
    if(getNimbleModelOption('verbose')) message(...)


## Temporarily here as need for initial testing of processing.
is.rcf <- function(x, inputIsName = FALSE, where = -1) {
    if(inputIsName)
        x <- get(x, pos = where)
    if(inherits(x, 'nfMethodRC'))
        return(TRUE)
    if(is.function(x)) {
        if(is.null(environment(x)))
            return(FALSE)
        if(exists('nfMethodRCobject', envir = environment(x), inherits = FALSE))
            return(TRUE)
    }
    return(FALSE)
}

nimbleUniqueID <- labelFunctionCreator("UID")
nimbleModelID  <- labelFunctionCreator("MID")
