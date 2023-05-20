nimblePreevaluationFunctionNames <- c('+',
                                      '-',
                                      '/',
                                      '*',
                                      'exp',
                                      'log',
                                      'pow',
                                      '^',
                                      '%%',
                                      'equals',
                                      'nimEquals',
                                      'sqrt',
                                      'logit',
                                      'expit',
                                      'ilogit',
                                      'probit',
                                      'iprobit',
                                      'phi',
                                      'cloglog',
                                      'icloglog',
                                      'step',
                                      'nimStep',
                                      'sin',
                                      'cos',
                                      'tan',
                                      'asin',
                                      'acos',
                                      'atan',
                                      'cosh',
                                      'sinh',
                                      'tanh',
                                      'asinh',
                                      'acosh',
                                      'atanh',
                                      'cube',
                                      'abs',
                                      'lgamma',
                                      'loggam',
                                      'log1p',
                                      'lfactorial',
                                      'besselK',
                                      'ceiling',
                                      'floor',
                                      'round',
                                      'nimRound',
                                      'trunc',
                                      '>',
                                      '<',
                                      '>=',
                                      '<=',
                                      '==',
                                      '!=',
                                      '[',
                                      '(',
                                      '%*%',
                                      't',
                                      'inprod',
                                      'optim',
                                      'nimOptim',
                                      'optimDefaultControl',
                                      'nimOptimDefaultControl',
                                      'mean',
                                      'sum',
                                      'sd',
                                      'var',
                                      'max',
                                      'min',
                                      'pmin',
                                      'pmax',
                                      'prod',
                                      'asRow',
                                      'asCol',
                                      'logdet',    
                                      'chol',
                                      'inverse',
                                      'forwardsolve',
                                      'backsolve',
                                      'solve',
                                      'nimEigen',
                                      'nimSvd',  
                                      '&',
                                      '|',
                                      '$',
                                      det_distributionFuns,
                                        # these are allowed in DSL as special
                                        # cases even though exp_nimble and
                                        # t_nonstandard are the canonical NIMBLE
                                        # distribution functions
                                      paste0(c('d','q','p'), 't'),
                                      paste0(c('d','q','p'), 'exp'),
                                      'nimC', 'nimRep', 'nimSeq', 'diag',
                                      'nimNumeric','nimMatrix','nimArray',
                                      'length'
                                      )

nimbleOrRfunctionNames <- c(':', '[', '(',
                            '+', '-', '/', '*',
                            'exp', 'log',
                            'pow', '^',
                            '%%', '%*%',
                            't',
                            'equals', 'nimEquals',
                            'inprod',
                            'sqrt',
                            'logit', 'expit', 'ilogit',
                            'probit', 'iprobit',
                            'phi',
                            'cloglog', 'icloglog',
                            'step', 'nimStep',
                            'sin', 'cos', 'tan',
                            'asin', 'acos', 'atan',
                            'cosh', 'sinh', 'tanh',
                            'asinh', 'acosh', 'atanh',
                            'cube',
                            'abs',
                            'lgamma', 'loggam',
                            'log1p',
                            'lfactorial',
                            'ceiling', 'floor',
                            'round', 'nimRound',
                            'trunc',
                            'optim', 'nimOptim',
                            'optimDefaultControl', 'nimOptimDefaultControl',
                            'mean',
                            'sum', 'prod',
                            'sd', 'var',
                            'max', 'min',
                            'asRow', 'asCol',
                            'chol',
                            'inverse',
                            'forwardsolve', 'backsolve', 'solve',
                            'nimEigen', 'nimSvd',  
                            '>', '<', '>=', '<=',
                            '==', '!=',
                            '&', '|',
                            '$',
                            distributionFuns,
                            # these are allowed in DSL as special
                            # cases even though exp_nimble and
                            # t_nonstandard are the canonical NIMBLE
                            # distribution functions
                            paste0(c('d','r','q','p'), 't'),
                            paste0(c('d','r','q','p'), 'exp'),
                            'nimC', 'nimRep', 'nimSeq',
                            'diag',
                            'length'
                            )


## Determine the RHS pieces, expressed symbolically.
getSymbolicParentNodes <- function(code,
                                   constNames = list(),
                                   indexNames = list(),
                                   nimbleFunctionNames = list(),
                                   addDistNames = FALSE,
                                   envir) {
    if(addDistNames)
        nimbleFunctionNames <- c(nimbleFunctionNames,
                                 getAllDistributionsInfo('namesExprList'))
    ans <- getSymbolicParentNodesRecurse(code,
                                         constNames,
                                         indexNames,
                                         nimbleFunctionNames,
                                         envir)
    return(ans$code)
}

getSymbolicParentNodesRecurse <- function(code,
                                          constNames = list(),
                                          indexNames = list(),
                                          nimbleFunctionNames = list(),
                                          envir) {
    ## This takes as input some code and returns the variables in it.
    ## It expects one line of code, not a `{` expression.
    ##
    ## However, `indexNames` (from for-loop indices) and `constNames` are
    ## not identified as separate variables.  E.g., `x[i]` is returned as
    ## `x` and `i` or as `x[i]` if `i` is an indexName
    ##
    ## `indexNames` and `constNames` can be substituted at compile time,
    ## such as a block index variable.
    ##
    ## Every function EXCEPT those in `nimbleFunctionNames` can be
    ## evaluated at compile time.
    ##
    ## `constNames`, `indexNames` and `nimbleFunctionNames` should be lists
    ## of names.
    ##
    ## details: each recursion returns a list with:
    ## - `code`: a list of `symbolicParentExprs`
    ## - `replaceable`: logical of whether it can be part of a partially
    ##                evaluated expression.
    ##                This includes numbers, constants, and indices.
    ##                This also used to include functions that can be evaluated in R,
    ##                but since we are no longer unrolling, we can no longer
    ##                precompute the results for eval'ing R-only functions.
    ##                Replacements aren't actually done but are used to
    ##                decide handling.
    ##                Something replaceable doesn't need to become a
    ##                `symbolicParentNode`.
    ##                Something replaceable in an index represents static
    ##                indexing, not dynamic indexing
    ## - `hasIndex`: is there an index inside numeric constant
    if(is.numeric(code))
        return(list(code = NULL,
                    replaceable = TRUE,
                    hasIndex = FALSE))
    ## a single name:
    if(length(code) == 1) {
        if(is.name(code)) {
            ## Is this for a blank index? E.g., from first index of
            ## x[, j].  At this point indices have been filled so
            ## there shouldn't be blanks.
            if(code == ''){
                return(list(code = NULL,
                            replaceable = TRUE,
                            hasIndex = FALSE))
            }
            ## an index name
            if(any(code == indexNames)) {
                return(list(code = NULL,
                            replaceable = TRUE,
                            hasIndex = TRUE))
            }
            ## a constant name
            if(any(code == constNames)) {
                return(list(code = NULL,
                            replaceable = TRUE,
                            hasIndex = FALSE))
            }
            ## just something regular: not constant or index
            return(list(code = list(code),
                        replaceable = FALSE,
                        hasIndex = FALSE))
        }
    }
    ## a call:
    if(is.call(code)) {
        indexingBracket <- code[[1]] == '['
        if(indexingBracket) {
            if(is.call(code[[2]])){
                ## A case like foo(x)[i] (when will this occur in model code?). 
                indexingBracket <- FALSE
            } 
        }
        if(indexingBracket) { 
            ## Recurse on the index arguments.
            contents <-
                lapply(code[-c(1,2)],
                       function(x)
                           getSymbolicParentNodesRecurse(x,
                                                         constNames,
                                                         indexNames,
                                                         nimbleFunctionNames,
                                                         envir)
                       )
            ## Unpack the codes returned from recursion.
            contentsCode <-
                unlist(lapply(contents,
                           function(x) x$code),
                    recursive = FALSE)
            ## Unpack whether each index has an index.
            contentsHasIndex <-
                unlist(lapply(contents,
                              function(x) x$hasIndex))
            ## Unpack whether each index is replaceable.
            contentsReplaceable <-
                unlist(lapply(contents,
                              function(x) x$replaceable))
            ## Recurse on the variable, e.g., `mu` in `mu[i]`.
            variable <-
                getSymbolicParentNodesRecurse(code[[2]],
                                              constNames,
                                              indexNames,
                                              nimbleFunctionNames,
                                              envir)

            ## Error if it looks like mu[i][j] where i is a for-loop index
            if(variable$hasIndex)
                stop("getSymbolicParentNodesRecurse: variable `",
                     safeDeparse(code[[2]]),
                     '` on outside of [ contains an index.')
            
            if(variable$replaceable) {
                ## A case like `x[ block[i] ]`, dealing with the
                ## `block[i]`, so `block` is replaceable.
                if(!all(contentsReplaceable)) 
                    ## dynamic index on a constant
                    stop('getSymbolicParentNodesRecurse: dynamic indexing of constants is not allowed in `',
                         safeDeparse(code), '`.')
                boolIndexingBlock <-
                    unlist(lapply(code[-c(1,2)],
                               function(x)
                                   if(length(x) > 1)
                                       if(x[[1]] == ':') TRUE else FALSE
                                   else
                                       FALSE)
                    )
                
                if(any(boolIndexingBlock)) {
                    return(list(code = c(contentsCode, code),
                                replaceable = FALSE,
                                hasIndex = any(contentsHasIndex)))
                } else {
                    return(list(code = contentsCode, 
                                replaceable = all(contentsReplaceable),
                                hasIndex = any(contentsHasIndex)))
                }
            } else {
                ## `x[i]` with `x` a variable and no dynamic indices.
                if(all(contentsReplaceable)) {
                    return(list(code = c(contentsCode, list(code)),
                                replaceable = FALSE,
                                hasIndex = any(contentsHasIndex)))
                } else { ## Non-replaceable indices are dynamic indices.
                    if(!getNimbleModelOption('allowDynamicIndexing')) {
                        warning("It appears you are trying to use dynamic indexing (i.e., the index of a variable is determined by something that is not a constant) in: ",
                                safeDeparse(code),
                                ". Please set `nimbleModelOptions(allowDynamicIndexing = TRUE)`.")
                        dynamicIndexParent <- code[[2]]
                    } else {
                        if(any(sapply(contentsCode, detectNonscalarIndex)))
                            stop("getSymbolicParentNodesRecurse: only scalar random indices are allowed; vector random indexing found in `", safeDeparse(code), "`.")
                        indexedVariable <- safeDeparse(code[[2]], warn = TRUE)
                        dynamicIndexParent <- code # addUnknownIndexToVarNameInBracketExpr(code)
                        ## Instead of inserting NA, leave indexing
                        ## code but with indication it is a dynamic
                        ## index so we can detect that later. We need
                        ## the indexing code so we can add it to
                        ## declInfo$dynamicIndexInfo for range checking.
                        dynamicIndexParent[-c(1, 2)][ !contentsReplaceable ] <- 
                            lapply(dynamicIndexParent[-c(1, 2)][ !contentsReplaceable ],
                                   addDynamicallyIndexedWrapping)
                        contentsCode = lapply(contentsCode, addIndexWrapping)
                    }
                    return(list(code = c(contentsCode, list(dynamicIndexParent)),
                                replaceable = FALSE,
                                hasIndex = any(contentsHasIndex)))
                }
            }
        } else {
            ## a regular call like foo(x)
            if(length(code) > 1) {
                if(code[[1]] == '$') ## `a$x`: recurse on `a`. 
                    contents <- lapply(code[2],
                                       function(x)
                                           getSymbolicParentNodesRecurse(x,
                                                                         constNames,
                                                                         indexNames,
                                                                         nimbleFunctionNames,
                                                                         envir)
                                       )
                else ## foo(x): recurse on x
                    contents <- lapply(code[-1],
                                       function(x)
                                           getSymbolicParentNodesRecurse(x,
                                                                         constNames,
                                                                         indexNames,
                                                                         nimbleFunctionNames,
                                                                         envir)
                                       )
                ## Unpack results of recursion.
                contentsCode <- unlist(
                    lapply(contents,
                           function(x) x$code),
                    recursive = FALSE)
                ## Unpack `hasIndex` entries.
                contentsHasIndex <- unlist(
                    lapply(contents,
                           function(x) x$hasIndex)
                )
                ## Unpack replaceable entries.
                contentsReplaceable <- unlist(
                    lapply(contents,
                           function(x) x$replaceable)
                )
                allContentsReplaceable <- all(contentsReplaceable)
            } else { ## no arguments: foo()
                contentsCode <- NULL
                contentsHasIndex <- FALSE
                allContentsReplaceable <- TRUE
            }
            ## Check if the function can be called only in R, not NIMBLE.
            isRfunction <- !any(code[[1]] == nimbleFunctionNames)
            funName <- safeDeparse(code[[1]], warn = TRUE)
            isRonly <- isRfunction && (!checkNimbleOrRfunctionNames(funName, envir))
            ## We can no longer handle R functions, even if all contents replaceable because we no longer unroll.
            if(isRonly) 
                stop("getSymbolicParentNodesRecurse: Detected use of an R function `", funName,
                     "`. This function cannot be compiled; it must be a nimbleFunction.")
            return(list(code = contentsCode,
                        replaceable = allContentsReplaceable & isRfunction,
                        hasIndex = any(contentsHasIndex)))
        }
    }
    stop("getSymbolicParentNodesRecurse: `", safeDeparse(code), "` cannot be evaluated.")
}

checkNimbleOrRfunctionNames <- function(functionName, envir) {
    if(any(functionName == nimbleOrRfunctionNames))
        return(TRUE)
    ## This is scoped to only look in environment from which model is being created.
    if(exists(functionName, envir) && is.rcf(get(functionName, envir)))
        return(TRUE)  
    return(FALSE)
}

detectNonscalarIndex <- function(expr) {
    if(isUsedInIndex(expr) || length(expr) == 1)
        return(FALSE)  ## The condition is needed because recursion
                       ## means that we might already have processed
                       ## the dynamic index.
    if(length(expr) == 2) {  ## This can occur if we have mu[k[j[i]]]
        expr <- stripIndexWrapping(expr)
        if(length(expr) <= 2)
            stop("detectNonscalarIndex: unexpected expression `", safeDeparse(expr), "`.")
    }
    return(
        any(sapply(expr[3:length(expr)], isVectorIndex))
    )
}

isVectorIndex <- function(expr) {
    if(isDynamicIndex(expr))
        return(FALSE)
    if(length(expr) > 1 && expr[[1]] == ":")
        return(TRUE)
    return(FALSE)
}

## Functionality used for handling dynamic indexing during model definition processing.

addIndexWrapping <- function(expr) {
    if(length(expr) > 1 && expr[[1]] == '.USED_IN_INDEX') ## nested random indexing
        return(expr)
    return(substitute(.USED_IN_INDEX(EXPR), list(EXPR = expr)))
}

addDynamicallyIndexedWrapping <- function(expr) {
    return(substitute(.DYN_INDEXED(EXPR), list(EXPR = expr)))
}

isUsedInIndex <- function(expr)
    return(length(expr) > 1 && expr[[1]] == ".USED_IN_INDEX")

isDynamicIndex <- function(expr) {
    return(length(expr) > 1 && expr[[1]] == ".DYN_INDEXED")
}

detectDynamicIndices <- function(expr) {
    if(length(expr) == 1 || expr[[1]] != "[")
        return(FALSE)
    return(sapply(expr[3:length(expr)], isDynamicIndex))
}

expandDynamicIndex <- function(expr) {
    if(length(expr) > 1 && expr[[1]] == ".DYN_INDEXED")
        return(substitute(1:M, list(M = .Machine$integer.max))) else return(expr)
}

stripIndexWrapping <- function(expr) { 
    if(length(expr) == 1 || !isUsedInIndex(expr))
        return(expr)
    else
        return(expr[[2]])
}

