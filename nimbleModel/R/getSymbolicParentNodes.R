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

nimbleOrRfunctionNames <- c('[', '(',
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

functionsThatShouldNeverBeReplacedInModelCode <- c(':','nimC','nimRep','nimSeq', 'diag')

getSymbolicParentNodes <- function(code,
                                   constNames = list(),
                                   indexNames = list(),
                                   nimbleFunctionNames = list(),
                                   addDistNames = FALSE,
                                   envir = .GlobalEnv) {
    ## We previously propagated a contextID through this
    ## recursive system.  It was used only for a piece of the label
    ## for unknown indices and seemed unnecessary.
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
                                          envir = .GlobalEnv) {
    ## This takes as input some code and returns the variables in it.
    ## It expects one line of code, not a '{' expression.
    ##
    ## However, indexNames (from for-loop indices) and constNames are
    ## not identified as separate variables.  e.g. x[i] is returned as
    ## 'x' and 'i' or as 'x[i]' if i is an indexName
    ##
    ## indexNames and constNames can be substituted at compile time,
    ## such as a block index variable.
    ##
    ## Every function EXCEPT those in nimbleFunctionNames can be
    ## evaluated at compile time.
    ##
    ## constNames, indexNames and nimbleFunctionNames should be lists
    ## of names.
    ##
    ## details: each recursion returns a list with:
    ## - code: a list of symbolicParentExprs
    ## - replaceable: logical of whether it can be part of a partially
    ##                evaluated expression.
    ##                This includes numbers, constants, indices and
    ##                functions that can be evaluated in R.
    ##                Replacements aren't actually done but are used to
    ##                decide handling.
    ##                Something replaceable doesn't need to become a
    ##                symbolicParentNode.
    ##                Something replaceable in an index represents static
    ##                indexing, not dynamic indexing
    ## - hasIndex: is there an index inside numeric constant
    if(is.numeric(code) ||
       (nimbleModelOptions('allowDynamicIndexing') &&
                       length(code) > 1 &&
                       code[[1]] == ".DYN_INDEXED")
       ) 
        ## Check for .DYN_INDEXED deals with processing of code when
        ## we add unknownIndex declarations.
        return(list(code = NULL,
                    replaceable = TRUE,
                    hasIndex = FALSE))
    cLength <- length(code)
    ## a single name:
    if(cLength == 1) {
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
                                                         envir
                                                         )
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
                                              envir
                                              )

            ## Error if it looks like mu[i][j] where i is a for-loop index
            if(variable$hasIndex)
                stop("getSymbolicParentNodesRecurse: variable `",
                     deparse(code[[2]]),
                     '` on outside of [ contains an index.')
            
            if(variable$replaceable) {
                ## A case like `x[ block[i] ]`, dealing with the
                ## `block[i]`, so `block` is replaceable.
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
                    if(!nimbleModelOptions('allowDynamicIndexing')) {
                        warning("It appears you are trying to use dynamic indexing (i.e., the index of a variable is determined by something that is not a constant) in: ",
                                deparse(code),
                                ". Please set `nimble::nimbleOptions(allowDynamicIndexing = TRUE)`.")
                        dynamicIndexParent <- code[[2]]
                    } else {
                        if(any(sapply(contentsCode, detectNonscalarIndex)))
                            stop("getSymbolicParentNodesRecurse: only scalar random indices are allowed; vector random indexing found in `", deparse(code), "`.")
                        indexedVariable <- deparse(code[[2]])
                        dynamicIndexParent <- deparse(code) # addUnknownIndexToVarNameInBracketExpr(code)
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
            if(cLength > 1) {
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
            funName <- deparse(code[[1]])
            isRonly <- isRfunction & (!checkNimbleOrRfunctionNames(funName, envir))
            ## If it can be called only in R but not all contents are replaceable, generate error.
            if(isRonly & !allContentsReplaceable) {
                if(!exists(funName))
                    stop("getSymbolicParentNodesRecurse: R function `", funName, "` does not exist.")
                unreplaceable <-
                    sapply(contents[!contentsReplaceable],
                           function(x) as.character(x$code)
                           )
                stop("getSymbolicParentNodesRecurse: R function `", funName,
                     "` has arguments that cannot be evaluated; either the function must be a nimbleFunction or values for the following inputs must be specified as constants in the model: `",
                     paste(unreplaceable, collapse = ","),
                     "`.")
            }
            return(list(code = contentsCode,
                        replaceable = allContentsReplaceable & isRfunction,
                        hasIndex = any(contentsHasIndex)))
        }
    }
    stop("getSymbolicParentNodesRecurse: `", deparse(code), "` cannot be evaluated.")
}

checkNimbleOrRfunctionNames <- function(functionName, envir) {
    if(any(functionName == nimbleOrRfunctionNames))
        return(TRUE)
    if(exists(functionName, envir) && is.rcf(get(functionName, envir)))
        return(TRUE)  ## FUTURE: Would like to do this by R's scoping rules.
    return(FALSE)
}


## CHECK: these next two fxns may not be needed anymore.
addUnknownIndexToVarName <- function(varName,
                                     extraText) {
    return(
        paste0(".",
               varName,
               "_unknownIndex_",
               extraText)
    )
    ## We previously appended a contextID to this string.
}


addUnknownIndexToVarNameInBracketExpr <- function(parentExpr) {
    parentExpr[[2]] <-
        as.name(
            addUnknownIndexToVarName(parentExpr[[2]],
                                     Rname2CppName(parentExpr))
        )
    return(parentExpr)
}

detectNonscalarIndex <- function(expr) {
    if(usedInIndex(expr) || length(expr) == 1)
        return(FALSE)  ## The condition is needed because recursion
                       ## means that we might already have processed
                       ## the dynamic index.
    if(length(expr) == 2) {  ## This can occur if we have mu[k[j[i]]]
        expr <- stripIndexWrapping(expr)
        if(length(expr) <= 2)
            stop("detectNonscalarIndex: unexpected expression `", deparse(expr), "`.")
    }
    return(
        any(sapply(expr[3:length(expr)], isVectorIndex))
    )
}

usedInIndex <- function(expr)
    return(length(expr) > 1 && expr[[1]] == ".USED_IN_INDEX")

isDynamicIndex <- function(expr) {
    return(
    (length(expr) > 1 && expr[[1]] == ".DYN_INDEXED") ||
    identical(expr, quote(NA_real_))
    )
}

stripIndexWrapping <- function(expr) { 
    if(length(expr) == 1 || !usedInIndex(expr))
        return(expr)
    else
        return(expr[[2]])
}

isVectorIndex <- function(expr) {
    if(isDynamicIndex(expr))
        return(FALSE)
    if(length(expr) > 1 && expr[[1]] == ":")
        return(TRUE)
    return(FALSE)
}

addIndexWrapping <- function(expr) {
    if(length(expr) > 1 && expr[[1]] == '.USED_IN_INDEX') ## nested random indexing
        return(expr)
    return(substitute(.USED_IN_INDEX(EXPR), list(EXPR = expr)))
}

addDynamicallyIndexedWrapping <- function(expr) {
    return(substitute(.DYN_INDEXED(EXPR), list(EXPR = expr)))
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


# Simply adds width.cutoff = 500 as the default to deal with creation of long variable names from expressions.
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
    if(TRUE) { ## TODO: nimble::nimbleOptions('useSafeDeparse')) {
        dotArgs <- list(...)
        if("nlines" %in% names(dotArgs))
            nlines <- dotArgs$nlines else nlines <- 1L
        if(nlines != -1L && length(out) > nlines) {
            if(warn)
                message("  [Note] safeDeparse: truncating deparse output to ", nlines, " lines.")
            out <- out[1:nlines]
        }
    }
    return(out)
}
