nimbleOrRfunctionNames <- c('[',
                            '+',
                            '-',
                            '/',
                            '*',
                            '(',
                            'exp',
                            'log',
                            'pow',
                            '^',
                            '%%',
                            '%*%',
                            't',
                            'equals',
                            'inprod',
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
                            'ceiling',
                            'floor',
                            'round',
                            'nimRound',
                            'trunc',
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
                            'prod',
                            'asRow',
                            'asCol',
                            'chol',
                            'inverse',
                            'forwardsolve',
                            'backsolve',
                            'solve',
                            'nimEigen',
                            'nimSvd',  
                            '>',
                            '<',
                            '>=',
                            '<=',
                            '==',
                            '!=',
                            '&',
                            '|',
                            '$',
                            distributionFuns,
                            # these are allowed in DSL as special
                            # cases even though exp_nimble and
                            # t_nonstandard are the canonical NIMBLE
                            # distribution functions
                            paste0(c('d','r','q','p'), 't'),
                            paste0(c('d','r','q','p'), 'exp'),
                            'nimC', 'nimRep', 'nimSeq', 'diag',
                            'length')

functionsThatShouldNeverBeReplacedInBUGScode <- c(':','nimC','nimRep','nimSeq', 'diag')

getSymbolicParentNodes <- function(code,
                                   constNames = list(),
                                   indexNames = list(),
                                   nimbleFunctionNames = list(),
                                   addDistNames = FALSE) {
    ## We previously propagated a contextID through this
    ## recursive system.  It was used only for a piece of the label
    ## for unknown indices and seemed unnecessary.
    if(addDistNames)
        nimbleFunctionNames <- c(nimbleFunctionNames,
                                 getAllDistributionsInfo('namesExprList'))
    ans <- getSymbolicParentNodesRecurse(code,
                                         constNames,
                                         indexNames,
                                         nimbleFunctionNames)
    return(ans$code)
}

getSymbolicParentNodesRecurse <- function(code,
                                          constNames = list(),
                                          indexNames = list(),
                                          nimbleFunctionNames = list()) {
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
    ##                replacements aren't actually done but are used to
    ##                decide handling.
    ##                Something replaceable doesn't need to become a
    ##                symbolicParentNode.
    ##                Something replaceable in an index represents static
    ##                indexing, not dynamic indexing
    ## - hasIndex: is there an index inside
    ## numeric constant
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
            ## Is this for a blank index?, e.g. from first index of
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
                ## a case like foo(x)[i] (when will this occur in BUGS?), 
              indexingBracket <- FALSE
            } 
        }
        if(indexingBracket) { 
            ## recurse on the index arguments
            contents <-
                lapply(code[-c(1,2)],
                       function(x)
                           getSymbolicParentNodesRecurse(x,
                                                         constNames,
                                                         indexNames,
                                                         nimbleFunctionNames
                                                         )
                       )
            ## unpack the codes returned from recursion
            contentsCode <-
                unlist(
                    lapply(contents,
                           function(x)
                               x$code),
                    recursive = FALSE)
            ## unpack whether each index has an index
            contentsHasIndex <-
                unlist(lapply(contents,
                              function(x) x$hasIndex))
            ## unpack whether each index is replaceable
            contentsReplaceable <-
                unlist(lapply(contents,
                              function(x) x$replaceable))
            ## recuse on the variable, e.g. mu in mu[i]
            variable <-
                getSymbolicParentNodesRecurse(code[[2]],
                                              constNames,
                                              indexNames,
                                              nimbleFunctionNames
                                              )

            ## error if it looks like mu[i][j] where i is a for-loop index
            if(variable$hasIndex)
                stop('Error: Variable',
                     deparse(code[[2]]),
                     'on outside of [ contains a BUGS code index.')
            
            if(variable$replaceable) {
                ## a case like x[ block[i] ], dealing with the
                ## block[i], so block is replaceable
                boolIndexingBlock <-
                    unlist(
                        lapply(code[-c(1,2)],
                               function(x)
                                   if(length(x) > 1)
                                       if(x[[1]] == ':')
                                           TRUE
                                       else
                                           FALSE
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
                ## x[i] with x a variable and no dynamic indices
                if(all(contentsReplaceable)) {
                    return(list(code = c(contentsCode, list(code)),
                                replaceable = FALSE,
                                hasIndex = any(contentsHasIndex)))
                } else { ## non-replaceable indices are dynamic indices
                    if(!nimbleModelOptions('allowDynamicIndexing')) {
                        warning("It appears you are trying to use dynamic indexing (i.e., the index of a variable is determined by something that is not a constant) in: ",
                                deparse(code),
                                ". This is now allowed as of version 0.6-6 (as an optional beta feature) and by default as of version 0.6-7. Please set 'nimbleOptions(allowDynamicIndexing = TRUE)' and report any issues to the NIMBLE users group.")
                        dynamicIndexParent <- code[[2]]
                    } else {
                        if(any(
                            sapply(contentsCode,
                                   detectNonscalarIndex))
                           )
                            stop("getSymbolicParentNodesRecurse: only scalar random indices are allowed; vector random indexing found in ",
                                 deparse(code))
                        indexedVariable <- deparse(code[[2]])
                        dynamicIndexParent <-
                            addUnknownIndexToVarNameInBracketExpr(code)
                        ## Instead of inserting NA, leave indexing
                        ## code but with indication it is a dynamic
                        ## index so we can detect that later. We need
                        ## the indexing code so we can add it to
                        ## declInfo$dynamicIndexInfo for range
                        ## checking.
                        dynamicIndexParent[-c(1, 2)][ !contentsReplaceable ] <- 
                            lapply(dynamicIndexParent[-c(1, 2)][ !contentsReplaceable ],
                                   addDynamicallyIndexedWrapping)
                        contentsCode = lapply(
                            contentsCode,
                            addIndexWrapping)
                    }
                    return(list(code = c(contentsCode,
                                         list(dynamicIndexParent)),
                                replaceable = FALSE,
                                hasIndex = any(contentsHasIndex)))
                }
            }
        } else {
            ## a regular call like foo(x)
            if(cLength > 1) {
                if(code[[1]] == '$') ## a$x: recurse on a 
                    contents <- lapply(
                        code[2],
                        function(x)
                            getSymbolicParentNodesRecurse(x,
                                                          constNames,
                                                          indexNames,
                                                          nimbleFunctionNames
                                                          )
                    )
                else ## foo(x): recurse on x
                    contents <- lapply(
                        code[-1],
                        function(x)
                            getSymbolicParentNodesRecurse(x,
                                                          constNames,
                                                          indexNames,
                                                          nimbleFunctionNames
                                                          )
                    )
                ## unpack results of recursion
                contentsCode <- unlist(
                    lapply(contents,
                           function(x) x$code),
                    recursive = FALSE)
                ## unpack hasIndex entries
                contentsHasIndex <- unlist(
                    lapply(contents,
                           function(x) x$hasIndex)
                )
                ## unpack replaceable entries
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
            ## check if the function can be called only in R, not NIMBLE
            isRfunction <- !any(code[[1]] == nimbleFunctionNames)
            funName <- deparse(code[[1]])
            isRonly <- isRfunction &
                (!checkNimbleOrRfunctionNames(funName))
            ## if it can be called only in R but not all contents are replaceable, generate error:
            if(isRonly & !allContentsReplaceable) {
                if(!exists(funName))
                    stop("R function '", funName,"' does not exist.")
                unreplaceable <-
                    sapply(contents[!contentsReplaceable],
                           function(x) as.character(x$code)
                           )
                stop("R function '",
                     funName,
                     "' has arguments that cannot be evaluated; either the function must be a nimbleFunction or values for the following inputs must be specified as constants in the model: ",
                     paste(unreplaceable, collapse = ","),
                     ".")
            }
            return(list(code = contentsCode,
                        replaceable = allContentsReplaceable & isRfunction,
                        hasIndex = any(contentsHasIndex)))
        }
    }
    stop(paste('Something went wrong in getSymbolicVariablesRecurse with',
               deparse(code)))
}

checkNimbleOrRfunctionNames <- function(functionName) {
    if(any(functionName == nimbleOrRfunctionNames))
        return(TRUE)
    if(exists(functionName) &&
       is.rcf(get(functionName)))
        return(TRUE)  ## Would like to do this by R's scoping rules.
    return(FALSE)
}


addUnknownIndexToVarName <- function(varName,
                                     extraText) {
    paste0(".",
           varName,
           "_unknownIndex_",
           extraText)
    ## We previously appended a contextID to this string
}


addUnknownIndexToVarNameInBracketExpr <- function(parentExpr) {
    parentExpr[[2]] <-
        as.name(
            addUnknownIndexToVarName(parentExpr[[2]],
                                     Rname2CppName(parentExpr))
        )
    parentExpr
}
