## This file contains updated versions of nimble's contexts.
##
## A context for a model declaration is defined by the for-loops
## enclosing the declaration.
##

## FUTURE: we have no tests for contexts. Given this code is ported from original
## nimble, that may not be much of an issue.

## quick proxy to let code run without sifting through existing getNimbleOption calls
getNimbleOption <- function(...) FALSE

## A class for the information in one for-loop.

singleContextClass <- R6Class(
    classname = "singleContextClass",
    portable = FALSE,
    public = list(
        indexVarExpr = NULL,
        indexRangeExpr = NULL,
        forCode = NULL,

        initialize = function(indexVarExpr, indexRangeExpr, forCode) {
            if(missing(forCode)) {
                if(missing(indexVarExpr) || missing(indexRangeExpr))
                    stop("singleContext: either `forCode` or both `indexVarExpr` and `indexRangeExpr` must be provided.")
                forCode <<- substitute(for(III in VVV) {},
                                       list(III = indexVarExpr,
                                            VVV = indexRangeExpr))[1:3]
                indexVarExpr <<- indexVarExpr
                indexRangeExpr <<- indexRangeExpr
            } else {
                if(missing(indexVarExpr)) {
                    indexVarExpr <<- forCode[[2]]
                } else {
                    if(!identical(indexVarExpr, forCode[[2]]))
                        stop("singleContext: `indexVarExpr` does not match the index variable in `forCode`.")
                }
                if(missing(indexRangeExpr)) {
                    indexRangeExpr <<- forCode[[3]]
                } else {
                    if(!identical(indexRangeExpr, forCode[[3]]))
                        stop("singleContext: `indexRangeExpr` does not match the index range in `forCode`.")
                }
                ## Remove any code from body of loop.
                if(length(forCode) > 3) forCode <<- forCode[1:3]
            }
        }
    )
)

## A class for the information in multiple for-loops (multiple single contexts).
modelContextClass <- R6Class(
        classname = 'modelContextClass',
        portable = FALSE,
        public = list(
            singleContexts = list(),      ## list of `singleContext` objects
            indexVarExprs = list(),       ## list of index variable expressions
            
            indexVarNames = character(),  ## vector of index variable names
            
            ## Sets all fields, which never change.
            initialize = function(singleContexts) {
                singleContexts <- lapply(singleContexts,
                                         function(x)
                                             if(is.call(x)) {
                                                 singleContextClass$new(forCode = x)
                                             } else if(is(x, "singleContextClass")) {
                                                 x
                                             } else stop("modelContextClass: `singleContexts` must be a list of `singleContextClass` objects or a list of for-loop code.")
                                         )
                singleContexts <<- singleContexts
                indexVarExprs <<- lapply(singleContexts,
                                         function(x) x$indexVarExpr)
                indexVarNames <<- if(length(indexVarExprs))
                                      unlist(lapply(indexVarExprs, as.character))
                                  else character(0)
                names(singleContexts) <<- indexVarNames
            },
            genIndexVarValues = function(constantsEnvCopy) {
                genIndexVarValues_recurse(singleContexts, constantsEnvCopy)
            },
            embedCodeInForLoop = function(innerLoopCode,
                                          useContext = NULL,
                                          allowNegativeIndexSequences = NULL) {
                modelContextClass_embedCodeInForLoop(singleContexts,
                                                innerLoopCode,
                                                useContext,
                                                allowNegativeIndexSequences)
            }
        )
    )

modelContextClass_embedCodeInForLoop = function(singleContexts,
                                           innerLoopCode,
                                           useContext = NULL,
                                           allowNegativeIndexSequences = NULL) {
    ## innerLoopCode is code to be embedded in (possibly nested) for-loops from this context
    ## useContext is an optional logical vector of which contexts to include
    ## allowNegativeIndexSequences: if TRUE, for(i in 2:1) results in iterating over c(2,1), as R would.  If FALSE (Default),
    ## behavior is like BUGS: for(i in 2:1) results in no iteration.
    if(is.null(allowNegativeIndexSequences))
        allowNegativeIndexSequences <-
            ifelse(is.null(getNimbleOption('processBackwardsModelIndexRanges')), TRUE,
                   getNimbleOption('processBackwardsModelIndexRanges'))
 
    if(is.null(useContext)) {
        useContext <- rep(TRUE, length(singleContexts))
    }
    iContext <- length(singleContexts)
    while(iContext >= 1) {
        if(useContext[iContext]) {
            newCode <- singleContexts[[iContext]]$forCode
            if(!allowNegativeIndexSequences) {
                indexRangeCode <- newCode[[3]]
                isColonExpr <-
                    ifelse(is.name(indexRangeCode[[1]]),
                        as.character(indexRangeCode[[1]]) == ':', FALSE)
                if(isColonExpr)
                    newCode[[3]] <-
                        as.call(
                            list(
                                as.name('nm_seq_noDecrease'),
                                indexRangeCode[[2]],
                                indexRangeCode[[3]])
                        )
            }
            newCode[[4]] <- innerLoopCode
            innerLoopCode <- newCode
        }
        iContext <- iContext - 1
    }
    innerLoopCode
}

genIndexVarValues_recurse <- function(singleContexts, constantsEnvCopy) {
    if(!length(singleContexts))
        return(list(list()))
    
    indexExpr <- singleContexts[[1]]$indexVarExpr
    indexName <- as.character(indexExpr)
    rangeExpr <- singleContexts[[1]]$indexRangeExpr
    
    ## This changes the behavior for expanding looping ranges (L:U),
    ## in particular in the strange cases when L > U.
    if(is.call(rangeExpr) && rangeExpr[[1]] == ':') {
        rangeValueL <- eval(rangeExpr[[2]], envir = constantsEnvCopy)
        rangeValueU <- eval(rangeExpr[[3]], envir = constantsEnvCopy)
        if(rangeValueL <= rangeValueU) {
            rangeValues <- rangeValueL:rangeValueU
        } else {
            if(getNimbleOption('processBackwardsModelIndexRanges')) {
                ## for(i in 9:7) --> for(i in c(9, 8, 7))
                rangeValues <- rangeValueU:rangeValueL
            } else {
                ## for(i in 9:7) --> for(i in numeric(0))
                rangeValues <- numeric(0)
            }
        }
    } else {
        ## CHECK: are there cases we want to allow here.
        stop("genIndexVarValues_recurse: loop range expression is not of the form `L:U`.")
        ## rangeValues <- eval(rangeExpr, envir = constantsEnvCopy)
    }
    
    indexVarValues <- list()
    for(value in rangeValues) {
        assign(indexName, value = value, envir = constantsEnvCopy)
        indexVarValuesNew <-
            genIndexVarValues_recurse(singleContexts[-1],
                                      constantsEnvCopy)
        indexVarValuesNew <-
            lapply(indexVarValuesNew,
                   function(l) {
                       l <- rev(l)
                       l[[indexName]] <- value
                       return(rev(l))
                   })
        indexVarValues <- c(indexVarValues, indexVarValuesNew)
    }
    return(indexVarValues)
}

expandContextAndReplacements <- function(allReplacements, allReplacementNameExprs, context, constantsEnv) {
    ## `allReplacements` is a list like `list(i = i, i_plus_1 = i+1, mean_x_1to5 = mean(x[1:5]))`
    ## `context` is a `modelContextClass` object
    ## `constantsEnv` is an environment with constants that can be used to permanently replace values in the `allReplacements` code

    numContexts <- length(context$singleContexts)
    if(!numContexts) { ## No indices or known indices
        if(!length(allReplacements)) {
            context$replacementsEnv <<- NULL
            return(NULL)
        }
    }
    
    ## When done, we will have created a new environment and want to remove the constants from it.
    namesToRemoveAtEnd <- ls(constantsEnv)
    constantsEnvCopy <- list2env(as.list(constantsEnv, all.names = TRUE))
    ## Some replacements like min(j:100) should no longer be needed but are still there.

    ## If this all works, `useContext` can be removed.
    useContext <- rep(TRUE, numContexts)
    
    valueVarNames <- if(numContexts > 0) paste0("INDEXVALUE_", 1:numContexts, "_") else character(0)
    ## `indexRecordingCode` gives lines of code like "INDEXVALUE_1_[iAns] <- i". This will later have its name changed to "i"
    indexRecordingCode <- vector('list', length = numContexts)
    for(i in seq_along(context$singleContexts)) {
        if(useContext[i])
            indexRecordingCode[[i]] <- substitute(V[iAns] <- index,
                                                  list(V = as.name(valueVarNames[i]),
                                                       index = context$singleContexts[[i]]$indexVarExpr))
    }

    numReplacements <- length(allReplacements)
    useReplacement <- unlist(lapply(allReplacementNameExprs,
                                    function(x) { ## do not use replacements that are identical to indexVars
                                        for(i in seq_along(context$singleContexts)) {
                                            if( identical(context$singleContexts[[i]]$indexVarExpr, x) ) return(FALSE)
                                        }
                                        return(TRUE)
                                        }))
    ## `replacementRecordingCode` gives lines of code like "i_plus_1[iAns] <- i+1".
    replacementRecordingCode <- vector('list', length = numReplacements)
    for(i in seq_along(replacementRecordingCode)) {
        if(useReplacement[i])
            replacementRecordingCode[[i]] <- substitute(A[[iAns]] <- B,
                                                        list(A = allReplacementNameExprs[[i]],
                                                             B = allReplacements[[i]])) 
    }

    ## From here through the while loop combines the for loops from the contexts,
    ## with the `replacementRecordingCode` and `indexRecordingCode` in the innermost.
    innerLoopCode <- as.call(c(list(quote(`{`)), replacementRecordingCode, indexRecordingCode, quote(iAns <- iAns + 1)))

    innerLoopCode <- context$embedCodeInForLoop(innerLoopCode, useContext)
    ## At this point `innerLoopCode` has the full loop
    ## `determineContextSize` does something similar -- creates and executes nested for loops --
    ## only for the purpose of counting how big the result will be.
    outputSize <- determineContextSize(context, useContext, constantsEnvCopy)
    for(i in seq_along(context$singleContexts)) {
        if(useContext[i])
            assign(valueVarNames[i], numeric(outputSize), constantsEnvCopy)
    }
    for(i in seq_along(replacementRecordingCode)) {
        if(useReplacement[i])
            assign(names(allReplacements)[i], vector('list', length = outputSize), constantsEnvCopy)
    }
    assign("iAns", 1, constantsEnvCopy)
    eval(innerLoopCode, constantsEnvCopy)
    for(i in seq_along(context$singleContexts)){
        if(useContext[i]) {
            constantsEnvCopy[[ as.character(context$singleContexts[[i]]$indexVarExpr) ]] <- constantsEnvCopy[[ valueVarNames[i] ]]
            rm(list = valueVarNames[i], envir = constantsEnvCopy)
        }
    }
    ## Turn lists into vectors when all elements are scalars.
    ## When not, ensure all list elements are numeric, not integer, to avoid compiler mix-ups.
    for(i in seq_along(allReplacementNameExprs)) {
        if(useReplacement[i]) {
            unlistScalarCode <- substitute( {
                FOO_allScalar <- all(unlist(lapply(VARNAME, function(x) length(x) == 1)))
                if(FOO_allScalar) {
                    VARNAME <- unlist(VARNAME) ## Ok to have integers here
                } else {
                    for(FOO_i in seq_along(VARNAME)) storage.mode(VARNAME[[FOO_i]]) <- 'double' ## but not here
                    rm(FOO_i)
                }
                rm(FOO_allScalar)
            }, list(VARNAME = allReplacementNameExprs[[i]]) )
            eval(unlistScalarCode, envir = constantsEnvCopy)
            ##rm(list = 'FOO_allScalar', envir = constantsEnvCopy)
        }
    }

    rm(list = c(namesToRemoveAtEnd, 'iAns'), envir = constantsEnvCopy)
    assign("outputSize", outputSize, constantsEnvCopy)
    return(constantsEnvCopy)   ## becomes replacementsEnv
}

determineContextSize <- function(context, useContext = rep(TRUE, length(context$singleContexts)), evalEnv = new.env()) {
    ## FUTURE: Could improve this by checking for nested loops that don't use indices from outer loops.
    innerLoopCode <- quote(iAns <- iAns + 1)
    innerLoopCode <- context$embedCodeInForLoop(innerLoopCode, useContext)

    assign("iAns", 0L, evalEnv)
    test <- try(eval(innerLoopCode, evalEnv))
    if(is(test, 'try-error'))
        stop("determineContextSize: Could not evaluate loop syntax: is indexing information provided via `constants`?")
    ans <- evalEnv$iAns
    rm(list = c('iAns', context$indexVarNames[useContext]), envir = evalEnv)
    return(ans)
}

nm_seq_noDecrease <- function(a, b) {
    if(a > b) {
        numeric(0)
    } else {
        a:b
    }
}
