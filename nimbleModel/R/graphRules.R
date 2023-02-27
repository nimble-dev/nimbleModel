## A graphRuleClass object represents an edge in the model graph using a set of indexRules,
## for the purpose of graph queries.

graphRuleClass <- R6Class(
    classname = "graphRuleClass",
    portable = FALSE,
    public = list(
        indexRules = NULL,
        indexSets = NULL,
        indexConstraints = NULL,
        numFromIndices = NULL,
        parentVar = NULL,
        childVar = NULL,
        stoch = logical(),
        
        initialize = function(toExpr, FromExpr, context, constants = list(), stoch = NULL) {
            stoch <<- stoch
            parentVar <<- deparse(ifelse(length(fromExpr) > 1, fromExpr[[2]], fromExpr))
            childVar <<- deparse(ifelse(length(toExpr) > 1, toExpr[[2]], toExpr))
            indexSets <<- makeSeparableIndexSets(toExpr, fromExpr, context)
            numFromIndices <<- length(fromExpr) - 2
            if(numFromIndices == -1)
                numFromIndices <<- 0
            if(numFromIndices < 0)
                stop("graphRuleClass: Unable to determine number of indices in ", deparse(fromExpr), ".")
            output <- makeIndexRules(toExpr, fromExpr, indexSets, context, constants)
            indexRules <<- output$indexRules
            indexConstraints <<- output$indexConstraints
        },
        
        apply = function(fromVarRange) {
            if(is.character(fromVarRange)) {
                if(parentVar == fromVarRange && numFromIndices) {
                    fromVarRange <- getFromRange()   # only varName given
                } else fromVarRange <- varRangeClass$new(fromVarRange)   # string providing the varRange         
            }
            if(!is.null(parentVar) && is(fromVarRange, 'varRangeClass') && getVarName(fromVarRange) != parentVar)
                return(NULL)
            if(!is(fromVarRange, 'varRangeClass'))
                stop("graphRuleClass$apply: 'fromVarRange' needs to be a `varRangeClass` object.")
            applyGraphRule(fromVarRange, self)
        },

        getFromRange = function() {
            if(!length(indexSets$fromIndexSlotToSet)) { ## no indexing
                varRange <- varRangeClass$new(parentVar)
            } else {
                maxes <- indexSets$fromIndexSlotToSet

                for(constraint in indexConstraints) 
                    maxes[constraint$slots] <- constraint$getMax()

                for(setID in seq_along(indexSets)) {
                    maxes[indexSets$fromIndexSlotToSet == setID] <- indexRules[[setID]]$getMax()

                varRange <- varRangeClass$new(lapply(seq_along(maxes),
                                               function(i) newIndexRange(
                                                               substitute(1:MAX, list(MAX = maxes[i])))),
                                        varName = parentVar) 
            }
            return(varRange)
        })
)


## Separate sets of index relationships into separable sets.
makeSeparableIndexSets <- function(toExpr, fromExpr, context) {
    if(length(context$singleContexts)) {
        indexVarNames <- structure(context$indexVarNames,
                                   names = context$indexVarNames)
    } else indexVarNames <- character(0)
    numIndexVars <- length(indexVarNames)

    if(length(toExpr) < 3) {  # no to indexing
        toIndexExprs <- NULL
        toDim <- 0
    } else {
        toIndexExprs <- as.list(toExpr[-c(1,2)])
        toDim <- length(toIndexExprs)
    }
    
    if(length(fromExpr) < 3) {  # no from indexing
        fromIndexExprs <- NULL
        fromDim <- 0
    } else {
        fromIndexExprs <- as.list(fromExpr[-c(1,2)])
        fromDim <- length(fromIndexExprs)
    }

    ## Keep track of which index var definitions use another index (e.g. `j in 1:n[i]`).
    contextExprsVars <- lapply(seq_along(context$singleContexts),
                               function(idx) all.vars(context$singleContexts[[idx]]$indexRangeExpr))
        
    varUsedinContextExprs <- lapply(indexVarNames, function(nm) {
        indexVarNames[sapply(contextExprsVars, function(usedVars) nm %in% usedVars)] })
        
    makeBoolIndexVarList <- function(indexExpr) {
        return(structure(
            indexVarNames %in% all.vars(indexExpr),
            names = indexVarNames
        ))
    }
    
    toBoolIndexVarList <- lapply(toIndexExprs,
                                   makeBoolIndexVarList)
    fromBoolIndexVarList <- lapply(fromIndexExprs,
                                   makeBoolIndexVarList)
    indexVarTosetID <- structure(vector('list', length = numIndexVars),
                                names = indexVarNames)
    toIndexSlotToSetID <- integer(length = toDim)
    fromIndexSlotToSetID <- integer(length = fromDim)
    indexVarNameSets <- list()
    currentSetID <- 0
    remainingIndexVarNames <- indexVarNames
    fromOnly <- NULL
    
    while(length(remainingIndexVarNames)) {
        currentSetID <- currentSetID + 1
        done <- FALSE
        currentIndexVarNames <- remainingIndexVarNames[1]
        while(!done) {
            toBoolUsesCurrentIndexVars <- unlist(
                lapply(toBoolIndexVarList,
                       function(x) any(x[currentIndexVarNames])))
            fromBoolUsesCurrentIndexVars <- unlist(
                lapply(fromBoolIndexVarList,
                       function(x) any(x[currentIndexVarNames])))

            toAdditionalIndexVars <- unique(unlist(lapply(
                toBoolIndexVarList[toBoolUsesCurrentIndexVars],
                function(x) indexVarNames[x])
                ))
            
            fromAdditionalIndexVars <- unique(unlist(lapply(
                fromBoolIndexVarList[fromBoolUsesCurrentIndexVars],
                function(x) indexVarNames[x])
                ))

            ## Add in vars where the current index is used in their context expression.
            ## This should cause any indices using i or j to be in the same set,
            ## which deals with ragged indexing such as for(i in 1:m) for(j in 1:n[i])

            contextAdditionalIndexVars <- unlist(varUsedinContextExprs[currentIndexVarNames])
            
            allAdditionalIndexVarNames <- setdiff(unique(c(toAdditionalIndexVars,
                                                           fromAdditionalIndexVars,
                                                           contextAdditionalIndexVars)),
                                                  currentIndexVarNames)
            if(!length(allAdditionalIndexVarNames)) {
                done <- TRUE
            } else {
                currentIndexVarNames <- c(currentIndexVarNames,
                                          allAdditionalIndexVarNames)
            }

            ## Case of index in fromExpr and not toExpr (from `getParents` cases):
            ## record that this rule is a fromOnly constraint rule.
            if(done && !sum(unlist(
                lapply(toBoolIndexVarList,
                       function(x) any(x[currentIndexVarNames]))))) 
                fromOnly[currentSetID] <- TRUE
        }
        
        ## Recording them this way sorts them.
        indexVarNameSets[[currentSetID]] <- indexVarNames[currentIndexVarNames]
        indexVarTosetID[currentIndexVarNames] <- currentSetID
        toIndexSlotToSetID[toBoolUsesCurrentIndexVars] <- currentSetID
        fromIndexSlotToSetID[fromBoolUsesCurrentIndexVars] <- currentSetID
        remainingIndexVarNames <- setdiff(remainingIndexVarNames,
                                          currentIndexVarNames)
    }

    if(is.null(fromOnly)) {
        fromOnly <- rep(FALSE, currentSetID)
    } else {
        tmp <- rep(FALSE, currentSetID)
        tmp[which(fromOnly)] <- TRUE
        fromOnly <- tmp
    }

    return(list(toIndexSlotToSetID = toIndexSlotToSetID,
         fromIndexSlotToSetID = fromIndexSlotToSetID,
         indexVarToSetID = indexVarToSetID,
         numSets = currentSetID,
         indexVarNameSets = indexVarNameSets,
         fromOnly = fromOnly))
}


checkForVars <- function(toExpr, fromExpr, context, constants) {
    varsInExpr <- NULL
    if(length(fromExpr) > 1)
        varsInExpr <- c(varsInExpr, all.vars(fromExpr[2:length(fromExpr)]))
    if(length(toExpr) > 1)
        varsInExpr <- c(varsInExpr, all.vars(toExpr[2:length(toExpr)]))
    wh <- which(!varsInExpr %in% c(names(constants), context$indexVarNames))
    if(length(wh))
        stop("graphRuleClass: Index or constant ", paste(unique(varsInExpr[wh]), collapse = ','), " not found as loop index or in constants.")
}


modifyContextForfromOnlyRules <- function(LHS, RHS, context, constants) {
    if(identical(LHS, RHS)) {
        varsInExpr <- NULL
        if(length(RHS) > 1)
            varsInExpr <- all.vars(RHS[2:length(RHS)]) 
        indexVarsInExpr <- varsInExpr[!varsInExpr %in% constants]
        context <- modelContextClass$new(context$singleContexts[names(context$singleContexts) %in% indexVarsInExpr])
    }
    return(context)
}

## Make indexRules and indexConstraints based on the indexSets.
makeIndexRules <- function(toExpr, fromExpr, indexSets, context, constants = list()) {
    constantsEnv <- if(is.environment(constants))
                        constants
                    else
                        list2env(constants)

    checkForVars(toExpr, fromExpr, context, constants)

    ## RHSonlyRules can involve fewer single contexts than the full declaration (e.g., mu[i] <- tau)
    ## Need to remove unneeded contexts or the indexSets won't be correct.
    context <- modifyContextForfromOnlyRules(toExpr, fromExpr, context, constants)
    
    if(length(toExpr) >= 3 && toExpr[[1]] == '[') {
        toIndexExprs <- as.list(toExpr[-c(1,2)])
    } else if(length(toExpr) == 1) toIndexExprs <- list() else
        stop("graphRuleClass: '", deparse(toExpr), "' should be an index expression or variable name.")

    if(length(fromExpr) >= 3 && fromExpr[[1]] == '[') {
        fromIndexExprs <- as.list(fromExpr[-c(1,2)])
    } else if(length(fromExpr) == 1) fromIndexExprs <- list() else
        stop("graphRuleClass: '", deparse(fromExpr), "' should be an index expression or variable name.") 

    ## Create simple constraints.
    fromConstantIndices <- which(indexSets$fromIndexSlotToSetID == 0)
    indexConstraints <- lapply(fromConstantIndices, function(idx)
        newIndexConstraint_fromSimple(fromIndexExprs[idx], idx, constants))
    
    numSets <- indexSets$numSets
    indexRules <- list()

    for(iSet in seq_len(numSets)) {
        ## Extract index expressions used in this set.
        toIndicesBool <- indexSets$toIndexSlotToSetID == iSet
        thisToIndexExprs <- structure(
            toIndexExprs[toIndicesBool],
            names = if(sum(toIndicesBool))
                        paste0("t", seq_len(sum(toIndicesBool)))
                    else character(0)
        )
        fromIndicesBool <- indexSets$fromIndexSlotToSetID == iSet
        thisFromIndexExprs <- structure(
            fromIndexExprs[fromIndicesBool],
            names = if(sum(fromIndicesBool))
                        paste0("f", seq_len(numFromBool))
                    else character(0)
        )
        indexVarNamesInThisSet <- indexSets$indexVarNameSets[[iSet]]
        thisContext <-
            modelContextClass$new(context$singleContexts[indexVarNamesInThisSet])

        ## Add additional indexConstraints based on "any" style constraint in
        ## `getParents` type context such as
        ## `y[i] -> x[2]`, `y[k[i]] -> x[2]`, `y[k1[i],k2[i]] -> x[2]`.
        if(length(thisContext) && !length(thisToIndexExprs) && length(thisFromIndexExprs)) {
            slots <- which(indexSets$fromIndexSlotToSetID == iSet)
            indexConstraints[[length(indexConstraints)+1]]  <-
                newIndexConstraint_fromUnrolling(thisFromIndexExprs, slots, thisContext, constantsEnv)
            next
        }
        
        ## We try making each rule in order. If it fails, try to make the next.
        
        ## y[i] <- x[2] 'all' case
        thisIndexRule <- indexRuleAllClass$new(
                                                toIndexExprList = thisToIndexExprs,
                                                fromIndexExprList = thisFromIndexExprs,
                                                context = thisContext,
                                                constants = constantsEnv)
        ## y[i] <- x[i] block case
        if(is.null(thisIndexRule$setupResults)) {
            thisIndexRule <- indexRuleBlockClass$new(
                                                      toIndexExprList = thisToIndexExprs,
                                                      fromIndexExprList = thisFromIndexExprs,
                                                      context = thisContext,
                                                      constants = constantsEnv)
        }
        ## catch-all, e.g., y[i] <- x[block[i]]
        if(is.null(thisIndexRule$setupResults)) {
            thisIndexRule <- indexRuleArbitraryClass$new(
                                                          toIndexExprList = thisToIndexExprs,
                                                          fromIndexExprList = thisFromIndexExprs,
                                                          context = thisContext,
                                                          constants = constantsEnv)
        }
        indexRules[[iSet]] <- thisIndexRule
    }
        
    ## Make constant rules for all `toExpr` constants, e.g. first index in y[3, i] <- x[i],
    ## and append to the indexRules.
    iSet <- length(indexRules) + 1
    for(constantSlot in which(indexSets$toIndexSlotToSetID == 0)) {
        thisToIndexExprs <- structure(toIndexExprs[constantSlot], names = 't1')        
        indexRules[[iSet]] <- indexRuleConstantClass$new(
                                                     toIndexExprList = thisToIndexExprs,
                                                     fromIndexExprList = character(0),
                                                     context = modelContextClass$new(),
                                                     constants = constantsEnv)
        iSet <- iSet + 1
    }
    
    ## Make constant rule for case of no LHS indexing, e.g., y <- x[3].
    ## These are stuck at the end in order of the index slots that are constant.
    if(!length(indexSets$toIndexSlotToSetID)) 
        indexRules[[iSet]] <- indexRuleConstantClass$new(
                                                     toIndexExprList = character(0),
                                                     fromIndexExprList = character(0),
                                                     context = modelContextClass$new(),
                                                     constants = constantsEnv)

    return(list(
        indexRules = indexRules,
        indexCconstraints = indexConstraints
    ))
}


## We need to extract the relevant components of fromVarRange for each rule, apply the rule,
## Applies a `graphRule` to a `varRange` by extract the relevant components of fromVarRange for
## constituent `indexRule`, applying the `indexRule` and composing the result as a new varRange.
applyGraphRule <- function(fromVarRange, rule, varName = NULL) {
    ## Some of the steps below will reveal things that
    ## could be cached and re-used.
    indexSets <- rule$indexSets
    indexConstraints <- rule$indexConstraints
    indexRules <- rule$indexRules
    numSets <- indexSets$numSets

    ## Determine number of sets applied to get result (i.e., excluding fromOnly constraint rules from getParents cases)
    constantSlots <- which(indexSets$toIndexSlotToSetID == 0)
    
    ## Check valid number of input indices
    numFromIndices <- length(unlist(fromVarRange$rangeToIndexSlot))
    if(numFromIndices != rule$numFromIndices)
        stop("applyGraphRule: incorrect number of input indices.")
        
    ansIndexRanges <- list()
    ansRangeToIndexSlot <- list()

    ## Determine which fromSlots will be used for each rule
    ## and set up index crossing and aligning needs.
    
    setToFromSlots <- vector('list', length = numSets)
    complicatedCrossing <- FALSE
    ## Each set corresponds to one rule.
    for(iSet in seq_len(numSets)) {
        thisFromSlots <- which(indexSets$fromIndexSlotToSetID == iSet)
        setToFromSlots[[iSet]] <- thisFromSlots
        ## TODO: we could avoid expanding all slots for slots that are not involved in the
        ## rule or the input indexRanges.
        if(checkForComplicatedCrossing(thisFromSlots, fromVarRange))
            complicatedCrossing <- TRUE
    }

    ## Check complicated crossing for constraints too -- cases where a constraint
    ## involves multiple input indexRanges and at least one of those indexRanges
    ## also covers other indices unused in the constraint.
    if(!complicatedCrossing) 
        for(constraint in indexConstraints)
            if(checkForComplicatedCrossing(constraint$slots, fromVarRange))
                complicatedCrossing <- TRUE
    
    numIndexRanges <- length(fromVarRange$indexRanges)
    fromIndexRangeToSetID <- lapply(seq_len(numIndexRanges),
                           function(x) integer())
    
    ## For complicated crossing we first set up the fully-crossed inputs.
    if(complicatedCrossing) {
        warning("applyGraphRule: Detected that not all indices in an indexRange are used in an indexRule that uses that indexRange, so fully crossing all inputs.")        
        result <-fromVarRange$extractIndexRange(seq_len(numFromndices), returnUsedRanges = TRUE)
        fromVarRange <- varRangeClass$new(list(result$indexRange))
        fromUsedRanges <- result$usedRanges
        numIndexRanges <- 1
    }

    ## Check valid fromExpr.
    ## Returns a list with one element for each input indexRange giving validity with respect to constraints,
    ## either a scalar when the range does not (also) involve unconstrained columns or the valid rows when it does.
    fromConstraints <- checkIndexConstraints(fromVarRange, rule$indexConstraints)

    ## No result if any constraint not satisfied for any input rows.
    invalid <- sapply(fromConstraints, function(constraint)
        !is.null(constraints) && !any(constraint))
    if(any(invalid)) 
        return(NULL)

    ## Apply indexRules one by one, getting inputs from multiple indexRanges if necessary.
    setIdx <- 1
    for(iSet in which(!indexSets$fromOnly)) {
        thisFromSlots <- setToFromSlots[[iSet]]

        ## Create an indexRange containing the indices for the needed index slots.
        if(length(thisFromSlots)) {
            result <- fromVarRange$extractIndexRange(thisFromSlots, returnUsedRanges = TRUE)
            fromIndexRange <- result$indexRange
            for(usedRange in result$usedRanges)
                fromIndexRangeToSetID[[usedRange]] <- append(fromIndexRangeToSetID[[usedRange]], iSet)
        } else {  ## no indexing or constant indexing
            fromIndexRange <- NULL
        }
        
        ## Apply the rule to produce a resulting range and noting the slots covered by the range.
        ansIndexRanges[[setIdx]] <- indexRules[[iSet]]$apply(fromIndexRange, collapse = FALSE)
        ansRangeToIndexSlot[[setIdx]] <-
            which(indexSets$toIndexSlotToSetID == iSet)
        setIdx <- setIdx + 1
    }

    ## Treat as a single input indexRange, so that collapse across results of all rules.
    if(complicatedCrossing)
        fromIndexRangeToSetID <- list(sort(unique(unlist(fromIndexRangeToSetID))))

    ## TODO: simplify above if this check works out.
    if(!identical(fromIndexRangeToSetID, list(which(!indexSets$fromOnly))))
        stop("CHECK: found non-match case.")

    ## Compose results from the various rules, including those unrelated to input indexRanges.

    finalIndexRanges <- list()
    finalRangeToIndexSlot <- list()

    ## Aggregate results from multiple rules applied to one input.

    ## Loop through results based on input indexRanges; this will not handle 'constant' rules.
    ## Also, this will produce duplicate results (handled at the end) when multiple indexRanges used in a single rule.
    iAns <- 1
    for(iRange in seq_len(numIndexRanges)) {
        sets <- fromIndexRangeToSetID[[iRange]]
        
        ## TODO: can we remove this next check?
        if(any(indexSets$fromOnly[sets]))
            stop("CHECK: found non-match case 2.")
        sets <- sets[!indexSets$fromOnly[sets]]  ## fromOnly constraint rule handled above.

        if(length(sets)) {
            if(length(sets) > 1) {  # Multiple rules operate on the indexRange.
                if(any(sapply(ansIndexRanges[sets],
                              function(x) is(x, 'indexRangeEmptyClass')))) {
                    finalIndexRanges[[iAns]] <- indexRangeEmptyClass$new()
                } else {
                    ## Combine results from multiple rules (which are in matrixList form, because result for an input row can have arbitrary output rows)
                    ## into multi-column indexRangeMatrix.
                    finalIndexRanges[[iAns]] <-
                        indexRangeMatrixListsToMatrix(ansIndexRanges[sets])
                }
                ## Sort the columns of the result based on ordering of `to` indices.
                finalRangeToIndexSlot[[iAns]] <- do.call('c', ansRangeToIndexSlot[sets])
                slotOrder <- order(finalRangeToIndexSlot[[iAns]])
                if(!identical(slotOrder, seq_along(slotOrder))) {
                    finalRangeToIndexSlot[[iAns]] <- finalRangeToIndexSlot[[iAns]][slotOrder]
                    ## TODO: do we need this check?
                    if(is(finalIndexRanges[[iAns]], 'indexRangeEmptyClass'))
                        stop("CHECK: found empty range")
                    finalIndexRanges[[iAns]] <- finalIndexRanges[[iAns]]$getColumns(slotOrder)
                }
            } else {  # Only one rule operates on the indexRange.
                finalIndexRanges[[iAns]] <- ansIndexRanges[[sets]]
                ## Simplify to a matrix indexRange.
                if(is(finalIndexRanges[[iAns]], 'indexRangeMatrixListClass'))  
                    finalIndexRanges[[iAns]] <-finalIndexRanges[[iAns]]$toMatrix()
                finalRangeToIndexSlot[[iAns]] <- ansRangeToIndexSlot[[sets]]
            }
            
            ## Remove invalid rows from matrix indexRanges flagged by constraint checking.
            ## Only need to check cases where have a logical vector as fromConstraint as
            ## logical scalar (resulting from the constraint index having its own input indexRange)
            ## already checked above where 'invalid' is created and used.
            if(length(fromConstraints[[iRange]]) > 1) {
                if(!is(finalIndexRanges[[iAns]], 'indexRangeMatrixClass'))
                    stop("applyGraphRule: Expecting `fromConstraints` to only be relevant for a matrix indexRange.")
                if(length(fromConstraints[[iRange]]) != finalIndexRanges[[iAns]]$numElements)
                    stop("applyGraphRule: Expecting fromConstraints to have as many logicals as rows of the indexRange.")
                finalIndexRanges[[iAns]] <- finalIndexRanges[[iAns]]$getRows(fromConstraints[[iRange]])
            }

            iAns <- iAns + 1
        }
    }

    ## Remove invalid rows from matrix indexRanges based on presence of NAs and set to empty if no rows left
    ## This can only be done after collapsing or else the constituent matrixLists will have different lengths
    ## and couldn't be properly collapsed above (i.e., if we removed the NAs earlier).
    for(iRange in seq_along(finalIndexRanges)) 
        if(is(finalIndexRanges[[iRange]], 'indexRangeMatrixClass')) {
            NArows <- apply(finalIndexRanges[[iRange]]$values, 1,
                            function(x) any(is.na(x)))
            if(all(NArows))
                finalIndexRanges[[iRange]] <- indexRangeEmptyClass$new()
            if(any(NArows))
                finalIndexRanges[[iRange]] <- finalIndexRanges[[iRange]]$getRows(!NArows)
        }

    ## Add in results from `indexRuleAll` cases, as these have no `from` index used in the rule,
    ## and are not populated into `finalIndexRanges` above.
    missedSets <- which(!seq_along(ansIndexRanges) %in% unlist(fromIndexRangeToSetID))
    if(length(missedSets)) {
       finalIndexRanges <- c(finalIndexRanges, ansIndexRanges[missedSets])
       finalRangeToIndexSlot <- c(finalRangeToIndexSlot, ansRangeToIndexSlot[missedSets])
    }

    ## Add in result of constant rules (constant `to` index and (of course) no corresponding `from` index).
    if(length(constantSlots)) {
        constantIndexRanges <- sapply(indexRules[(numSets+1):(numSets+length(constantSets))],
                                      function(rule) rule$apply(NULL))
        finalIndexRanges <- c(finalIndexRanges, constantIndexRanges)
        finalRangeToIndexSlot <- c(finalRangeToIndexSlot, as.list(constantSlots))
    }

    ## Add in result of constant rule for case of no `to` indexing at all.
    if(!length(indexSets$toIndexSlotTosetID)) {
        if(length(finalIndexRanges))
            stop("applyGraphRule: unexpected result for non-indexed result.")
        finalIndexRanges <- indexRangeNoneClass$new()
        finalRangeToIndexSlot <- as.list(1)
    }

    ## Convert single-column matrix indexRanges to sequence if possible
    ## (e.g., to more efficiently handle y[i] <- x[k[i]] cases where all y's included).
    for(iRange in seq_along(finalIndexRanges))
        if(is(finalIndexRanges[[iRange]], 'indexRangeMatrixClass'))
            finalIndexRanges[[iRange]] <- finalIndexRanges[[iRange]]$toSequence()

    ## Put final results in natural order (based on first slot for each indexRange) in case they are not already.
    finalIndexOrderStarts <- sapply(finalRangeToIndexSlot, `[`, 1)
    orderFinalIndexOrderStarts <- order(finalIndexOrderStarts)
    if(!identical(orderFinalIndexOrderStarts, seq_along(orderFinalIndexOrderStarts))) {
        finalIndexRanges <- finalIndexRanges[orderFinalIndexOrderStarts]
        finalRangeToIndexSlot <- finalRangeToIndexSlot[orderFinalIndexOrderStarts]
    }

    ## Remove duplicate columns (from cases where two indexRanges are used in a single rule).
    repeats <- duplicated(finalRangeToIndexSlot)

    result <- varRangeClass$new(
        indexInfo = finalIndexRanges[!repeats],
        rangeToIndexSlot = finalRangeToIndexSlots[!repeats],
        varName = ifelse(is.null(varName), rule$childVar, varName),
        fromStochRule = rule$stoch
        )
    if(result$isEmpty()) result <- NULL
    return(result)
}


## Discover cases where a rule uses multiple input indexRanges and 
## at least one of those indexRanges also covers other indices unused in the rule.
## E.g., y[f(i,j),k] <- x[i,j,k] where j,k are together in an input indexRange.
## We need those tied to together to avoid incorrect crossing of results.
## For simplicity, if this happens at all, we do full crossing of all input indices to
## implicitly produce a single indexRange.
checkForComplicatedCrossing <- function(fromSlots, fromVarRange) {
    usedRanges <- unique(fromVarRange$indexSlotToRange[fromSlots])
    ## equal to: `sapply(fromVarRange$rangeToIndexSlot, function(x) any(thisFromIndices %in% x))`
    
    if(length(fromSlots) > 1 && length(usedRanges) > 1 &&
       !identical(fromSlots, sort(unique(unlist(fromVarRange$rangeToIndexSlot[usedRanges])))))
        return(TRUE)
    return(FALSE)
}
