## A graphRuleClass object represents an edge in the model graph using a set
## of indexRules, for the purpose of graph queries.

## There are a variety of cases, some tricky, that must be handled.
## Rules/constraints can cover one or more index slots arbitrarily,
## as can input indexRanges.
## Furthermore, an input index can produce 0, 1, or more output indices,
## e.g., `y[i] <- x[k[i]]`.
## These sometimes need to be combined with results for other index slots
## with a different number of output indices,
## e.g., `y[i,j] <- x[k[i], j]` for a 2-d input indexRange.
## We must also deal with `getParents` type cases, which can create
## "any" style constraints,
## e.g. `y[i] -> x[2]` where any valid input 'i' should produce `x[2]` as output.
## We must also deal with nested indexing creating arbitrary rules,
## e.g., `for(i in 1:m); for(j in 1:n[i]) y[i,j] <- x[i,j`].

## Some of the cases include:

## Applying one rule (or constraint) to one input indexRange.
##   - If constraint applied, result of constraint check should be scalar boolean.

## Applying one (single-index) rule (or constraint) to one input indexRange that contains additional
## indices not involved in the rule. Additional index slots could be involved in another rule or a constraint.
##   - If constraint applied, result of constraint check should be vector boolean,
##     so that can filter result of rules on the other indices
##   - Result of rule needs to be crossed (expanded) element-wise with results of other rules
##     applied to same input indexRange.
##   - If there are multiple constraints on the indexRange, result needs to be combined element-wise.


## Applying one (multi-index) rule (or constraints) to multiple input indexRanges.
##   - Input indexRanges must be crossed (expanded) before applying the rule.

## Applying one (multi-index) rule (or constraints) to multiple input indexRanges,
## where at least one input indexRange also contains additional index slots not involved in the rule.
##   - For this 'complicatedCrossing' case, we cross _all_ the input indexRanges
##     so that the new input varRange has a single indexRange covering all index slots.
##   - Otherwise, we would need to deal with the fact that the additional index slot
##     needs to be expanded as part of the rule but wouldn't be expanded in the rule
##     that actuallly applies to the additional index slot.

graphRuleClass <- R6Class(
    classname = "graphRuleClass",
    portable = FALSE,
    public = list(
        indexRules = list(),
        indexSets = NULL,
        indexConstraints = NULL,
        numFromIndexSlots = NULL,
        fromVarName = NULL,
        toVarName = NULL,
        stoch = logical(),
        
        initialize = function(toExpr, fromExpr, context, constants = list(), stoch = NULL, checkVars =  TRUE) {
            stoch <<- stoch
            fromVarName <<- safeDeparse(ifelse(length(fromExpr) > 1, fromExpr[[2]], fromExpr), warn = TRUE)
            toVarName <<- safeDeparse(ifelse(length(toExpr) > 1, toExpr[[2]], toExpr), warn = TRUE)

            ## Don't want to check for index values if could be dynamically indexed.
            if(checkVars || !getNimbleModelOption('allowDynamicIndexing'))
                checkForVars(toExpr, fromExpr, context, constants)

            ## RHSonlyRules can involve fewer single contexts than the full declaration (e.g., mu[i] <- tau)
            ## Need to remove unneeded contexts or the indexSets won't be correct.
            context <- modifyContextForFromOnlyRules(toExpr, fromExpr, context, constants)

            indexSets <<- makeSeparableIndexSets(toExpr, fromExpr, context)
            numFromIndexSlots <<- length(fromExpr) - 2
            if(numFromIndexSlots == -1)
                numFromIndexSlots <<- 0
            if(numFromIndexSlots < 0)
                stop("unable to determine number of indices in ", safeDeparse(fromExpr), ".")
            output <- makeIndexRules(toExpr, fromExpr, indexSets, context, constants)
            indexRules <<- output$indexRules
            indexConstraints <<- output$indexConstraints
        },
        
        apply = function(fromVarRange, removeDuplicates = TRUE) {
            if(is.character(fromVarRange) || inherits(fromVarRange, 'varRangeClass')) {
                inputVarName <- getVarName(fromVarRange)
                if(!is.null(fromVarName) && !is.null(inputVarName) && inputVarName != fromVarName)
                    return(NULL)
                ## CHECK: should we error out if fromVarRange doesn't have a varName?
            }
            if(is.character(fromVarRange)) {
                if(fromVarName == fromVarRange && numFromIndexSlots) {
                    fromVarRange <- getFromRange()   # only varName given
                } else fromVarRange <- varRangeClass$new(fromVarRange)   # string providing the varRange         
            }
            if(!inherits(fromVarRange, 'varRangeClass'))
                stop("`fromVarRange` needs to be a `varRangeClass` object.")
            applyGraphRule(fromVarRange, self, removeDuplicates = removeDuplicates)
        },

        getFromRange = function() {
            if(!length(indexSets$fromIndexSlotToSet)) { ## no indexing
                varRange <- varRangeClass$new(fromVarName)
            } else {
                maxes <- indexSets$fromIndexSlotToSet

                for(constraint in indexConstraints) 
                    maxes[constraint$slots] <- constraint$getMax()

                ## Cases with indexing in `fromExpr` apart from `any` style constraints.
                if(length(indexRules)) {
                    sets <- which(sapply(indexRules, function(x)
                        class(x)[1] %in% c('indexRuleBlockClass', 'indexRuleArbitraryClass')))
                    for(set in sets) 
                        maxes[indexSets$fromIndexSlotToSet == set] <- indexRules[[set]]$getMax()
                }
                
                varRange <- varRangeClass$new(lapply(seq_along(maxes),
                                                     function(i) newIndexRange(
                                                                     substitute(1:MAX, list(MAX = maxes[i])))),
                                              varName = fromVarName) 
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
    indexVarToSet <- structure(vector('list', length = numIndexVars),
                                names = indexVarNames)
    toIndexSlotToSet <- integer(length = toDim)
    fromIndexSlotToSet <- integer(length = fromDim)
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
        indexVarToSet[currentIndexVarNames] <- currentSetID
        toIndexSlotToSet[toBoolUsesCurrentIndexVars] <- currentSetID
        fromIndexSlotToSet[fromBoolUsesCurrentIndexVars] <- currentSetID
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

    return(list(toIndexSlotToSet = toIndexSlotToSet,
         fromIndexSlotToSet = fromIndexSlotToSet,
         indexVarToSet = indexVarToSet,
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
        stop("index or constant `", paste(unique(varsInExpr[wh]), collapse = ','), "` not found as loop index or in `constants`.")
}


modifyContextForFromOnlyRules <- function(LHS, RHS, context, constants) {
    if(identical(LHS, RHS)) {
        varsInExpr <- NULL
        if(length(RHS) > 1)
            varsInExpr <- all.vars(RHS[2:length(RHS)]) 
        indexVarsInExpr <- varsInExpr[!varsInExpr %in% names(constants)]
        context <- modelContextClass$new(context$singleContexts[names(context$singleContexts) %in% indexVarsInExpr])
    }
    return(context)
}

## Make indexRules and indexConstraints based on the indexSets.
makeIndexRules <- function(toExpr, fromExpr, indexSets, context, constants) {
    constantsEnv <- list2env(constants, parent = getDefaultNamespace())
    if(length(toExpr) >= 3 && toExpr[[1]] == '[') {
        toIndexExprs <- as.list(toExpr[-c(1,2)])
    } else if(length(toExpr) == 1) toIndexExprs <- list() else
        stop("`", safeDeparse(toExpr), "` should be an index expression or variable name.")

    if(length(fromExpr) >= 3 && fromExpr[[1]] == '[') {
        fromIndexExprs <- as.list(fromExpr[-c(1,2)])
    } else if(length(fromExpr) == 1) fromIndexExprs <- list() else
        stop("`", safeDeparse(fromExpr), "`should be an index expression or variable name.") 

    ## Create simple constraints.
    fromConstantIndexSlots <- which(indexSets$fromIndexSlotToSet == 0)
    indexConstraints <- lapply(fromConstantIndexSlots, function(idx)
        newIndexConstraint_fromSimple(fromIndexExprs[[idx]], idx, constantsEnv))

   
    ## `indexRules` will have one rule per set with a `NULL` for `indexRuleAny` cases
    ## (which are treated as constraints). `indexRuleConstant` cases are tacked on at end.
    numSets <- indexSets$numSets
    indexRules <- list()
    length(indexRules) <- numSets

    for(iSet in seq_len(numSets)) {
        ## Extract index expressions used in this set.
        toIndicesBool <- indexSets$toIndexSlotToSet == iSet
        thisToIndexExprs <- structure(
            toIndexExprs[toIndicesBool],
            names = if(sum(toIndicesBool))
                        paste0("t", seq_len(sum(toIndicesBool)))
                    else character(0)
        )
        fromIndicesBool <- indexSets$fromIndexSlotToSet == iSet
        thisFromIndexExprs <- structure(
            fromIndexExprs[fromIndicesBool],
            names = if(sum(fromIndicesBool))
                        paste0("f", seq_len(sum(fromIndicesBool)))
                    else character(0)
        )
        indexVarNamesInThisSet <- indexSets$indexVarNameSets[[iSet]]
        thisContext <-
            modelContextClass$new(context$singleContexts[indexVarNamesInThisSet])

        ## Add additional indexConstraints based on "any" style constraint in
        ## `getParents` type context such as
        ## `y[i] -> x[2]`, `y[k[i]] -> x[2]`, `y[k1[i],k2[i]] -> x[2]`.
        if(length(thisContext) && !length(thisToIndexExprs) && length(thisFromIndexExprs)) {
            slots <- which(indexSets$fromIndexSlotToSet == iSet)
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
        
    ## No indexing in result, so no indexRanges.
    if(!length(indexSets$toIndexSlotToSet))
        return(list(
            indexRules = list(),
            indexConstraints = indexConstraints
        ))

    ## Make constant rules for all `toExpr` constants, e.g. first index in y[3, i] <- x[i],
    ## and append to the indexRules.
    iSet <- length(indexRules) + 1
    for(constantSlot in which(indexSets$toIndexSlotToSet == 0)) {
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
    if(!length(indexSets$toIndexSlotToSet)) 
        indexRules[[iSet]] <- indexRuleConstantClass$new(
                                                     toIndexExprList = character(0),
                                                     fromIndexExprList = character(0),
                                                     context = modelContextClass$new(),
                                                     constants = constantsEnv)

    return(list(
        indexRules = indexRules,
        indexConstraints = indexConstraints
    ))
}


## We need to extract the relevant components of fromVarRange for each rule, apply the rule,
## Applies a `graphRule` to a `varRange` by extract the relevant components of `fromVarRange` for
## constituent `indexRule`, applying the `indexRule` and composing the result as a new `varRange`.
applyGraphRule <- function(fromVarRange, rule, varName = NULL, removeDuplicates = TRUE) {
    ## Some of the steps below will reveal things that
    ## could be cached and re-used.
    indexSets <- rule$indexSets
    indexConstraints <- rule$indexConstraints
    indexRules <- rule$indexRules
    
    numIndexRanges <- length(fromVarRange$indexRanges)
    if(fromVarRange$isNone())
        numIndexRanges <- 0

    ## Check valid number of input indices
    numFromIndexSlots <- length(unlist(fromVarRange$rangeToIndexSlot))
    if(numFromIndexSlots != rule$numFromIndexSlots)
        stop("incorrect number of input indices.")
     
    ## Determine which fromSlots will be used for each rule
    ## and set up index crossing and aligning needs.
    ## Each set corresponds to one rule.

    ## Discover cases where a set uses multiple input indexRanges and 
    ## at least one of those indexRanges also covers other indices unused in the set.
    ## E.g., y[f(i,j),k] <- x[i,j,k] where j,k are together in an input indexRange.
    ## We need those tied to together to avoid incorrect crossing of results.
    ## For simplicity, if this happens at all, we do full crossing of all input indices to
    ## implicitly produce a single indexRange.
    complicatedCrossing <- FALSE
    for(iSet in seq_len(indexSets$numSets)) {
        ## TODO: we could avoid expanding all slots for slots that are not involved in the
        ## rule or the input indexRanges.
        thisFromSlots <- which(indexSets$fromIndexSlotToSet == iSet)
        if(checkForComplicatedCrossing(thisFromSlots, fromVarRange)) {
            complicatedCrossing <- TRUE
            break
        }
    }

    ## For complicated crossing we first set up the fully-crossed inputs.
    if(complicatedCrossing) {
        messageIfVerbose("  [Note] applyGraphRule: Detected that not all indices in an `indexRange` are used in an `indexRule` that uses that `indexRange`, so fully crossing all inputs.")        
        result <- fromVarRange$extractIndexRange(seq_len(numFromIndexSlots))
        fromVarRange <- varRangeClass$new(list(result))
        numIndexRanges <- 1
    }

    ## Check index constraints (i.e., valid `fromExpr`).
    ## Returns a list with one element for each input indexRange giving validity with respect to constraints,
    ## either a scalar when the range does not (also) involve unconstrained columns or the valid rows when it does.
    if(length(rule$indexConstraints)) {
        fromConstraints <- checkIndexConstraints(fromVarRange, rule$indexConstraints)

        ## No result if any constraint not satisfied for any input rows.
        invalid <- sapply(fromConstraints, function(constraint)
            !is.null(constraint) && !any(constraint))
        if(any(invalid)) 
            return(NULL)
    }
    
    if(!length(indexRules))
        return(
            varRangeClass$new(ifelse(is.null(varName), rule$toVarName, varName),
                              fromStochRule = rule$stoch)
        )
    
    ## Step 1: Apply indexRules one by one, getting inputs from multiple indexRanges if necessary.

    ## One answer indexRange per indexSet, with `any` cases left as `NULL`.
    ansIndexRanges <- list()
    length(ansIndexRanges) <- indexSets$numSets
    ansRangeToIndexSlot <- list()
    
    for(iSet in which(!indexSets$fromOnly)) {
        thisFromSlots <- which(indexSets$fromIndexSlotToSet == iSet)

        ## Create an indexRange containing the indices for the needed index slots.
        if(length(thisFromSlots)) {
            fromIndexRange <- fromVarRange$extractIndexRange(thisFromSlots)
        } else {  ## no indexing or constant indexing
            fromIndexRange <- NULL
        }
        
        ## Apply the rule to produce a resulting range and noting the slots covered by the range.
        result <- indexRules[[iSet]]$apply(fromIndexRange, collapse = FALSE)
        if(is.null(result)) 
            return(NULL) 
        ansIndexRanges[[iSet]] <- result
        ansRangeToIndexSlot[[iSet]] <-
            which(indexSets$toIndexSlotToSet == iSet)
    }

    ## Step 2: Compose results from the various rules, aggregating results from multiple rules
    ## applied to one input `indexRange`.

    finalIndexRanges <- list()
    finalRangeToIndexSlot <- list()

    ## Loop through results based on input `indexRanges`; this will not handle 'constant' or 'all' rules.
    ## Also, this will produce duplicate results (handled at the end) when multiple input indexRanges
    ## used in a single rule.
    iAns <- 1
    for(iRange in seq_len(numIndexRanges)) {
        sets <- unique(indexSets$fromIndexSlotToSet[fromVarRange$rangeToIndexSlot[[iRange]]])
        ## constraint cases
        sets <- sets[sets != 0]
        sets <- sets[!indexSets$fromOnly[sets]]  
        
        if(length(sets)) {

            ## Remove invalid elements from rangeList elements flagged by constraint checking.
            ## This needs to be done before conversion to `indexRangeMatrixClass` because
            ## booleans of constraint are 1:1 mapped to elements of `rangeList`, where an
            ## element of a `rangeList` can have arbitrarily many sets of indices.
            if(length(rule$indexConstraints) && length(fromConstraints[[iRange]]) > 1) {  # scalar constraint already checked where `invalid` created/used
                for(set in sets) {
                    if(!inherits(ansIndexRanges[[set]], 'indexRangeMatrixListClass'))
                        stop("expecting `fromConstraints` to only be relevant for a `matrixList` `indexRange`.")
                    if(length(fromConstraints[[iRange]]) != ansIndexRanges[[set]]$numElements)
                        stop("expecting `fromConstraints` to have as many logicals as elements of the `indexRange`.")
                    ansIndexRanges[[set]] <- ansIndexRanges[[set]]$getRows(fromConstraints[[iRange]])
                }
            }
            if(length(sets) > 1) {  # Multiple rules operate on the indexRange.
                ## Combine results from multiple rules (which are in matrixList form, because
                ## result for an input row can have arbitrarily many output rows)
                ## into multi-column indexRangeMatrix.
                
                finalIndexRanges[[iAns]] <-
                    indexRangeMatrixListsToMatrix(ansIndexRanges[sets])

                ## Sort the columns of the result based on ordering of `to` indices.
                finalRangeToIndexSlot[[iAns]] <- do.call('c', ansRangeToIndexSlot[sets])
                slotOrder <- order(finalRangeToIndexSlot[[iAns]])
                if(!identical(slotOrder, seq_along(slotOrder))) {
                    finalRangeToIndexSlot[[iAns]] <- finalRangeToIndexSlot[[iAns]][slotOrder]
                    ## TODO: do we need this check?
                    if(is.null(finalIndexRanges[[iAns]]))
                        stop("found unexpected empty `indexRange`.")
                    finalIndexRanges[[iAns]] <- finalIndexRanges[[iAns]]$getColumns(slotOrder)
                }
            } else {  # Only one rule operates on the indexRange.
                finalIndexRanges[[iAns]] <- ansIndexRanges[[sets]]
                ## Simplify to a matrix indexRange.
                if(inherits(finalIndexRanges[[iAns]], 'indexRangeMatrixListClass'))  
                    finalIndexRanges[[iAns]] <- finalIndexRanges[[iAns]]$toMatrix()
                finalRangeToIndexSlot[[iAns]] <- ansRangeToIndexSlot[[sets]]
            }

            ## Remove duplicate rows in matrix indexRanges.
            ## Can only do this after matrixLists have been combined.
            if(removeDuplicates)
                if(inherits(finalIndexRanges[[iAns]], 'indexRangeMatrixClass'))
                    finalIndexRanges[[iAns]]$removeDuplicates()
 
            iAns <- iAns + 1
        }
    }

    ## Remove invalid rows from matrix indexRanges based on presence of NAs and return NULL if no rows left.
    ## This can only be done after collapsing or else the constituent matrixLists will have different lengths
    ## and couldn't be properly collapsed above (i.e., if we removed the NAs earlier).
    for(iRange in seq_along(finalIndexRanges)) 
        if(inherits(finalIndexRanges[[iRange]], 'indexRangeMatrixClass')) {
            NArows <- which(is.na(rowSums(finalIndexRanges[[iRange]]$values)))
            num <- length(NArows)
            if(num) {
                if(num == finalIndexRanges[[iRange]]$numElements)
                    return(NULL)
                finalIndexRanges[[iRange]] <- finalIndexRanges[[iRange]]$getRows(-NArows)
            }
        }

    ## Step 3: Add additional results for "all" and "constant" cases.
    
    ## Add in results from `indexRuleAll` cases, as these have no `from` index used in the rule,
    ## and are not populated into `finalIndexRanges` above.
    allRuleSets <- sapply(indexRules, function(x) inherits(x, 'indexRuleAllClass'))
    if(sum(allRuleSets)) {
       finalIndexRanges <- c(finalIndexRanges, ansIndexRanges[allRuleSets])
       finalRangeToIndexSlot <- c(finalRangeToIndexSlot, ansRangeToIndexSlot[allRuleSets])
    }

    ## Add in result of constant rules (constant `to` index and (of course) no corresponding `from` index).
    constantRulesIdx <- which(sapply(indexRules, function(x) inherits(x, 'indexRuleConstantClass')))
    if(length(constantRulesIdx)) {
        constantIndexRanges <- sapply(indexRules[constantRulesIdx], 
                                      function(rule) rule$apply(NULL))
        finalIndexRanges <- c(finalIndexRanges, constantIndexRanges)
        constantSlots <- which(indexSets$toIndexSlotToSet == 0)
        if(length(constantSlots) != length(constantRulesIdx) &&
           length(constantSlots) > 0)
            stop("number of constant rules doesn't match number of index slots.")
        finalRangeToIndexSlot <- c(finalRangeToIndexSlot, as.list(constantSlots))
    }

    ## Convert single-column matrix indexRanges to sequence if possible
    ## (e.g., to more efficiently handle y[i] <- x[k[i]] cases where all y's included).
    for(iRange in seq_along(finalIndexRanges)) {
        if(inherits(finalIndexRanges[[iRange]], 'indexRangeMatrixClass'))
            finalIndexRanges[[iRange]] <- finalIndexRanges[[iRange]]$toSequence()
        if(inherits(finalIndexRanges[[iRange]], 'indexRangeSequenceClass'))
            finalIndexRanges[[iRange]] <- finalIndexRanges[[iRange]]$toScalar()
    }
    
    ## Put final results in natural order (based on first slot for each indexRange) in case they are not already.
    finalIndexOrderStarts <- sapply(finalRangeToIndexSlot, `[`, 1)
    orderFinalIndexOrderStarts <- order(finalIndexOrderStarts)
    if(!identical(orderFinalIndexOrderStarts, seq_along(orderFinalIndexOrderStarts))) {
        finalIndexRanges <- finalIndexRanges[orderFinalIndexOrderStarts]
        finalRangeToIndexSlot <- finalRangeToIndexSlot[orderFinalIndexOrderStarts]
    }

    ## Remove duplicate columns (from cases where two indexRanges are used in a single rule).
    repeats <- duplicated(finalRangeToIndexSlot)

    return(
        varRangeClass$new(
        indexInfo = finalIndexRanges[!repeats],
        rangeToIndexSlot = finalRangeToIndexSlot[!repeats],
        varName = ifelse(is.null(varName), rule$toVarName, varName),
        fromStochRule = rule$stoch)
    )
}


checkForComplicatedCrossing <- function(fromSlots, fromVarRange) {
    usedRanges <- unique(fromVarRange$indexSlotToRange[fromSlots])
    ## equal to: `sapply(fromVarRange$rangeToIndexSlot, function(x) any(thisFromIndices %in% x))`
    if(length(fromSlots) > 1 && length(usedRanges) > 1 &&
       !identical(fromSlots, sort(unique(unlist(fromVarRange$rangeToIndexSlot[usedRanges])))))
        return(TRUE)
    return(FALSE)
}
