## Various functions used in creating various kinds of rules and determining graph structure.


## Split up original RHS rules from declarations to get `rhsOnlyRules` using `exclude()`,
## to extract parts of rhs that don't appear in LHS.

## CHECK: Not clear we need to generate RHSonlyRules
## There may be some non-uniqueness if we don't combine the results of
## running `exclude` on a rhsRule applied to another rhsRule.
makeRHSonlyRules <- function(rhsOriginalRules, declRules, constants = list()) {
    rhsOnlyRules <- rhsOriginalRules

    ## Step 1: exclude elements of each rhsRule that overlap with 'earlier' rhsRules.
    ## Otherwise exclusion process with declRules can create redundant rhsOnlyRules.

    pos <- 1
    while(pos < length(rhsOnlyRules)) {
        newRules <- NULL
        mx <- length(rhsOnlyRules)
        for(i in (pos+1):mx) {
            if(rhsOnlyRules[[i]]$varName == rhsOnlyRules[[pos]]$varName) {
                ## `exclude` gives back what needs to be kept - either the original rule or part of it.
                result <- exclude(rhsOnlyRules[[i]], rhsOnlyRules[[pos]], constants)
                if(!is.null(result)) {  # some or no overlap
                    newRules <- c(newRules, result)
                    ## FUTURE: combine result and rhsOnlyRules[[pos]] (for sequence rules and maybe matrix rules)
                    ## if possible to reduce non-uniqueness.
                    ## But be careful as one could get a resulting rhsOnlyRule that is larger than a declaration RHS
                }
            } else newRules <- c(newRules, rhsOnlyRules[[i]])
        }
        rhsOnlyRules <- c(rhsOnlyRules[1:pos], newRules)
        pos <- pos + 1
    }

    ## Step 2: exclude elements of each rhsRule that overlap with declRules.
    
    for(pos in seq_along(declRules)) {
        newRules <- NULL
        for(i in seq_along(rhsOnlyRules)) {
            if(rhsOnlyRules[[i]]$varName == declRules[[pos]]$varName) {
                result <- exclude(rhsOnlyRules[[i]], declRules[[pos]], constants)
                if(!is.null(result)) {
                    newRules <- c(newRules, result)
                }
            } else newRules <- c(newRules, rhsOnlyRules[[i]])
        }
        rhsOnlyRules <- newRules  # Next declRule will split on current rhsOnlyRules.
    }
    
    ## Assign unique ID sequentially (these will not be unique w.r.t. calcRule IDs).
    ## CHECK: do we use these IDs?
    sapply(seq_along(rhsOnlyRules), function(idx) rhsOnlyRules[[idx]]$ID <- idx)
    return(rhsOnlyRules)
}


## Split original calcRules based on intersections with RHS rules and with dependents of other LHS rules.
## This is needed so that we can determine top/end/latent nodes.
makeCalcRules <- function(calcRules, rhsOriginalRules, graphRules, recurseFracturing = FALSE, constants = list()) {
    ## Step 1: fracture LHS with rhsOriginalRules for same variable.
    ## e.g., mu[i] ~ dnorm(z[i], 1); y[2:3] <- mu[2:3]
    ## I.e., fracture based on pieces of variable potentially having different children.
    
    ## Step 2: fracture LHS based on same-variable dependents of other LHS. 
    ## e.g., y[i] ~ dnorm(z[i], 1); z[j] ~ dnorm(0,1)
    ## I.e., fracture based on pieces of variable potentially having different parents.

    ## Step 1:
    numRHSrules <- length(rhsOriginalRules)
    pos <- 1
    fracturedRules <- rep(FALSE, length(calcRules))
    currentID <- length(calcRules)

    ## Use `while` rather than `for` to match needed `while` in loop over calcRules in Step 2.
    while(pos <= length(rhsOriginalRules)) {   
        rhsRange <- rhsOriginalRules[[pos]]$getFullRange()
        if(!rhsRange$isNone()) {
            ## Try to fracture all rules by looping over rules.
            for(i in seq_along(calcRules)) {
                if(!fracturedRules[i] && rhsRange$varName == calcRules[[i]]$varName) {
                    result <- fracture(calcRules[[i]], rhsRange, currentID = currentID,
                                       parentRule = NULL, currentRules = calcRules, constants)
                    
                    ## NULL result indicates complete overlap or no overlap, so leave as is.
                    ## If fracturing has occurred, add nodes to end.
                    if(!is.null(result)) {
                        calcRules <- c(calcRules, result)
                        fracturedRules[i] <- TRUE
                        fracturedRules <- c(fracturedRules, rep(FALSE, length(result)))
                        currentID <- currentID + length(result)
                        
                    }
                }
            }
        }
        pos <- pos + 1
    }

    calcRules <- calcRules[!fracturedRules]
    ## Determine rules that must be top rules as don't need to try to fracture those.
    knownTopRules <- sapply(calcRules, function(rule) rule$checkAllRHSconstants(constants))
    start <- sum(knownTopRules) + 1 # first index of rules to be fractured
    calcRules <- c(calcRules[knownTopRules], calcRules[!knownTopRules])

    ## Step 2:
    pos <- 1  # index of fracturer
    fracturedRules <- rep(FALSE, length(calcRules))
    numOrigCalcRules <- length(calcRules)

    while(pos <= length(calcRules)) {  
        if(!recurseFracturing && pos > numOrigCalcRules)
            break
        if(!fracturedRules[pos]) {
            varName <- calcRules[[pos]]$varName
            deps <- getChildren(
                calcRules[[pos]]$getFullRange(),
                graphRules[[varName]]$rules)
            ## FUTURE: don't try to fracture singletons (how could I detect this?).
            ## FUTURE: precompute the relevant rules to loop over to avoid if() checking.
            for(d in seq_along(deps)) {
                ## Try to fracture all remaining rules by looping over non-top rules.
                for(i in start:length(calcRules)) {
                    if(!fracturedRules[i] && deps[[d]]$varName == calcRules[[i]]$varName) {
                        result <- fracture(calcRules[[i]], deps[[d]], currentID = currentID,
                                           parentRule = calcRules[[pos]], currentRules = calcRules, constants)
                        
                        ## NULL result indicates complete overlap or no overlap, so leave as is.
                        ## If fracturing has occurred, add nodes to end.
                        if(!is.null(result)) {
                            calcRules <- c(calcRules, result)
                            fracturedRules[i] <- TRUE
                            fracturedRules <- c(fracturedRules, rep(FALSE, length(result)))
                            currentID <- currentID + length(result)
                        }
                    }
                    ## If parent rule has been fractured (state-space use case),
                    ## don't continue fracturing, as we'll fracture with the pieces later.
                    if(fracturedRules[pos]) break
                }
                ## Don't fracture parent rule with additional deps either.
                if(fracturedRules[pos]) break
            }
        }
        pos <- pos + 1
    }

    calcRules <- calcRules[!fracturedRules]
    
    ## Find additional parent/children links from new rules resulting from fracturing,
    ## Since these new rules have not gone through fracturing when `recurseFracturing` is `FALSE`.
    if(!recurseFracturing && sum(fracturedRules))
        setRelationships(calcRules, graphRules, startPos = numOrigCalcRules + 1 - sum(fracturedRules))
    
    return(calcRules)
}

## Find all children of a given varRange.
getChildren <- function(varRange, graphRules) {
    result <- lapply(graphRules, function(rule)
        rule$apply(varRange))
    return(result[!sapply(result, is.null)])
}

## Find parent-child links by looking for all children of every rule.
setRelationships <- function(calcRules, graphRules, startPos = 1) {
    setToCheck <- startPos:length(calcRules)
    for(pos in setToCheck) {
        varName <- calcRules[[pos]]$varName
        deps <- getChildren(
            calcRules[[pos]]$getFullRange(),
            graphRules[[varName]]$rules)
        for(d in seq_along(deps)) {
            for(i in seq_along(calcRules))
                if(deps[[d]]$varName == calcRules[[i]]$varName) 
                    checkAndCreateLink(calcRules[[i]], deps[[d]], parentRule = calcRules[[pos]])
        }
    }
    invisible(NULL)
}

## Find endRules based on not having stochastic dependents.
setEndRules <- function(calcRules) {
    sapply(calcRules, function(rule)
        rule$setStochDep(calcRules))
    sapply(calcRules, function(rule)
        if(rule$stochDep) rule$unset('end') else rule$set('end'))
    invisible(NULL)
}

## Find topRules based on not having stochastic parents.
setTopRules <- function(calcRules) {
    sapply(calcRules, function(rule)
        rule$setStochParent(calcRules))
    sapply(calcRules, function(rule)
        if(rule$stochParent) rule$unset('top') else rule$set('top'))
    invisible(NULL)
}

## Set sortIDs based on recursive calls to setSortID for individual rules.
setSortIDs <- function(calcRules) {
    sortIDs <- sapply(calcRules, function(rule)
        rule$setSortID(calcRules))
    ## Reorder sortIDs so that smallest sortID values are at top.
    if(all(is.finite(sortIDs))) {
        mx <- max(sortIDs)
        sapply(calcRules, function(rule)
            rule$sortID <- mx - rule$sortID + 1
            )
        return(TRUE) ## All sortIDs resolved.
    }
    return(FALSE)  ## Cyclic or state-space type case.
}


## Walks graph to find children or parents, by default
## stopping at stochastic nodes, unless requested to go through (`follow = TRUE`)
## or to stop at immediate parent or child (`immediateOnly = TRUE`).
## Result is a set of varRanges (not nodeRanges), so users may need to
## pass result through `getNodes`.
## This is the meat of `getDependencies` and `getParents`.
traverseGraph <- function(streamRules, declRules,
                          nodes, down, self = TRUE,
                          follow = FALSE, immediateOnly = FALSE) {
                          
    if(is(nodes, 'varRangeClass')) nodes <- list(nodes)  # We use `lapply` on 'nodes' later.
    
    if(!all(is.character(nodes) | sapply(nodes, function(node) is(node, 'varRangeClass'))))
        stop("`nodes` must be variable names or `varRange`s.")

    results <- traverseGraphRecurse(streamRules, nodes, down, follow, immediateOnly)

    if(self) {
        ## Need to handle "self" for three cases: (a) when an input node is a full variable,
        ## (b) character expression for a range, or (c) an actual varRange or nodeRange.
        varNames <- sapply(nodes, getVarName)
        vars <- nodes == varNames
        selfRangeFromVars <- flatten(lapply(nodes[vars],
                                       function(varName)
                                           lapply(declRules[[varName]]$rules,
                                                  function(declRule) declRule$getFullRange())))
        
        charRanges <- is.character(nodes) & !vars
        selfRangeFromCharRanges <- flatten(lapply(nodes[charRanges],
                                              function(node) {
                                                  lapply(declRules[[getVarName(node)]]$rules,
                                                         function(declRule) {
                                                             tmp <- declRule$apply(node)
                                                             if(is.null(tmp)) NULL else tmp$toVarRange()
                                                         })
                                                  }))
        if(identical(selfRangeFromCharRanges, list(NULL)))
            selfRangeFromCharRanges <- NULL
        selfRangeFromNodes <- lapply(nodes[!vars & !charRanges],
                                     function(node)
                                         if(is(node, 'nodeRangeClass')) {
                                             return(node$toVarRange())
                                         } else return(node))
        results <- c(selfRangeFromNodes, selfRangeFromVars, selfRangeFromCharRanges, results)
    }
    
    if(!length(results))
        return(NULL)
    return(removeDuplicateVarRanges(results))
}

traverseGraphRecurse <- function(rules, nodes, down, follow = FALSE, immediateOnly = FALSE, firstPass = TRUE) {
    results <- flatten(lapply(nodes, function(node) applyRules(rules, node)))
    if(immediateOnly)
        return(results)
    ## Following graph more than one level requires knowing whether a link is
    ## from a stochastic declaration. Determining this has to be done differently
    ## for upward vs. downward traversal.
    if(!down && !firstPass && !follow) {
        ## For upward traversal, check current rule to see if continue upwards, but always go up on first pass.
        ## (Because we need to determine stochasticity of the next rule up, not stochasticity of starting rule.
        stoch <- sapply(results, function(varRange) varRange$fromStochRule)
        results <- results[!stoch]  ## Stop here if upwards involves stochastic rule, excluding the upwards result.
    }    
    propagators <- results
    if(!follow && down) {
        ## For downward traversal, stop propagating at stochastic cases, but results included. 
        stoch <- sapply(propagators, function(varRange) varRange$fromStochRule)
        propagators <- propagators[!stoch]
    }
    ## Continue traversing.
    if(length(propagators)) {
        results <- c(results, traverseGraphRecurse(rules, propagators, down, follow, firstPass = FALSE))
    } else {
        return(results)
    }
}

## Utility for determining which varRule is needed for a node and
## applying it.
applyRules <- function(rules, node) {
    varName <- getVarName(node)  
    if(varName %in% names(rules)) {
        return(rules[[varName]]$apply(node))
    } else return(NULL)
}

## Set sortIDs elementwise for nodes in a nodeRule for nodeRules involved in cyclic relationships.
## Note that this could involve one or more variables, e.g. `y[i] ~ dnorm(z[i],1); z[i] ~ dnorm(y[i-1],1)`,
## in addition to standard state space style `z[i] ~ dnorm(z[i-1],1)`. And there could be multiple distinct
## state-space structures in a model.

## This code is complicated. We have testing for a variety of tricky cases, but there may be
## unconsidered additional cases.
processCyclicRules <- function(allCalcRules, modelDef) {
    eps <- 1e-12 # increment that ensures unique increasing sortID values

    sortIDs <- sapply(allCalcRules, function(rule) rule$sortID)
    varNames <- sapply(allCalcRules, function(rule) rule$varName) 
    cyclicRulesSet <- which(is.na(sortIDs))
    if(any(is.finite(sortIDs))) {
        fullMaxSortID <- max(sortIDs[is.finite(sortIDs)])
    } else fullMaxSortID <- 0
               
    focalRules <- NULL
    directions <- NULL
    focalIndices <- NULL
    
    ## Find calcRules whose indexing is offset, so that we can process each as potential cyclic cases.
    ## Need to know if sortIDs are increasing or decreasing with the indexing.
    for(currentCyclicRule in cyclicRulesSet) {
        parentVars <- sapply(modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules, function(rule)
            rule$toVarName)
        idx <- which(parentVars %in% varNames[cyclicRulesSet])

        ## Multiple graphRules can result from AR(p) structure for p>1.
        upstreamGraphRules <- modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules[idx]
        offsets <- lapply(upstreamGraphRules, function(graphRule) sapply(graphRule$indexRules, function(rule) 
            if(is(rule, 'indexRuleBlockClass')) return(rule$setupResults$offset) else return(0)
            ))

        ## Multiple indices in a rule are lagged.
        if(any(sapply(offsets, function(x)
            sum(x != 0)) > 1))
            return(allCalcRules)

        focalIndexRules <- lapply(offsets, function(x) which(x != 0))
        ## Any non-zero lags,
        if(sum(sapply(focalIndexRules, length))) {
            thisFocalIndices <- sapply(seq_along(focalIndexRules), function(i)
                which(upstreamGraphRules[[i]]$indexSets$fromIndexSlotToSet == focalIndexRules[[i]]))
            focalIndex <- unique(unlist(thisFocalIndices))  # unlist() deals with AR(p) case where there might be a non-block rule
            if(length(focalIndex) > 1)
                return(allCalcRules)
            allCalcRules[[currentCyclicRule]]$multiSortIDindex <- focalIndex

            ## Determine offsets,
            offsets <- unlist(offsets)
            if(min(offsets) < 0 && max(offsets) > 0)
                return(allCalcRules)  
            focalRules <- c(focalRules, currentCyclicRule)
            directions <- c(directions, sign(sum(offsets)))
            focalIndices <- c(focalIndices, focalIndex)
        }
    }
    if(!length(focalRules))
        return(allCalcRules) 


    ## Process each of focalRules. For each rule, set initial sortID values and recursively look through parentRules,
    ## setting their sortID values.
    for(i in seq_along(focalRules)) {
        currentCyclicRule <- focalRules[i]
        focalIndex <- focalIndices[i]
    
        ## Need to find indexRule corresponding to the focalIndex
        focalIndexRule <- allCalcRules[[currentCyclicRule]]$fullRule$indexSets$toIndexSlotToSet[focalIndex]
        if(focalIndexRule == 0 ||
            !inherits(allCalcRules[[currentCyclicRule]]$fullRule$indexRules[[focalIndexRule]], 'indexRuleBlockClass'))
            return(allCalcRules) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        setup <- allCalcRules[[currentCyclicRule]]$fullRule$indexRules[[focalIndexRule]]$setupResults

        ## Assign initial ordered, non-integer sortID values to focal rule.
        ## Relevant indices for other calcRules in cycle need to be determined from the identified rule.
        ## It needs to be the identified rule because only for that rule do we know the relevant index.
        ## TODO: extract out initial assignment as a function, since code repeated in `followUpstream`
        if(all(is.na(allCalcRules[[currentCyclicRule]]$sortID))) {
            childSortIDs <- sortIDs[allCalcRules[[currentCyclicRule]]$children]
            if(any(is.infinite(childSortIDs))) ## Another cycle downstream; can't handle this.
                return(allCalcRules) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
            if(any(is.finite(childSortIDs)))
                maxSortID <- max(childSortIDs[is.finite(childSortIDs)]) else maxSortID <- fullMaxSortID
            
            sortIDvals <- eps + seq(from = maxSortID,
                              to = maxSortID + setup$fromMax - setup$fromMin,
                              by = 1)
            ## Adjust for 'direction' of 'time'.
            if(directions[i] < 0)
                sortIDvals <- rev(sortIDvals)
            indices <- setup$fromMin:setup$fromMax
            allCalcRules[[currentCyclicRule]]$sortID[indices] <- sortIDvals
        }
        
        ## Walk through parent calcRules involved in cycle and assign sortID based on upstream graphRule.

        childSortID <- allCalcRules[[currentCyclicRule]]$sortID
        ## Determine the graphRule involved in getting next upstream calcRule.
        parentVars <- sapply(modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules,
                             function(rule) rule$toVarName)
        idx <- which(parentVars %in% varNames[cyclicRulesSet])
        upstreamGraphRules <- modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules[idx]
        sapply(upstreamGraphRules, followUpstream, modelDef$upstreamRules, childSortID, focalIndex, allCalcRules,
               cyclicRulesSet, varNames, sortIDs, fullMaxSortID, directions[i], eps)
    }
    
    ## Set up actual integer-valued sortIDs.
    cyclicRules <- allCalcRules[cyclicRulesSet]
    numSortIDs <- cumsum(sapply(cyclicRules, function(rule) length(rule$sortID)))
    tmpSortIDs <- unlist(lapply(cyclicRules,
                                function(rule) rule$sortID), use.names = FALSE)
    NAsortIDs <- is.na(tmpSortIDs)
    ## Can have ties if have distinct unrelated cycles. 
    rk <- rank(tmpSortIDs, ties.method = 'min')
    ## Remove gaps from duplicated ranks.
    if(any(duplicated(rk))) {
        uniqs <- sort(unique(rk))
        lookup <- rep(NA, max(uniqs))
        lookup[uniqs] <- 1:length(uniqs)
        rk <- lookup[rk]
    }
    
    tmpSortIDs <- fullMaxSortID + rk
    tmpSortIDs[NAsortIDs] <- NA

    sapply(seq_along(numSortIDs), function(i) {
        if(i == 1) {
            cyclicRules[[i]]$sortID <- tmpSortIDs[1:numSortIDs[1]]
        } else cyclicRules[[i]]$sortID <- tmpSortIDs[(numSortIDs[i-1]+1):numSortIDs[i]]
        ## Clean up unneeded NAs.
        nonNAs <- which(!is.na(cyclicRules[[i]]$sortID))
        if(length(nonNAs))
            if(length(nonNAs) == 1) {
                cyclicRules[[i]]$sortID <- cyclicRules[[i]]$sortID[nonNAs]
            } else cyclicRules[[i]]$sortID <- cyclicRules[[i]]$sortID[1:max(nonNAs)]
    })
    
    allCalcRules[cyclicRulesSet] <- cyclicRules
    return(allCalcRules)
}

## Follow cycles until reach a previously encountered rule and don't need to modify it.
followUpstream <- function(upstreamGraphRule, upstreamRules, childSortID, focalIndex,
                           allCalcRules, cyclicRulesSet, varNames, sortIDs, fullMaxSortID,
                           direction, eps, count = 0) {

    parentVar <- upstreamGraphRule$toVarName

    ## There could be multiple rules if model has two cycles involving a single variable.
    currentCyclicRules <- cyclicRulesSet[varNames[cyclicRulesSet] == parentVar]

    if(count > 10)  { ## prevent infinite recursion and stack overflow
        for(currentCyclicRule in currentCyclicRules)
            allCalcRules[[currentCyclicRule]]$sortID <- NA
        return(NULL)
    }

    focalIndexRule <- upstreamGraphRule$indexSets$fromIndexSlotToSet[focalIndex]
    if(focalIndexRule == 0)  # non-block rule, nothing to follow
        return(NULL)
    focalIndex <- which(upstreamGraphRule$indexSets$toIndexSlotToSet == focalIndexRule)
    ## The upstreamGraphRule should only involve one index.
    if(length(focalIndex) != 1)
        return(NULL) 

    for(currentCyclicRule in currentCyclicRules) {
        currentSortID <- allCalcRules[[currentCyclicRule]]$sortID
        initialized <- FALSE
        if(all(is.na(currentSortID))) {
            setup <- allCalcRules[[currentCyclicRule]]$fullRule$indexRules[[focalIndexRule]]$setupResults
            childSortIDs <- sortIDs[allCalcRules[[currentCyclicRule]]$children]
            if(any(is.infinite(childSortIDs))) ## Another cycle downstream; can't handle this.
                return(NULL) 
            if(any(is.finite(childSortIDs)))
                maxSortID <- max(childSortIDs[is.finite(childSortIDs)]) else maxSortID <- fullMaxSortID
            
            sortIDvals <- eps + seq(from = maxSortID,
                              to = maxSortID + setup$fromMax - setup$fromMin,
                              by = 1)
            ## Adjust for 'direction' of 'time'.
            if(direction < 0)
                sortIDvals <- rev(sortIDvals)
            indices <- setup$fromMin:setup$fromMax
            allCalcRules[[currentCyclicRule]]$sortID[indices] <- sortIDvals
            currentSortID <- allCalcRules[[currentCyclicRule]]$sortID
            allCalcRules[[currentCyclicRule]]$multiSortIDindex <- focalIndex
            initialized <- TRUE
        }
        
        ## Determine sortID incrementing based on graphRule.
        setup <- upstreamGraphRule$indexRules[[focalIndexRule]]$setupResults
        if(!inherits(upstreamGraphRule$indexRules[[focalIndexRule]], 'indexRuleBlockClass'))
            return(NULL) 
        childIndices <- setup$fromMin:setup$fromMax
        sortIDvals <- childSortID[childIndices] + eps
        currentIndices <- childIndices + as.integer(setup$offset)  # as.integer() because of identical() below
        
        focalIndexRule <- allCalcRules[[currentCyclicRule]]$indexSlotToSet[focalIndex]
        if(!inherits(allCalcRules[[currentCyclicRule]]$fullRule$indexRules[[focalIndexRule]], 'indexRuleBlockClass'))
            return(NULL) 
        
        setup <- allCalcRules[[currentCyclicRule]]$fullRule$indexRules[[focalIndexRule]]$setupResults
        currentCalcRuleIndices <- setup$fromMin:setup$fromMax
        
        ## `currentIndices` may include indices not in the calcRule, if the rule has been fractured,
        ## so do not update sortID for elements not in the calcRule, using setup info for the calcRule.
        indicesToUpdate <- currentIndices %in% currentCalcRuleIndices
        
        ## If new sortID values based on graph are higher than previously assigned,
        ## or current sortID is NA, update the sortID values.
        wh <- (is.na(allCalcRules[[currentCyclicRule]]$sortID[currentIndices]) &
               !is.na(sortIDvals)) |
            allCalcRules[[currentCyclicRule]]$sortID[currentIndices] < sortIDvals
        wh[is.na(wh)] <- FALSE
        wh[!indicesToUpdate] <- FALSE
        if(any(wh))
            allCalcRules[[currentCyclicRule]]$sortID[currentIndices][wh] <- sortIDvals[wh]
        
        if(!sum(wh) && !initialized)   ## Only complete cycle if get back to a calcRule and don't need to modify it.
            next
        
        ## If no indices to update, we have disjoint indices so don't continue upstream.
        if(!sum(indicesToUpdate, na.rm = TRUE))
            next
        
        childSortID <- allCalcRules[[currentCyclicRule]]$sortID

        ## Determine the graphRule involved in getting next upstream calcRule.
        parentVars <- sapply(upstreamRules[[varNames[currentCyclicRule]]]$rules,
                             function(rule) rule$toVarName)
        idx <- which(parentVars %in% varNames[cyclicRulesSet])
        
        upstreamGraphRules <- upstreamRules[[varNames[currentCyclicRule]]]$rules[idx]
        sapply(upstreamGraphRules, followUpstream, upstreamRules, childSortID, focalIndex, allCalcRules,
               cyclicRulesSet, varNames, sortIDs, fullMaxSortID, direction, eps, count+1)
        next 
    }
    return(NULL)
}

reprioritizeColonOperator <- function(code) {
    split.code <- strsplit(safeDeparse(code, warn = TRUE), ":")
    if(length(split.code[[1]]) == 2)
        return(
            parse(
                text = paste0("(",
                              split.code[[1]][1],
                              "):(",
                              split.code[[1]][2],
                              ")"),
                keep.source = FALSE)[[1]])
    if(length(split.code[[1]]) > 2)
        stop("could not process colon operator in `", safeDeparse(code), "`.")
    return(code)
}
