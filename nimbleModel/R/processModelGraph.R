## Code to take the declRules and graphRules and create full set of fractured calcRules and 'excluded' rhsRules

## assume we have graphRules as list of lists, indexed by fromVar
## e.g., graphRules[['mu']], graphRules[['x']]

 
## split up rhsRules to get rhsOnlyRules using exclude(), to extract parts of rhs that don't appear in LHS

## Not clear we need to generate RHSonlyRules
## There may be some non-uniqueness if we don't combine the results of
## running exclude on a rhsRule applied to another rhsRule
generateRHSonlyRules <- function(rhsOriginalRules, declRules) {
    rhsOnlyRules <- rhsOriginalRules

    ## Step 1: exclude each rhsRule based on other rhsRules
    ## Otherwise exclusion process with LHSrules can create redundant rhsOnlyRules
    ## Step 2: exclude each rhsRule with each LHSrule

    pos <- 1
    while(pos < length(rhsOnlyRules)) {
        newRules <- NULL
        mx <- length(rhsOnlyRules)
        for(i in (pos+1):mx) {
            if(rhsOnlyRules[[i]]$varName == rhsOnlyRules[[pos]]$varName) {
                ## Exclude gives back what needs to be kept - either the original rule or part of it.
                result <- exclude(rhsOnlyRules[[i]], rhsOnlyRules[[pos]])
                if(!is.null(result)) {  # some or no overlap
                    newRules <- c(newRules, result)
                    ## TODO: combine result and rhsOnlyRules[[pos]] (for sequence rules and maybe matrix rules)
                    ## if possible to reduce non-uniqueness.
                    ## But note that one can get a resulting rhsOnlyRule that is larger than a declaration RHS
                }
            } else newRules <- c(newRules, rhsOnlyRules[[i]])
        }
        rhsOnlyRules <- c(rhsOnlyRules[1:pos], newRules)
        pos <- pos + 1
    }
    for(pos in seq_along(declRules)) {
        newRules <- NULL
        for(i in seq_along(rhsOnlyRules)) {
            if(rhsOnlyRules[[i]]$varName == declRules[[pos]]$varName) {
                result <- exclude(rhsOnlyRules[[i]], declRules[[pos]])
                if(!is.null(result)) {
                    newRules <- c(newRules, result)
                }
            } else newRules <- c(newRules, rhsOnlyRules[[i]])
        }
        rhsOnlyRules <- newRules
    }
    tmp <- sapply(seq_along(rhsOnlyRules), function(idx) rhsOnlyRules[[idx]]$ID <- idx)
    return(rhsOnlyRules)
}

getChildren <- function(varRange, graphRules) {
    result <- lapply(graphRules, function(rule)
        applyGraphRule(varRange, rule))
    if(length(result) == 1 && is.null(result[[1]]))
        return(NULL)
    return(result)
}

generateCalcRules <- function(declRules, rhsOriginalRules, graphRules, recurseFracturing = FALSE) {
    ## Step 1: fracture LHS with rhsOriginalRules of same var
    ## e.g., mu[i] ~ dnorm(z[i], 1); y[2:3] <- mu[2:3]
    ## Fracture based on pieces of var potentially having different children.
    
    ## Step 2: fracture LHS based on same-var deps of other LHS 
    ## e.g., y[i] ~ dnorm(z[i], 1); z[j] ~ dnorm(0,1)
    ## Fracture based on pieces of var potentially having different parents.

    numRHSrules <- length(rhsOriginalRules)
    
    originalCalcRules <- lapply(declRules, function(rule)
        calcRuleClass$new(rule, NULL, NULL, rule$context, rule$constants)
        )

    ## Start process with known top calcRules 
    topRules <- !sapply(originalCalcRules, function(rule) rule$checkAnyRHS())
    calcRules <- c(originalCalcRules[topRules], originalCalcRules[!topRules])

    ## Use character representation of numbers to index calcRules as we remove fractured calcRules
    ## but don't want to have to modify the child/parent ids.
    tmp <- sapply(seq_along(calcRules), function(i) calcRules[[i]]$ID <- as.character(i))
    names(calcRules) <- sapply(calcRules, function(rule) rule$ID)
    
    ## fracture LHS of same varName as rhsRule
    pos <- 1
    start <- sum(topRules) + 1 # index of rules to be fractured

    fracturedRules <- rep(FALSE, length(calcRules))
    currentID <- length(calcRules)
    while(pos <= length(rhsOriginalRules)) {   # use while rather than for to match needed while in loop over calcRules
        rhsRange <- rhsOriginalRules[[pos]]$getFullRange()
        if(!rhsRange$isNone()) {
            ## Try to fracture all rules by looping over non-top rules.
            for(i in start:length(calcRules)) {
                if(!fracturedRules[i] && rhsRange$varName == calcRules[[i]]$varName) {
                    result <- fracture(calcRules[[i]], rhsRange, currentID = currentID,
                                       parentRule = NULL, currentRules = calcRules)
                    
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
    topRules <- !sapply(calcRules, function(rule) rule$checkAnyRHS())
    calcRules <- c(calcRules[topRules], calcRules[!topRules])
    
    pos <- 1  # index of fracturer
    start <- sum(topRules) + 1 # index of rules to be fractured
    fracturedRules <- rep(FALSE, length(calcRules))

    numOrigCalcRules <- length(calcRules)
##     while(pos <= numOrigCalcRules) {  ## originally `length(calcRules)` but that leads to very slow SSM processing
    while(pos <= length(calcRules)) {  ## originally `length(calcRules)` but that leads to very slow SSM processing
        if(!recurseFracturing && pos > numOrigCalcRules)
            break
        if(!fracturedRules[pos]) {
            varName <- calcRules[[pos]]$varName
            deps <- getChildren(
                calcRules[[pos]]$getFullRange(),
                graphRules[[varName]]$rules)
            if(!is.null(deps)) {
                deps <- deps[!sapply(deps, is.null)]
                ## TODO: don't try to fracture singletons (how could I detect this?)
                ## TODO: precompute the relevant rules to loop over to avoid if() checking
                for(d in seq_along(deps)) {
                    ## Try to fracture all remaining rules by looping over non-top rules.
                    for(i in start:length(calcRules)) {
                        if(!fracturedRules[i] && !is.null(deps[[d]]) && deps[[d]]$varName == calcRules[[i]]$varName) {
                            result <- fracture(calcRules[[i]], deps[[d]], currentID = currentID,
                                               parentRule = calcRules[[pos]], currentRules = calcRules)
                            
                            ## NULL result indicates complete overlap or no overlap, so leave as is.
                            ## If fracturing has occurred, add nodes to end.
                            if(!is.null(result)) {
                                calcRules <- c(calcRules, result)
                                fracturedRules[i] <- TRUE
                                fracturedRules <- c(fracturedRules, rep(FALSE, length(result)))
                                currentID <- currentID + length(result)
                            }
                        }
                        ## If parent rule has been fractured (state-space use case)
                        ## don't continue fracturing, as we'll fracture with the pieces later.
                        if(fracturedRules[pos]) break
                    }
                    ## Don't fracture with additional deps either.
                    if(fracturedRules[pos]) break
                }
            }
        }
        pos <- pos + 1
        ## start <- max(c(start, pos))  # this prevents SSM with intervening det nodes from working correctly
    }

    ## Find additional parent/children links (will this only be needed for SSM cases?)
    numCalcRules <- length(calcRules)
    if(numCalcRules > numOrigCalcRules)   # set children/parents for new rules
        for(pos in (numOrigCalcRules + 1):numCalcRules) {
            if(!fracturedRules[pos]) {
                varName <- calcRules[[pos]]$varName
                deps <- getChildren(
                    calcRules[[pos]]$getFullRange(),
                    graphRules[[varName]]$rules)
                if(!is.null(deps)) 
                    for(d in seq_along(deps)) 
                        ## Find additional parent/children relationships.
                        for(i in seq_len(numCalcRules)) 
                            if(!fracturedRules[i] && !is.null(deps[[d]]) && deps[[d]]$varName == calcRules[[i]]$varName) 
                                findLinks(calcRules[[i]], deps[[d]], parentRule = calcRules[[pos]])
            }
        }
     
    return(calcRules[!fracturedRules])
}

## how will getDependencies work and interact with set(s) of graphRules?

setEndNodes <- function(calcRules) {
    tmp <- sapply(calcRules, function(rule)
        rule$setStochDep(calcRules))
    tmp <- sapply(calcRules, function(rule)
        if(rule$stochDep) rule$unset('end') else rule$set('end'))
    invisible(0)
}

setTopNodes <- function(calcRules) {
    tmp <- sapply(calcRules, function(rule)
        rule$setStochParent(calcRules))
    tmp <- sapply(calcRules, function(rule)
        if(rule$stochParent) rule$unset('top') else rule$set('top'))
    invisible(0)
}

## setLatentNodes <- function(calcRules) {
##     tmp <- sapply(calcRules, function(rule)
##         if(rule$is_type('end') || rule$is_type('top')) rule$unset('latent'))
##     invisible(0)
## }
        

## TODO: should this be method of modelDefClass
processCyclicRules <- function(allCalcRules, modelDef) {
    ## Set sortIDs elementwise for nodes in a nodeRule for nodeRules involved in cyclic relationships
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
    
    ## Find calcRules whose indexing is offset, so that we can process each.
    ## Need to know if sortIDs are increasing or decreasing with the indexing.
    for(currentCyclicRule in cyclicRulesSet) {
        parentVars <- sapply(modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules, function(rule)
            rule$toVarName)
        idx <- which(parentVars %in% varNames[cyclicRulesSet])

        ## if(length(idx) != 1) 
        ##    if(length(unique(parentVars[idx])) > 1)  ## e.g. mu[i] <- mu[i-1] + z[i], with z[i] also in a cycle
        ##        return(allCalcRules) ## stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        ## might be able to handle this if restrict to direction being same mu[i] <- mu[i-1]+z[i] (can't be z[i+1])

        ## Multiple graphRules can result from AR(p) structure for p>1.
        upstreamGraphRules <- modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules[idx]
        offsets <- lapply(upstreamGraphRules, function(graphRule) sapply(graphRule$indexRules, function(rule) 
            if(is(rule, 'indexRuleClass_block')) return(rule$setupResults$offset) else return(0)
            ))

        ## Multiple indices in a rule are lagged.
        if(any(sapply(offsets, function(x)
            sum(x != 0)) > 1))
            return(allCalcRules) ## stop("new nimbleModel processing reached unexpected structure in cycle processing.")

        focalIndexRules <- lapply(offsets, function(x) which(x != 0))
        ## Any non-zero lags
        if(sum(sapply(focalIndexRules, length))) {
            thisFocalIndices <- sapply(seq_along(focalIndexRules), function(i)
                which(upstreamGraphRules[[i]]$indexSets$RHSindex2setID == focalIndexRules[[i]]))
            focalIndex <- unique(unlist(thisFocalIndices))  # unlist() deals with AR(p) case where there might be a non-block rule
            if(length(focalIndex) > 1)
                return(allCalcRules) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
            allCalcRules[[currentCyclicRule]]$multiSortIDindex <- focalIndex

            ## determine offsets
            offsets <- unlist(offsets)
            if(min(offsets) < 0 && max(offsets) > 0)
                return(allCalcRules)  # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
            focalRules <- c(focalRules, currentCyclicRule)
            directions <- c(directions, sign(sum(offsets)))
            focalIndices <- c(focalIndices, focalIndex)
        }
    }
    if(!length(focalRules))
        return(allCalcRules) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")

    ## Assign initial ordered, non-integer sortIDs to identified rule in cycle.
    ## It needs to be the identified rule because only for that rule do we know the relevant index.
    ## Relevant indices for other calcRules in cycle need to be determined from the identified rule.
    ## Fill in sortID vals for extent of the calcRule (not extent of the upstream rule)

    ## Process each of focalRules. For each rule, recursively look through parentRules.

    for(i in seq_along(focalRules)) {
        currentCyclicRule <- focalRules[i]
        focalIndex <- focalIndices[i]
    
        ## Need to find indexRule corresponding to the focalIndex
        focalIndexRule <- allCalcRules[[currentCyclicRule]]$graphRule$indexSets$LHSindex2setID[focalIndex]
        if(focalIndexRule == 0 ||
            !inherits(allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]], 'indexRuleClass_block'))
            return(allCalcRules) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        setup <- allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]]$setupResults

        ## TODO: extract out initial assignment as a function, since also done in `followUpstream`
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
        ## touched <- rep(FALSE, length(allCalcRules))
        
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
                                function(rule) rule$sortID))
    NAsortIDs <- is.na(tmpSortIDs)
    ## Can have ties if have distinct unrelated cycles. Use of `'first'` ensures no gaps in the sortIDs.
    rk <- rank(tmpSortIDs, ties.method = 'min')
    ## Remove gaps from duplicated ranks
    if(any(duplicated(rk))) {
        uniqs <- sort(unique(rk))
        lookup <- rep(NA, max(uniqs))
        lookup[uniqs] <- 1:length(uniqs)
        rk <- lookup[rk]
    }
    
    tmpSortIDs <- fullMaxSortID + rk
    tmpSortIDs[NAsortIDs] <- NA

    tmp <- sapply(seq_along(numSortIDs), function(i) {
        if(i == 1) {
            cyclicRules[[i]]$sortID <- tmpSortIDs[1:numSortIDs[1]]
        } else cyclicRules[[i]]$sortID <- tmpSortIDs[(numSortIDs[i-1]+1):numSortIDs[i]]
    })
    allCalcRules[cyclicRulesSet] <- cyclicRules
    return(allCalcRules)
}

setSortIDs <- function(calcRules) {
    sortIDs <- sapply(calcRules, function(rule)
        rule$setSortID(calcRules))

    ## TODO: remove as this seems redundant
    ## sortIDs <- sapply(calcRules, function(rule)
    ##    if(length(rule$sortID) == 1 && is.na(rule$sortID)) {
    ##        return(NA)
    ##    } else return(max(rule$sortID, na.rm = TRUE)))
    ## Now renumber so sortID=1 is first
    if(all(is.finite(sortIDs))) {
        mx <- max(sortIDs)
        tmp <- sapply(calcRules, function(rule)
            rule$sortID <- mx - rule$sortID + 1
            )
        return(TRUE)
    }
    return(FALSE)
}

followUpstream <- function(upstreamGraphRule, upstreamRules, childSortID, focalIndex, allCalcRules, cyclicRulesSet, varNames, sortIDs, fullMaxSortID, direction, eps, count = 0) {

    parentVar <- upstreamGraphRule$toVarName

    ## There could be multiple rules if model has two cycles involving a single variable.
    currentCyclicRules <- cyclicRulesSet[varNames[cyclicRulesSet] == parentVar]

    if(count > 10)  { ## prevent infinite recursion and stack overflow
        for(currentCyclicRule in currentCyclicRules)
            allCalcRules[[currentCyclicRule]]$sortID <- NA
        return(NULL)
    }

    focalIndexRule <- upstreamGraphRule$indexSets$RHSindex2setID[focalIndex]
    if(focalIndexRule == 0)  # non-block rule, nothing to follow
        return(NULL)
    focalIndex <- which(upstreamGraphRule$indexSets$LHSindex2setID == focalIndexRule)
    ## The upstreamGraphRule should only involve one index.
    if(length(focalIndex) != 1)
        return(NULL) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")

    for(currentCyclicRule in currentCyclicRules) {
        currentSortID <- allCalcRules[[currentCyclicRule]]$sortID
        initialized <- FALSE
        if(all(is.na(currentSortID))) {
            setup <- allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]]$setupResults
            childSortIDs <- sortIDs[allCalcRules[[currentCyclicRule]]$children]
            if(any(is.infinite(childSortIDs))) ## Another cycle downstream; can't handle this.
                return(NULL) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
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
        if(!inherits(upstreamGraphRule$indexRules[[focalIndexRule]], 'indexRuleClass_block'))
            return(NULL) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        childIndices <- setup$fromMin:setup$fromMax
        sortIDvals <- childSortID[childIndices] + eps
        currentIndices <- childIndices + as.integer(setup$offset)  # as.integer() because of identical() below
        
        focalIndexRule <- allCalcRules[[currentCyclicRule]]$index2setID[focalIndex]
        if(!inherits(allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]], 'indexRuleClass_block'))
            return(NULL) # stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        
        setup <- allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]]$setupResults
        currentCalcRuleIndices <- setup$fromMin:setup$fromMax
        
        ## currentIndices may include indices not in the calcRule, if the rule has been fractured,
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
        
        if(!sum(wh) && !initialized)   ## only complete cycle if get back to a calcRule and don't need to modify it
            next # return(NULL)

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
        next # return(NULL)
    }
    return(NULL)
}
