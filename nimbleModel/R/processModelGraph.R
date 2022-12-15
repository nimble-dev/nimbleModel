## Code to take the declRules and graphRules and create full set of fractured calcRules and 'excluded' rhsRules

## assume we have graphRules as list of lists, indexed by parentVar
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

generateCalcRules <- function(declRules, rhsOriginalRules, graphRules) {
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
    while(pos <= numOrigCalcRules) {  ## originally `length(calcRules)` but that leads to very slow SSM processing
        varName <- calcRules[[pos]]$varName
        deps <- getChildren(
            calcRules[[pos]]$getFullRange(),
            graphRules[[varName]]$rules)
        if(!is.null(deps)) {
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
        pos <- pos + 1
        # start <- max(c(start, pos))  # this prevents SSM with intervening det nodes from working correctly
    }

    ## Find additional parent/children links (will this only be needed for SSM cases?)
    numCalcRules <- length(calcRules)
    if(numCalcRules > numOrigCalcRules)   # set children/parents for new rules
        for(pos in (numOrigCalcRules + 1):numCalcRules) {
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
    fullMaxSortID <- max(sortIDs[is.finite(sortIDs)])

    found <- FALSE

    ## Find calcRule whose indexing is offset as this will determine whether
    ## sortIDs are increasing or decreasing with the indexing.
    for(currentCyclicRule in cyclicRulesSet) {
        parentVars <- sapply(modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules, function(rule)
            rule$childVar)
        idx <- which(parentVars %in% varNames[cyclicRulesSet])
        if(length(idx) != 1) stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        upstreamGraphRule <- modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules[[idx]]
        offsets <- sapply(upstreamGraphRule$indexRules, function(rule) 
            if(is(rule, 'indexRuleClass_block')) return(rule$setupResults$offset) else return(0)
            )
        if(sum(offsets != 0) > 1) 
            stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        if(sum(offsets)) {
            focalIndexRule <- which(offsets != 0)
            ## Determine which index is involved.
            focalIndex <- which(upstreamGraphRule$indexSets$RHSindex2setID == focalIndexRule)
            allCalcRules[[currentCyclicRule]]$multiSortIDindex <- focalIndex
            offset <- offsets[offsets != 0]
            if(abs(offset) != 1)
                stop("new nimbleModel processing reached unexpected structure in cycle processing.")
            found <- TRUE
            break
        }
    }
    if(!found)
        stop("new nimbleModel processing reached unexpected structure in cycle processing.")

    ## Assign initial ordered, non-integer sortIDs to identified rule in cycle.
    ## It needs to be the identified rule because only for that rule do we know the relevant index.
    ## Relevant indices for other calcRules in cycle need to be determined from the identified rule.
    ## Fill in sortID vals for extent of the calcRule (not extent of the upstream rule)

    ## Need to find indexRule corresponding to the focalIndex
    focalIndexRule <- allCalcRules[[currentCyclicRule]]$graphRule$indexSets$LHSindex2setID[focalIndex]
    if(!inherits(allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]], 'indexRuleClass_block'))
        stop("new nimbleModel processing reached unexpected structure in cycle processing.")
    setup <- allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]]$setupResults
    childSortIDs <- sortIDs[allCalcRules[[currentCyclicRule]]$children]
    if(any(is.finite(childSortIDs)))
        maxSortID <- max(childSortIDs[is.finite(childSortIDs)]) else maxSortID <- fullMaxSortID
    
    sortIDvals <- seq(from = maxSortID + eps,
                      to = maxSortID + eps + setup$from_max - setup$from_min,
                      by = 1)
    ## Adjust for 'direction' of 'time'.
    if(offset == -1)
        sortIDvals <- rev(sortIDvals)
    indices <- setup$from_min:setup$from_max
    allCalcRules[[currentCyclicRule]]$sortID[indices] <- sortIDvals
    
    ## Walk through parent calcRules involved in cycle and assign sortID based on upstream graphRule.    
    touched <- rep(FALSE, length(allCalcRules))
    
    while(TRUE) {
        ## focalIndex for the child calcRule.
        childSortID <- allCalcRules[[currentCyclicRule]]$sortID

        ## Determine the graphRule involved in getting next upstream calcRule.
        parentVars <- sapply(modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules,
                             function(rule) rule$childVar)
        idx <- which(parentVars %in% varNames[cyclicRulesSet])
        parentVar <- parentVars[idx]
        if(length(idx) != 1) stop("new nimbleModel processing reached unexpected structure in cycle processing.")

        upstreamGraphRule <- modelDef$upstreamRules[[varNames[currentCyclicRule]]]$rules[[idx]]
        ## Determine the calcRule under consideration, and focalIndex for that calcRule.
        currentCyclicRule <- cyclicRulesSet[varNames[cyclicRulesSet] == parentVar]
        if(touched[currentCyclicRule]) break

        focalIndexRule <- upstreamGraphRule$indexSets$RHSindex2setID[focalIndex]
        focalIndex <- which(upstreamGraphRule$indexSets$LHSindex2setID == focalIndexRule)
        ## The graphRule should only involve one index.
        if(length(focalIndex) != 1) stop("new nimbleModel processing reached unexpected structure in cycle processing.")

        ## Assign default sortID based on graph children if not set already.
        ## This is needed for a calcRule for which one of its elements does not have
        ## any of the rules in the cycle as children but has a rule outside the cycle
        ## a child.
        currentSortID <- allCalcRules[[currentCyclicRule]]$sortID
        if(all(is.na(currentSortID))) {
            setup <- allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]]$setupResults
            childSortIDs <- sortIDs[allCalcRules[[currentCyclicRule]]$children]
            if(any(is.finite(childSortIDs)))
                maxSortID <- max(childSortIDs[is.finite(childSortIDs)]) else maxSortID <- fullMaxSortID
            
            sortIDvals <- seq(from = maxSortID + eps,
                              to = maxSortID + eps + setup$from_max - setup$from_min,
                              by = 1)
            ## Adjust for 'direction' of 'time'.
            if(offset == -1)
                sortIDvals <- rev(sortIDvals)
            indices <- setup$from_min:setup$from_max
            allCalcRules[[currentCyclicRule]]$sortID[indices] <- sortIDvals
            currentSortID <- allCalcRules[[currentCyclicRule]]$sortID
            allCalcRules[[currentCyclicRule]]$multiSortIDindex <- focalIndex
        }

        
        ## Determine sortID incrementing based on graphRule.
        setup <- upstreamGraphRule$indexRules[[focalIndexRule]]$setupResults
        if(!inherits(upstreamGraphRule$indexRules[[focalIndexRule]], 'indexRuleClass_block'))
            stop("new nimbleModel processing reached unexpected structure in cycle processing.")
        childIndices <- setup$from_min:setup$from_max
        sortIDvals <- childSortID[childIndices] + eps
        currentIndices <- childIndices + as.integer(setup$offset)  # as.integer() because of identical() below
        
        focalIndexRule <- allCalcRules[[currentCyclicRule]]$index2setID[focalIndex]
        if(!inherits(allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]], 'indexRuleClass_block'))
            stop("new nimbleModel processing reached unexpected structure in cycle processing.")

        setup <- allCalcRules[[currentCyclicRule]]$graphRule$indexRules[[focalIndexRule]]$setupResults
        currentCalcRuleIndices <- setup$from_min:setup$from_max
        
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
        allCalcRules[[currentCyclicRule]]$sortID[currentIndices][wh] <- sortIDvals[wh]

        if(!sum(wh))   ## only complete cycle if get back to a calcRule and don't need to modify it
            touched[currentCyclicRule] <- TRUE
    }


    ## Set up actual integer-valued sortIDs.
    cyclicRules <- allCalcRules[cyclicRulesSet]
    numSortIDs <- cumsum(sapply(cyclicRules, function(rule) length(rule$sortID)))
    tmpSortIDs <- unlist(lapply(cyclicRules,
                                function(rule) rule$sortID))
    NAsortIDs <- is.na(tmpSortIDs)
    tmpSortIDs <- fullMaxSortID + rank(tmpSortIDs)
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
    tmp <- sapply(calcRules, function(rule)
        rule$setSortID(calcRules))
    ## Now renumber so sortID=1 is first
    sortIDs <- sapply(calcRules, function(rule)
        if(length(rule$sortID) == 1 && is.na(rule$sortID)) {
            return(NA)
        } else return(max(rule$sortID, na.rm = TRUE)))
    if(all(is.finite(sortIDs))) {
        mx <- max(sortIDs)
        tmp <- sapply(calcRules, function(rule)
            rule$sortID <- mx - rule$sortID + 1
            )
        return(TRUE)
    }
    return(FALSE)
}

