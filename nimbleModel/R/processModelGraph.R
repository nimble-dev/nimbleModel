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

    ## TODO: check removal process and if need 'rulesToRemove'
    
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

getDependencies <- function(varRange, graphRules) {
    lapply(graphRules, function(rule)
        applyGraphIndexRules(varRange, rule))
}

generateCalcRules <- function(declRules, rhsOriginalRules, graphRules) {
    ## Step 1: fracture LHS with rhsOriginalRules of same var
    ## Step 2: fracture LHS based on same-var deps of other LHS (e.g., y[i] ~ dnorm(z[i], 1); z[j] ~ dnorm(0,1))
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

    while(pos <= length(calcRules)) {
        varName <- calcRules[[pos]]$varName
        deps <- getDependencies(calcRules[[pos]]$getFullRange(), graphRules[[varName]])
        for(d in seq_along(deps)) {
            ## Try to fracture all remaining rules by looping over non-top rules.
            for(i in start:length(calcRules)) {
                if(!fracturedRules[i] && !deps[[d]]$isEmpty() && deps[[d]]$varName == calcRules[[i]]$varName) {
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
            }
        }
        pos <- pos + 1
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
        

setSortIDs <- function(calcRules) {
    tmp <- sapply(calcRules, function(rule)
        rule$setSortID(calcRules))
    ## Now renumber so sortID=1 is first
    mx <- max(sapply(calcRules, function(rule) rule$sortID))
    tmp <- sapply(calcRules, function(rule)
        rule$sortID <- mx - rule$sortID + 1
        )
    invisible(0)
}


##        if(!hasStochDep(calcRules[[idx]], calcRules))
##           calcRules[[idx]]$set('end')

    ## for(idx in seq_along(calcRule$children)) {
    ##     if(rules[[calcRule$children[[idx]]]]$declRule$stoch) return(TRUE)
    ##     if(hasStochDep(rules[[calcRule$children[idx]]], rules)) return(TRUE)
    ## }
