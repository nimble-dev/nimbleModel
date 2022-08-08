## Code to take the declRules and graphRules and create full set of fractured calcRules and 'excluded' rhsRules

## assume we have graphRules as list of lists, indexed by parentVar
## e.g., graphRules[['mu']], graphRules[['x']]

if(FALSE) {  ## start of testing of mdelDef creation and subsequent processing

    code <- nimbleCode({
        for(i in 1:10)
            y[i] ~ dnorm(mu[i], sigma)
        for(j in 2:3)
            mu[j] ~ dnorm(mu0, 1)
        sigma ~ dunif(0, 5)
        mu[7:8] ~ dmnorm(z[1:2],pr[1:2,1:2])
        w ~ dnorm(y[10], theta)
        z[2] ~ dnorm(y[12], 1)
    })

    md <- modelDefClass$new(code)

    ## now extract declRules, rhsOriginalRules and graphRules
    ## rework graphRules to be named based on RHS

}


if(FALSE) {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 2:3){}))
    context_0 <- modelContextClass$new()
    
    context_i <- modelContextClass$new(list(singleContext1))
    context_j <- modelContextClass$new(list(singleContext2))
    
    ## code <- nimbleCode({
    ##     for(i in 1:10)
    ##         y[i] ~ dnorm(mu[i], sigma)
    ##     for(j in 2:3)
    ##         mu[j] ~ dnorm(mu0, 1)
    ##     sigma ~ dunif(0, 5)
    ##     mu[7:8] ~ dmnorm(z[1:2],pr[1:2,1:2])
    ##     w ~ dnorm(y[10], theta)
    ##     z[2] ~ dnorm(y[12], 1)
    ## })

    ## declRules should have their IDs in order of entries in list.
    declRules <- list(
        declRuleClass$new(quote(y[i] ~ dnorm(mu[i],sigma)), 1, context_i),
        declRuleClass$new(quote(mu[j] ~ dnorm(mu0,1)), 2, context_j),
        declRuleClass$new(quote(sigma ~ dunif(0,1)), 3, context_0),
        declRuleClass$new(quote(mu[7:8] ~ dmnorm(z[1:2],pr[1:2,1:2])), 4, context_0),
        declRuleClass$new(quote(w ~ dnorm(y[10],1)), 5, context_0),
        declRuleClass$new(quote(z[2] ~ dnorm(y[12],1)), 6, context_0)
    )

    ## set up test of generation of rhsOnlyRules starting with originalRHSrules 
    ## for now hand-code rhsRules


    rhsOriginalRules <- list(
        rhsRuleClass$new(quote(mu[i]), 1, context_i),
        rhsRuleClass$new(quote(sigma), 2, context_0),
        rhsRuleClass$new(quote(mu0), 3, context_0),
        rhsRuleClass$new(quote(z[1:2]), 4, context_0),
        rhsRuleClass$new(quote(pr[1:2,1:2]), 5, context_0),
        rhsRuleClass$new(quote(y[10]), 6, context_0),
        rhsRuleClass$new(quote(theta), 7, context_0),
        rhsRuleClass$new(quote(y[12]), 8, context_0))
    
    graphRules <- list()
    graphRules[['sigma']] <- list(makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(sigma),
                                 context = context_i))
    graphRules[['mu']] <- list(makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(mu[i]),
                                 context = context_i))
    graphRules[['mu0']] <- list(makeGraphIndexRules(LHS = quote(mu[j]),
                                 RHS = quote(mu0),
                                 context = context_j))
    graphRules[['z']] <- list(makeGraphIndexRules(LHS = quote(mu[7:8]),
                                 RHS = quote(z[1:2]),
                                 context = context_0))
    graphRules[['pr']] <- list(makeGraphIndexRules(LHS = quote(mu[7:8]),
                                 RHS = quote(pr[1:2,1:2]),
                                 context = context_0))
    graphRules[['y']] <- list(makeGraphIndexRules(LHS = quote(w),
                                 RHS = quote(y[10]),
                                 context = context_0))

    ## LHS-oriented -- ever useful?
    graphRules0 <- list()
    graphRules0[['y']] <- list(makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(mu[i]),
                                 context = context_i),
                         makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(sigma),
                                 context = context_i))
    graphRules0[['mu']] <- list(makeGraphIndexRules(LHS = quote(mu[j]),
                                 RHS = quote(mu0),
                                 context = context_j),
                         makeGraphIndexRules(LHS = quote(mu[7:8]),
                                 RHS = quote(z[1:2]),
                                 context = context_0),
                         makeGraphIndexRules(LHS = quote(mu[7:8]),
                                 RHS = quote(pr[1:2,1:2]),
                                 context = context_0))
    graphRules0[['w']] <- list(makeGraphIndexRules(LHS = quote(w),
                                 RHS = quote(y[10]),
                                 context = context_0,
                                 constants = list(theta = 2)))

}

if(FALSE) {  # test case for rhsOnly

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 2:3){}))
    context_0 <- modelContextClass$new()
    
    context_i <- modelContextClass$new(list(singleContext1))
    context_j <- modelContextClass$new(list(singleContext2))
    
    ## code <- nimbleCode({
    ##     for(i in 1:10)
    ##         y[i] ~ dnorm(mu[i], sigma)
    ##     for(j in 2:3)
    ##         mu[j] ~ dnorm(mu0, 1)
    ##     sigma ~ dunif(0, 5)
    ##     mu[7:8] ~ dmnorm(z[1:2],pr[1:2,1:2])
    ##     w ~ dnorm(y[10], theta)
    ##     z[2] ~ dnorm(y[12], 1)
    ## })

    ## declRules should have their IDs in order of entries in list.
    declRules <- list(
        declRuleClass$new(quote(w[i] ~ dnorm(mu[i],sigma)), 1,
                          modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:10){}))))),
        declRuleClass$new(quote(y[i] ~ dnorm(mu[i],sigma)), 2,
                          modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 10:15){}))))),
        declRuleClass$new(quote(z[i] ~ dnorm(mu[i],sigma)), 3,
                          modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:7){}))))),
        declRuleClass$new(quote(mu[j] ~ dnorm(0,1)), 4, context_j)
    )
   
    rhsOriginalRules <- list(
        rhsRuleClass$new(quote(mu[i]), 1,
                         modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:10){}))))),
        rhsRuleClass$new(quote(mu[i]), 2,
                         modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 10:15){}))))),
        rhsRuleClass$new(quote(mu[i]), 3,
                         modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:7){}))))),
        rhsRuleClass$new(quote(sigma), 4, context_0),
        rhsRuleClass$new(quote(mu0), 5, context_0)
        )

}

if(FALSE) { # for testing 'end' nodes
    context_0 <- modelContextClass$new()
    declRules <- list(
        declRuleClass$new(quote(theta ~ dnorm(0,1)), 1, context_0),
        declRuleClass$new(quote(mu <- theta), 2, context_0),
        declRuleClass$new(quote(y ~ dnorm(mu,1)), 3, context_0),
        declRuleClass$new(quote(z <- y), 4, context_0)
    )
    rhsOriginalRules <- list()

    graphRules <- list()
    graphRules[['y']] <- list(makeGraphIndexRules(LHS = quote(z),
                                                      RHS = quote(y),
                                                  context = context_0))
    graphRules[['mu']] <- list(makeGraphIndexRules(LHS = quote(y),
                                                      RHS = quote(mu),
                                                  context = context_0))
    graphRules[['theta']] <- list(makeGraphIndexRules(LHS = quote(mu),
                                                      RHS = quote(theta),
                                                  context = context_0))
    calcRules <- generateCalcRules(declRules, rhsOriginalRules, graphRules)

    ## temp while need to debug parent/children generation
    calcRules[[1]]$setChildren(2)
    calcRules[[2]]$setChildren(3)
    calcRules[[3]]$setChildren(4)
    calcRules[[2]]$setParents(1)
    calcRules[[3]]$setParents(2)
    calcRules[[4]]$setParents(3)
}
 

if(FALSE) { # for testing 'top' nodes
    context_0 <- modelContextClass$new()
    declRules <- list(
        declRuleClass$new(quote(theta <- 7), 1, context_0),
        declRuleClass$new(quote(mu <- theta), 2, context_0),
        declRuleClass$new(quote(y ~ dnorm(mu,1)), 3, context_0),
        declRuleClass$new(quote(z <- y), 4, context_0)
    )
    rhsOriginalRules <- list()

    graphRules <- list()
    graphRules[['y']] <- list(makeGraphIndexRules(LHS = quote(z),
                                                      RHS = quote(y),
                                                  context = context_0))
    graphRules[['mu']] <- list(makeGraphIndexRules(LHS = quote(y),
                                                      RHS = quote(mu),
                                                  context = context_0))
    graphRules[['theta']] <- list(makeGraphIndexRules(LHS = quote(mu),
                                                      RHS = quote(theta),
                                                  context = context_0))
    calcRules <- generateCalcRules(declRules, rhsOriginalRules, graphRules)

    ## temp while need to debug parent/children generation
    calcRules[[1]]$setChildren(2)
    calcRules[[2]]$setChildren(3)
    calcRules[[3]]$setChildren(4)
    calcRules[[2]]$setParents(1)
    calcRules[[3]]$setParents(2)
    calcRules[[4]]$setParents(3)
}

if(FALSE) { # for testing sortID
    context_0 <- modelContextClass$new()
    declRules <- list(
        declRuleClass$new(quote(theta ~ dnorm(0,1)), 1, context_0),
        declRuleClass$new(quote(mu ~ dnorm(theta, 1)), 2, context_0),
        declRuleClass$new(quote(y ~ dnorm(mu+theta, 1)), 3, context_0)
    )
    rhsOriginalRules <- list()

    graphRules <- list()
    graphRules[['mu']] <- list(makeGraphIndexRules(LHS = quote(y),
                                                      RHS = quote(mu),
                                                  context = context_0))
    graphRules[['theta']] <- list(makeGraphIndexRules(LHS = quote(mu),
                                                      RHS = quote(theta),
                                                      context = context_0),
                                  makeGraphIndexRules(LHS = quote(y),
                                                      RHS = quote(theta),
                                                      context = context_0))
    calcRules <- generateCalcRules(declRules, rhsOriginalRules, graphRules)

    ## temp while need to debug parent/children generation
    calcRules[[1]]$setChildren(2)
    calcRules[[1]]$setChildren(3)
    calcRules[[2]]$setChildren(3)
    calcRules[[2]]$setParents(1)
    calcRules[[3]]$setParents(1)
    calcRules[[3]]$setParents(2)
}
 
## split up rhsRules to get rhsOnlyRules using exclude(), to extract parts of rhs that don't appear in LHS

## Not clear we need to generate RHSonlyRules
## There may be some non-uniqueness if we don't combine the results of
## running exclude on a rhsRule applied to another rhsRule
generateRHSonlyRules <- function(rhsOriginalRules) {
    rhsOnlyRules <- rhsOriginalRules

    ## Step 1: exclude each rhsRule based on other rhsRules
    ## Otherwise exclusion process with LHSrules can create redundant rhsOnlyRules
    ## Step 2: exclude each rhsRule with each LHSrule
    
    pos <- 1
    while(pos < length(rhsOnlyRules)) {
        mx <- length(rhsOnlyRules)
        rulesToRemove <- NULL
        for(i in (pos+1):mx) {
            if(rhsOnlyRules[[i]]$varName == rhsOnlyRules[[pos]]$varName) {
                result <- exclude(rhsOnlyRules[[i]], rhsOnlyRules[[pos]])
                if(!is.null(result)) {  # some or no overlap
                    rhsOnlyRules <- c(rhsOnlyRules, result)
                    ## TODO: combine result and rhsOnlyRules[[pos]] (for sequence rules and maybe matrix rules)
                    ## if possible to reduce non-uniqueness.
                    ## But note that one can get a resulting rhsOnlyRule that is larger than a declaration RHS
                }
                rulesToRemove <- c(rulesToRemove, i)
            }
        }
        if(length(rulesToRemove))
            rhsOnlyRules <- rhsOnlyRules[-rulesToRemove]
        pos <- pos + 1
    }
    
    for(pos in seq_along(declRules)) {
        rulesToRemove <- NULL
        for(i in seq_along(rhsOnlyRules)) {
            if(rhsOnlyRules[[i]]$varName == declRules[[pos]]$varName) {
                result <- exclude(rhsOnlyRules[[i]], declRules[[pos]])
                if(!is.null(result)) {
                    rhsOnlyRules <- c(rhsOnlyRules, result)
                    rulesToRemove <- c(rulesToRemove, i)
                }
            }
        }
        if(length(rulesToRemove))
            rhsOnlyRules <- rhsOnlyRules[-rulesToRemove]
    }
    tmp <- sapply(seq_along(rhsOnlyRules), function(idx) rhsOnlyRules[[idx]]$ID <- idx)
}

getDependencies <- function(varRange, graphRules) {
    lapply(graphRules, function(rule)
        applyGraphIndexRules(varRange, rule))
}

generateCalcRules <- function(declRules, rhsOriginalRules, graphRules) {
    ## Step 1: fracture LHS with rhsOriginalRules of same var
    ## Step 2: fracture LHS based on same-var deps of other LHS
    numRHSrules <- length(rhsOriginalRules)
    
    originalCalcRules <- lapply(declRules, function(rule)
        calcRuleClass$new(rule, NULL, NULL, rule$context, rule$constants)
        )

    ## Determine if no RHS vars as clear top nodes.
    for(rule in originalCalcRules) 
        rule$setObviousTop()

    ## Start process with known top calcRules 
    topRules <- sapply(originalCalcRules, function(rule) rule$is_type('top'))
    calcRules <- c(originalCalcRules[topRules], originalCalcRules[!topRules])

    tmp <- sapply(seq_along(calcRules), function(i) calcRules[[i]]$ID <- i)
    
    ## fracture LHS of same varName as rhsRule
    pos <- 1
    start <- sum(topRules) + 1 # index of rules to be fractured

    fracturedRules <- rep(FALSE, length(calcRules))
    while(pos <= length(rhsOriginalRules)) {   # use while rather than for to match needed while in loop over calcRules
        rhsRange <- rhsOriginalRules[[pos]]$getFullRange()
        if(!rhsRange$isNone()) {
            ## Try to fracture all rules by looping over non-top rules.
            for(i in start:length(calcRules)) {
                if(rhsRange$varName == calcRules[[i]]$varName) {
                    result <- fracture(calcRules[[i]], rhsRange, currentID = length(calcRules),
                                       parentRule = rhsOriginalRules[[pos]], currentRules = calcRules)
                    
                    ## RHS doesn't overlap with LHS
                    ## Could probably handle this compared to full overlap more elegantly.
                    if(!is.null(result) && !is.list(result) && result$isEmpty())
                        next
                    
                    ## if result is same as original rule, don't put at end
                    if(!is.null(result)) {
                        calcRules <- c(calcRules, result)
                        fracturedRules[i] <- TRUE
                        fracturedRules <- c(fracturedRules, rep(FALSE, length(result)))
                    }
                }
            }
        }
        pos <- pos + 1
    }
    
    pos <- 1  # index of fracturer

    while(pos <= length(calcRules)) {
        varName <- calcRules[[pos]]$varName
        deps <- getDependencies(calcRules[[pos]]$getFullRange(), graphRules[[varName]])
        for(d in seq_along(deps)) {
            ## Try to fracture all remaining rules by looping over non-top rules.
            newRules <- list()
            rulesToRemove <- NULL
            for(i in start:length(calcRules)) {
                if(!deps[[d]]$isNone() && deps[[d]]$varName == calcRules[[i]]$varName) {
                    result <- fracture(calcRules[[i]], deps[[d]], currentID = length(calcRules),
                                       parentRule = calcRules[[pos]], currentRules = calcRules)
                    if(!is.null(result) && !is.list(result) && result$isEmpty())
                        next
                    if(!is.null(result)) {
                        calcRules <- c(calcRules, result)
                        fracturedRules[i] <- TRUE
                        fracturedRules <- c(fracturedRules, rep(FALSE, length(result)))
                    }
                }
            }
        }
        pos <- pos + 1
    }
    calcRules <- calcRules[!fracturedRules]
}

## how will getDependencies work and interact with set(s) of graphRules?

setEndNodes <- function(calcRules) {
    tmp <- sapply(calcRules, function(rule)
        rule$setStochDep(calcRules))
    tmp <- sapply(calcRules, function(rule)
        if(rule$stochDep) rule$unset('end') else rule$set('end'))
}

setTopNodes <- function(calcRules) {
    tmp <- sapply(calcRules, function(rule)
        rule$setStochParent(calcRules))
    tmp <- sapply(calcRules, function(rule)
        if(rule$stochParent) rule$unset('top') else rule$set('top'))
}

setLatentNodes <- function(calcRules) {
    tmp <- sapply(calcRules, function(rule)
        if(rule$is('end') || rule$is('top')) rule$unset('latent'))
}
        

setSortIDs <- function(calcRules) {
    tmp <- sapply(calcRules, function(rule)
        rule$setSortID(calcRules))
    ## Now renumber so sortID=1 is first
    mx <- max(sapply(calcRules, function(rule) rule$sortID))
    tmp <- sapply(calcRules, function(rule)
        rule$sortID <- mx - rule$sortID + 1
        )
}


##        if(!hasStochDep(calcRules[[idx]], calcRules))
##           calcRules[[idx]]$set('end')

    ## for(idx in seq_along(calcRule$children)) {
    ##     if(rules[[calcRule$children[[idx]]]]$declRule$stoch) return(TRUE)
    ##     if(hasStochDep(rules[[calcRule$children[idx]]], rules)) return(TRUE)
    ## }
