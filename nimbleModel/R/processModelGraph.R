## Code to take the declRules and graphRules and create full set of fractured calcRules and 'excluded' rhsRules

## assume we have graphRules as list of lists, indexed by parentVar
## e.g., graphRules[['mu']], graphRules[['x']]

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
    ##     z ~ dnorm(y[12], 1)
    ## })
    
    declRules <- list(
        declRuleClass$new(quote(y[i] ~ dnorm(mu[i],sigma)), 9, context_i),
        declRuleClass$new(quote(mu[j] ~ dnorm(mu0,1)), 10, context_j),
        declRuleClass$new(quote(sigma ~ dunif(0,1)), 11, context_0),
        declRuleClass$new(quote(mu[7:8] ~ dmnorm(z[1:2],pr[1:2,1:2])), 12, context_0),
        declRuleClass$new(quote(w ~ dnorm(y[10],1)), 13, context_0),
        declRuleClass$new(quote(z ~ dnorm(y[12],1)), 14, context_0)
    )

    ## need rhsRules; should these be before or after exclude()?

    rhsRules <- list(
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

    irNone <- nimbleModel:::indexRange_none()
    vrNone <- varRangeClass$new(list(irNone))

    gr <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x),
                                 context = context_i)
    tmp=applyGraphIndexRules(vrNone,gr)
}


generateInitialCalcRules <- function(declRules) {
    currentID <- length(rhsRules) + length(declRules)
    
    originalCalcRules <- lapply(declRules, function(rule)
        calcRuleClass$new(rule, NULL, rule$ID, rule$context, rule$constants)
        )

    ## Determine if top
    for(rule in originalCalcRules) 
        rule$setTop()

    ## Start process with known top calcRules 
    topRules <- sapply(originalCalcRules, function(rule) rule$is_type('top'))
    calcRules <- c(originalCalcRules[topRules], originalCalcRules[!topRules])

    ## fracture LHS of same varName as rhsRule
    pos <- 1
    while(pos <= length(rhsRules)) {
        rhsRange <- rhsRules[[pos]]$getFullRange()
        for(i in idx:length(calcRules)) {
            if(rhsRange$varName == calcRules[[i]]$varName) {
                result <- fracture(calcRules[[i]], rhsRange, currentID = currentID,
                                   stochParent = FALSE, parentID = rhsRules[[pos]]$ID)

                ## need to check if RHS doesn't overlap with LHS
                
                ## if result is same as original rule, don't put at end
                if(is.null(result)) {
                    rhsRules[[pos]]$setChild(calcRules[[i]]$ID)
                    calcRules[[i]]$setParent(rhsRules[[pos]]$ID)
                } else {
                    ## first of the newRules will be the fracturingRule, which is the child
                    ## This is awkward as relies on assumption about how fracture() works internally
                    rhsRules[[pos]]$setChild(length(calcRules)+1)
                    newRules <- c(newRules, result)
                    rulesToRemove <- c(rulesToRemove, i)
                    currentID <- result[[length(result)]]$ID
                }
            }
        }
        if(!is.null(rulesToRemove))
            calcRules <- c(calcRules[-rulesToRemove], newRules)
        pos <- pos + 1
    }
    
    pos <- 1  # index of fracturer
    idx <- sum(topRules) + 1 # index of rules to be fractured

    while(pos <= length(calcRules)) {
        varName <- calcRules[[pos]]$varName
        deps <- getDependencies(calcRules[[pos]]$getFullRange(), graphRules[[varName]])
        stochParent <- calcRules[[pos]]$declRule$stoch || calcRules[[pos]]$stochParent
        for(d in seq_along(deps)) {
            ## Try to fracture all remaining rules
            newRules <- list()
            rulesToRemove <- NULL
            for(i in idx:length(calcRules)) {
                if(deps[[d]]$varName == calcRules[[i]]$varName) {
                    result <- fracture(calcRules[[i]], deps[[d]], currentID = length(calcRules),
                                       stochParent, parentID = calcRules[[pos]]$ID)
                    ## if result is same as original rule, don't put at end
                    if(is.null(result)) {
                        if(stochParent)
                            calcRules[[i]]$set('stochParent')
                        calcRules[[pos]]$setChild(calcRules[[i]]$ID)
                        calcRules[[i]]$setParent(calcRules[[pos]]$ID)
                    } else {
                        ## first of the newRules will be the fracturingRule, which is the child
                        ## This is awkward as relies on assumption about how fracture() works internally
                        calcRules[[pos]]$setChild(length(calcRules)+1)
                        newRules <- c(newRules, result)
                        rulesToRemove <- c(rulesToRemove, i)
                    }
                }
            }
            if(!is.null(rulesToRemove))
                calcRules <- c(calcRules[-rulesToRemove], newRules)
        }
        pos <- pos + 1
    }
}

## will need to deal with generating unique IDs
## will need to deal with top/end/stochParent, etc.

## how will getDependencies work and interact with set(s) of graphRules?

getDependencies <- function(varRange, graphRules) {
    lapply(graphRules, function(rule)
        applyGraphIndexRules(varRange, rule))
}


    
