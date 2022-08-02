## Code to take the declRules and graphRules and create full set of fractured calcRules and 'excluded' rhsRules

## assume we have graphRules as list of lists, indexed by parentVar
## e.g., graphRules[['mu']], graphRules[['x']]

generateInitialCalcRules <- function(declRules) {
    calcRules <- lapply(declRules, function(rule)
        calcRuleClass$new(rule, NULL, rule$ID, rule$context, rule$constants)
        )

    ## Determine if top
    for(rule in lapply(calcRules)) {
        rule$setTop()
    }

    ## Start process with known top calcRules 
    topRules <- sapply(calcRules, function(rule) rule$is_type('top'))
    rules <- c(calcRules[topRules], calcRules[!topRules])
    
    pos <- 1  # index of fracturer
    idx <- sum(topRules) + 1 # index of rules to be fractured

    while(pos <= length(rules)) {
        varName <- rules[[pos]]$varName
        deps <- getDependencies(rule$getFullRange(), graphRules[[varName]])
        for(d in deps) {
            ## Try to fracture all remaining rules
            newRules <- list()
            for(i in idx:length(rules)) {
                if(deps$varName == rules[[i]]$varName) {
                    result <- fracture(rules[[i]], deps[d], 1)
                    ## if result is same as original rule, don't put at end
                    if(is.null(result)) {
                        rules[[i]]$set('stochParent')
                    } else {
                        newRules <- c(newRules, result)
                        rulesToRemove <- c(rulesToRemove, i)
                    }
                }
            }
            rules <- c(rules[-rulesToRemove], newRules)
        }
        pos <- pos + 1
    }
}

## will need to deal with generating unique IDs
## will need to deal with top/end/stochParent, etc.

## how will getDependencies work and interact with set(s) of graphRules?

getDependencies <- function(varRange, graphRules) {
    deps <- list()
    for(i in seq_along(graphRules)) 
        deps <- c(deps, applyGraphIndexRules(varRange, graphRules[[i]])
}


    

