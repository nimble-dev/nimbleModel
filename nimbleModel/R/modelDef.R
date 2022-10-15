modelDefClass <- R6Class(
    classname = "modelDefClass",
    portable = FALSE,
    public = list(
        modelCode = NULL,
        contexts = list(),
        constants = list(),
        declInfo = list(),
        downstreamRules = NULL,
        upstreamRules = NULL,
        calcRules = NULL,
        rhsOnlyRules = NULL,
        declRules = NULL,
        topRules = NULL,
        latentRules = NULL,
        endRules = NULL,
        varNames = NULL,
        initialize = function(modelCode = NULL, constants = list()) {
            modelCode <<- modelCode
            constants <<- constants
            initializeContexts()
        },
        processModelCode = function(code = NULL, contextID = 1, lineNumber =  0, userEnv = NULL) {
            ## uses BUGScode, sets fields: contexts, declInfo$code, declInfo$contextID.
            ## all processing of code is done by BUGSdeclClass$setup(code, contextID).
            ## all processing of contexts is done by BUGScontextClass$setup()
            recursiveCall <- lineNumber != 0
            if(is.null(code)) {
                code <- modelCode
                declInfo <<- list()
            }
            for(i in 1:length(code)) {
                if(code[[i]] == '{')
                    if(length(code[[i]])==1)
                        next  ## skip { lines
                lineNumber <- lineNumber + 1
                if(code[[i]][[1]] == '~' ||
                   code[[i]][[1]] == '<-') {  ## a BUGS declaration
                    iAns <- length(declInfo) + 1
                    modelDeclClassObject <- modelDeclClass$new()
                    if(FALSE) {
                        if(code[[i]][[1]] == '~') {
                            code[[i]] <- replaceDistributionAliases(code[[i]])
                            checkUserDefinedDistribution(code[[i]], userEnv)
                        }
                        if(code[[i]][[1]] == '<-')
                            checkForDeterministicDorR(code[[i]])
                    } else
                        message(paste0('Need to turn replaceDistributionAliases,\n ',
                                       'checkUserDefinedDistribution, and\n',
                                       'checkForDeterministicDorR back on.'))
                    
                    modelDeclClassObject$setup(code[[i]],
                                               contexts[[contextID]],
                                               constants,
                                               lineNumber)
                    declInfo[[iAns]] <<- modelDeclClassObject
                }
                if(code[[i]][[1]] == 'for') {
                    ## e.g. (for i in 1:N).  New context (for-loop info) needed
                    indexVarExpr <- code[[i]][[2]]   ## This is the `i`
                    if(length(contexts) > 0) {
                        if(as.character(indexVarExpr) %in%
                           contexts[[contextID]]$indexVarNames)
                            stop(paste0(
                                "Variable ",
                                as.character(indexVarExpr),
                                " used multiple times as for loop index in nested\n",
                                "loops.\n",
                                "If your model has macros or if-then-else blocks\n",
                                "you can inspect the processed model code by doing\n",
                                "nimbleOptions(stop_after_processing_model_code = TRUE)\n",
                                "before calling nimbleModel.\n"
                            ),
                            call. = FALSE)
                    }
                    indexRangeExpr <- code[[i]][[3]] ## This is the `1:N`
                    if(nimbleModelOptions()$prioritizeColonLikeBUGS)
                        indexRangeExpr <- reprioritizeColonOperator(indexRangeExpr)
                    
                    nextContextID <- length(contexts) + 1
                    forCode <- code[[i]][1:3]        ## This is the (for i in 1:N) without the code block
                    forCode[[3]] <- indexRangeExpr
                    singleContexts <- c(
                        if(contextID == 1) NULL
                        else contexts[[contextID]]$singleContexts,
                        list(modelSingleContext(
                            indexVarExpr = indexVarExpr,       ## Add the new context
                            indexRangeExpr = indexRangeExpr,
                            forCode = forCode)
                            )
                    )
                    modelContextClassObject <- modelContextClass$new()
                    modelContextClassObject$setup(singleContexts = singleContexts)
                    contexts[[nextContextID]] <<- modelContextClassObject
                    if(length(code[[i]][[4]])==1) {
                        stop(paste0('Error, not sure what to do with ',
                                    deparse(code[[i]])))
                    }
                    recurseCode <- if(code[[i]][[4]][[1]] == '{') {
                                       code[[i]][[4]]
                                   } else {
                                       substitute( {ONELINE},
                                                  list(ONELINE = code[[i]][[4]]))
                                   }
                    ## Recursive call to process the contents of the for loop
                    lineNumber <-
                        processModelCode(
                            recurseCode,
                            nextContextID,
                            lineNumber = lineNumber,
                            userEnv = userEnv)
                }
                if(code[[i]][[1]] == '{') {
                    ## recursive call to a block contained in a {},
                    ## perhaps as a result of processCodeIfThenElse
                    lineNumber <-
                        processModelCode(
                            code[[i]],
                            contextID,
                            lineNumber = lineNumber,
                            userEnv = userEnv)
                }
                if(!deparse(code[[i]][[1]]) %in% c('~', '<-', 'for', '{')) 
                    stop("Error: ",
                         deparse(code[[i]][[1]]),
                         " not allowed in BUGS code in ",
                         deparse(code[[i]]))
            }
            invisible(lineNumber)
        },
        processDecls = function() {
            ## placeholder so we don't need to invoke all our distribution stuff
            nimFunNames <- list(as.name(':'), as.name('dmnorm'), as.name('dnorm'), as.name('dunif'), as.name('dwish'))
            ## placeholder until we add in constants processing
            for(i in seq_along(declInfo)) {
                declInfo[[i]]$process(constants, nimFunNames)
            }
            invisible(0)
        },
        generateGraphInfo = function() {
            declRules <<- lapply(declInfo, function(x) x$declRule)
            varNames <<- unique(lapply(declRules, function(rule) rule$varName))
            rhsOriginalRules <- unlist(lapply(declInfo, function(x) x$rhsOriginalRules))
            rhsOnlyRules <<- createNestedList(
                generateRHSonlyRules(rhsOriginalRules, declRules))

            allDownstreamRules <- unlist(lapply(declInfo, function(x) x$downstreamRules))
            varNames <- sapply(allDownstreamRules, function(rule)
                rule$parentVar)
            downstreamRules <<- createNestedList(allDownstreamRules, varNames)
            
            allUpstreamRules <- unlist(lapply(declInfo, function(x) x$upstreamRules))
            varNames <- sapply(allUpstreamRules, function(rule)
                rule$parentVar)   ## nomenclature confusing - parent in an upstream rule is child in graph
            upstreamRules <<- createNestedList(allUpstreamRules, varNames)

            allCalcRules <- generateCalcRules(declRules, rhsOriginalRules, downstreamRules)
            setSortIDs(allCalcRules)  ## do before top/end to catch cycles
            setEndNodes(allCalcRules)
            setTopNodes(allCalcRules)
            ## setLatentNodes(calcRules)

            ## Set up nested lists indexed by varName
            topRules <<- createNestedList(allCalcRules, type = 'top')
            endRules <<- createNestedList(allCalcRules, type = 'end')
            latentRules <<- createNestedList(allCalcRules, type = 'latent')
            calcRules <<- createNestedList(allCalcRules)
            declRules <<- createNestedList(declRules)
                        
            invisible(0)
        },
        initializeContexts = function() {
            contextClassObject <- modelContextClass$new()
            contextClassObject$setup(singleContexts = list())
            contexts[[1]] <<- contextClassObject
            invisible(0)
        }
    )
)

## TODO: rename to account for the fact that this creates varRules not lists?
createNestedList <- function(items, varNames = NULL, type = NULL) {
    if(is.null(varNames)) {
        varNames <- sapply(items, function(item) item$varName)
    } else 
        if(length(varNames) != length(items))
            stop("createNestedList: length of `varNames` must match length of `items`.")
    if(!is.null(type)) {
        if(is(items[[1]], 'varRangeClass')) stop("createNestedList: `type` restriction cannot be applied to `varRange`s.")
        include <- sapply(items, function(item) item$is_type(type))
    } else include <- rep(TRUE, length(varNames))
    uniqVarNames <- unique(varNames)
    if(is(items[[1]], 'varRangeClass')) {
        result <- lapply(uniqVarNames, function(nm)
            items[varNames == nm & include])
    } else  # some sort of rule
        result <- lapply(uniqVarNames, function(nm)
            varRuleClass$new(items[varNames == nm & include]))
    names(result) <- uniqVarNames
    return(result)
}


## Graph and node querying -- standalone functions for now, but presumably will become
## part of model class. That said, more naturally part of modelDef class.

## TODO: look into combining results - duplication only deals with complete overlap, e.g., from y[i]~dnorm(mu,sigma)
## getDependencies(c('mu','sigma'))
## TODO: data-related flags not yet dealt with. Perhaps not done here as that relates to nodes and not varRanges?
## NOTE: formerly had `includeRHSonly` but that relates to nodes and not varRanges.

## graph traversal functions cannot handle stochOnly or determOnly because a given varRange result
## for getParents could be partially stoch and partially determ
## instead a user would pass the result through getNodes().

getDependencies <- function(modelDef, nodes,
                            self = TRUE,
                            includeData = TRUE, dataOnly = FALSE, 
                            downstream = FALSE, immediateOnly = FALSE) {
    traverseGraph(modelDef$downstreamRules, modelDef$declRules, nodes = nodes,
              down = TRUE, self = self, 
              includeData = includeData, dataOnly = dataOnly, 
              follow = downstream, immediateOnly = immediateOnly)

}

getParents <- function(modelDef, nodes,
                            self = FALSE,
                            includeData = TRUE, dataOnly = FALSE, 
                            upstream = FALSE, immediateOnly = FALSE) {
    traverseGraph(modelDef$upstreamRules, modelDef$declRules, nodes = nodes,
              down = FALSE, self = self, 
              includeData = includeData, dataOnly = dataOnly, 
              follow = upstream, immediateOnly = immediateOnly)
}


traverseGraph <- function(streamRules, declRules,
                          nodes, down, self = TRUE,
                            includeData = TRUE, dataOnly = FALSE, 
                            follow = FALSE, immediateOnly = FALSE) {
                          
    if(is(nodes, 'varRangeClass')) nodes <- list(nodes)  # we use lapply on 'nodes' later
    if(!all(is.character(nodes) | sapply(nodes, function(node) is(node, 'varRangeClass'))))
        stop("getNodes: `nodes` must be variable names or variable ranges.")

    results <- traverseGraphRecurse(streamRules, nodes, down, follow, immediateOnly)

    if(self) {
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
                                                             if(is.null(tmp)) NULL else tmp$getVarRange()
                                                         })
                                                  }))
        if(identical(selfRangeFromCharRanges, list(NULL)))
            selfRangeFromCharRanges <- NULL
        results <- c(nodes[!vars & !charRanges], selfRangeFromVars, selfRangeFromCharRanges, results)
    }
    
    ## if(stochOnly)
    ##     results <- results[sapply(results, function(varRange) varRange$stoch)]
    ## if(determOnly)
    ##     results <- results[!sapply(results, function(varRange) varRange$stoch)]
    if(!length(results))
        return(NULL)
    return(removeDuplicates(results))
}

traverseGraphRecurse <- function(rules, nodes, down, follow = FALSE, immediateOnly = FALSE, first = TRUE) {
    results <- flatten(lapply(nodes, function(node) traverseGraphOne(rules, node)))
    if(immediateOnly)
        return(results)
    if(!down && !first && !follow) {
        ## stoch/determ needs to be determined from rule in which the range is on LHS
        stoch <- sapply(results, function(varRange) varRange$fromStochRule)
        results <- results[!stoch]
    }    
    propagators <- results
    if(!follow && down) {
        ## can only be used for getDeps because type of LHS not relevant for upward traversal
        stoch <- sapply(propagators, function(varRange) varRange$fromStochRule)
        propagators <- propagators[!stoch]
    }
    if(length(propagators)) {
        results <- c(results, traverseGraphRecurse(rules, propagators, down, follow, first = FALSE))
    } else {
        return(results)
    }
}
        

traverseGraphOne <- function(rules, node) {
    varName <- getVarName(node)
    if(varName %in% names(rules)) {
        return(rules[[varName]]$apply(node))
    } else return(NULL)
}



## Incorporates functionality formerly in `getNodeNames` and `expandNodeNames`
getNodes <- function(modelDef, nodes = NULL,
                     stochOnly = FALSE, determOnly = FALSE,
                     includeData = TRUE, dataOnly = FALSE,
                     includeRHSonly = FALSE, topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE) {
    ## `nodes` may include varRanges or varNames.
    
    if(topOnly + latentOnly + endOnly > 1)
        stop("getNodes: only one of `topOnly`, `latentOnly`, `endOnly` can be `TRUE`.")

    if(is.null(nodes)) {
        nodes <- names(modelDef$declRules)
        if(includeRHSonly)
            nodes <- c(nodes, names(modelDef$rhsOnlyRules))
    } else {
        if(is(nodes, 'varRangeClass')) nodes <- list(nodes) 
        if(!all(is.character(nodes) | sapply(nodes, function(node) is(node, 'varRangeClass'))))
            stop("getNodes: `nodes` must be variable names or variable ranges.")
    }
    
    if(!topOnly && !latentOnly && !endOnly) 
        result <- lapply(nodes, function(node) getNodesOne(modelDef$declRules, node))
        
    if(topOnly) result <- lapply(nodes, function(node) getNodesOne(modelDef$topRules, node))
    if(latentOnly) result <- lapply(nodes, function(node) getNodesOne(modelDef$latentRules, node))
    if(endOnly) result <- lapply(nodes, function(node) getNodesOne(modelDef$endRules, node))

    result <- flatten(result)  ## flatten the result so don't have nested list
    if(includeRHSonly) {
        rhsResult <- lapply(nodes, function(node) getNodesOne(modelDef$rhsOnlyRules, node))
        result <- c(result, flatten(rhsResult))
    }

    if(stochOnly)
        result <- result[sapply(result, function(nodeRange) nodeRange$declRule$stoch)]
    if(determOnly)
        result <- result[!sapply(result, function(nodeRange) nodeRange$declRule$stoch)]

    if(!length(result)) return(NULL)
    
    return(removeDuplicates(result))

}

## TODO: this is identical to traverseGraphOne
getNodesOne <- function(rules, node) {
    varName <- getVarName(node)  
    if(varName %in% names(rules)) {
        return(rules[[varName]]$apply(node))
    } else return(NULL)
}

        
flatten <- function(x) {
    result <- do.call(c, x)
    names(result) <- NULL
    if(identical(result, list(NULL)))
        return(NULL)
    result <- result[!sapply(result, is.null)]
    return(result)
}

removeDuplicates <- function(varRanges) {
    varRanges <- createNestedList(varRanges)
    flatten(lapply(varRanges, function(vr) removeDuplicatesOne(vr)))
}

removeDuplicatesOne <- function(varRanges) {
    mx <- length(varRanges)
    if(mx == 1) return(varRanges)
    
    varRangeIDs <- seq_len(mx)
    dups <- rep(FALSE, mx)
    for(id in 1:(mx-1)) {
        equal <- sapply((id+1):mx, function(id2)
            varRange_isEqual(varRanges[[id]], varRanges[[id2]]))
        dups[(id+1):mx] <- dups[(id+1):mx] | equal
    }
    return(varRanges[!dups])
}
