## A class representing a model definition; i.e., the model declarations
## and graph structure of a model.

## Contains sets of graphRules (upstream/parent and downstream/child) and sets of various
## kinds of nodeRules.
## {top,end,latent}Rules are just lists of pointers/shallow copies of calcRules.

modelDefClass <- R6Class(
    classname = "modelDefClass",
    portable = FALSE,
    public = list(
        modelCode = NULL,
        contexts = list(),
        constants = NULL,  # an environment (formerly `constantsEnv`)
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
        
        initialize = function(code = NULL, constants = list(), userEnv = parent.frame()) {
            ## Create environment of constants and check for unused constants.
            assignConstants(constants)
            ## Process if-then-else. Note that need input `constants` list as `self$constants` has wrong enclosing env't.
            modelCode <<- codeProcessIfThenElse(code, constants, userEnv)  
            modelCode <<- nimble:::nf_changeNimKeywords(modelCode)   ## was in assignBUGScode()
            initializeContexts()
        },

        ## Set up environment of constants; needed as we do various `eval`s that make use of `constants`.
        assignConstants = function(constants) {
            if(!is.list(constants) || (length(constants) && is.null(names(constants))))
                stop('modelDefClass$assignConstants: `constants` must be a named list.')
            if(length(names(constants))) {
                  constantsInCode <- names(constants) %in% all.vars(code)
                if(!all(constantsInCode)) 
                    for(constName in names(constants)[!constantsInCode])
                        messageIfVerbose("  [Note] '", constName,
                                         "' is provided in `constants` but not used in the model code and is being ignored.") 
            }
            constants <<- list2env(constants, parent = getDefaultNamespace())
        },
    
        ## Process raw code to determine declarations and contexts.
        processModelCode = function(code = NULL, contextID = 1, lineNumber = 0, userEnv = NULL) {
            recursiveCall <- lineNumber != 0
            if(is.null(code)) {
                code <- modelCode
                declInfo <<- list()
            }
            for(i in seq_along(code)) {
                if(code[[i]] == '{')
                    if(length(code[[i]]) == 1)
                        next  ## skip { lines
                lineNumber <- lineNumber + 1
                if(code[[i]][[1]] == '~' || code[[i]][[1]] == '<-') {  ## a declaration
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
                    ## e.g. (for i in 1:N).  New context (for-loop info) needed.
                    indexVarExpr <- code[[i]][[2]]   ## This is the `i`.
                    if(length(contexts) > 0) {
                        if(as.character(indexVarExpr) %in% contexts[[contextID]]$indexVarNames)
                            stop("modelDefClass$processModelCode: variable `",
                                as.character(indexVarExpr),
                                "` used multiple times as for loop index in nested loops.",
                                "If your model has macros or if-then-else blocks,",
                                "you can inspect the processed model code by running ",
                                "`nimbleOptions(stop_after_processing_model_code = TRUE)`",
                                "before calling nimbleModel.",
                            call. = FALSE)
                    }
                    indexRangeExpr <- code[[i]][[3]] ## This is the `1:N`.
                    if(nimbleModelOptions()$prioritizeColonLikeBUGS)
                        indexRangeExpr <- reprioritizeColonOperator(indexRangeExpr)
                    
                    nextContextID <- length(contexts) + 1
                    forCode <- code[[i]][1:3]     ## This is the `(for i in 1:N)` without the code block.
                    forCode[[3]] <- indexRangeExpr
                    ## Add the new context.
                    singleContexts <- c(
                        if(contextID == 1) NULL else contexts[[contextID]]$singleContexts,
                        list(singleContextClass$new(
                            indexVarExpr = indexVarExpr,       
                            indexRangeExpr = indexRangeExpr,
                            forCode = forCode)
                            )
                    )
                    contexts[[nextContextID]] <<- modelContextClass$new(singleContexts = singleContexts)
                    if(length(code[[i]][[4]]) == 1) {
                        stop("modelDefClass$processModelCode: cannot evaluate `", deparse(code[[i]]))
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
                    ## Recursive call to a block contained in a `{}`,
                    ## perhaps as a result of `processCodeIfThenElse`.
                    lineNumber <-
                        processModelCode(
                            code[[i]],
                            contextID,
                            lineNumber = lineNumber,
                            userEnv = userEnv)
                }
                if(!deparse(code[[i]][[1]]) %in% c('~', '<-', 'for', '{')) 
                    stop("modelDefClass$processModelCode: `",
                         deparse(code[[i]][[1]]),
                         " not allowed in model code in `",
                         deparse(code[[i]]), "`.")
            }
            invisible(lineNumber)
        },

        ## Create declaration rule and declaration-specific graphRules and various kinds of node rules
        ## for each declaration.
        processDecls = function() {
            ## Placeholder so we don't need to invoke all our distribution stuff
            nimFunNames <- list(as.name(':'), as.name('dmnorm'), as.name('dnorm'), as.name('dunif'), as.name('dwish'))
            ## Placeholder until we add in constants processing
            for(i in seq_along(declInfo)) {
                declInfo[[i]]$makeRules(nimFunNames)
            }
            invisible(NULL)
        },

        ## Create calcRules and full sets of declRules and graphRules based on all declarations.
        makeGraphInfo = function() {
            declRules <<- lapply(declInfo, function(x) x$declRule)
            varNames <<- unique(lapply(declRules, function(rule) rule$varName))
            
            rhsOriginalRules <- unlist(lapply(declInfo, function(x) x$rhsOriginalRules))
            rhsOnlyRules <<- newVarRules(makeRHSonlyRules(rhsOriginalRules, declRules))

            allDownstreamRules <- unlist(lapply(declInfo, function(x) x$downstreamRules))
            fromVarNames <- sapply(allDownstreamRules, function(rule) rule$fromVarName)
            downstreamRules <<- newVarRules(allDownstreamRules, fromVarNames)
            
            allUpstreamRules <- unlist(lapply(declInfo, function(x) x$upstreamRules))
            fromVarNames <- sapply(allUpstreamRules, function(rule) rule$fromVarName)   
            upstreamRules <<- newVarRules(allUpstreamRules, fromVarNames)

            ## Check for cycles. Need to use `initialCalcRules` rather than `declRules`
            ## as `declRules` don't have `sortID`.

            initialCalcRules <- lapply(declRules, function(rule)
                calcRuleClass$new(rule, NULL, NULL, rule$context, rule$constants)
                )
            sapply(seq_along(initialCalcRules), function(i) initialCalcRules[[i]]$ID <- as.character(i))
            names(initialCalcRules) <- sapply(initialCalcRules, function(rule) rule$ID)

            setRelationships(initialCalcRules, downstreamRules)
            sorted <- setSortIDs(initialCalcRules)  
            ## At this point, we have a potential cyclic case if `sorted` is `FALSE`.
            
            ## Do fracturing, but in potential cyclic case, do not fracture already-fractured nodes
            ## to avoid very slow one-by-one carving off calcRules in state-space cases.

            ## Start from scratch with clean set of `initialCalcRules`
            ## (empty `sortID`, `parents`, `children` slots).
            initialCalcRules <- lapply(declRules, function(rule)
                calcRuleClass$new(rule, NULL, NULL, rule$context, rule$constants)
                )
            sapply(seq_along(initialCalcRules), function(i) initialCalcRules[[i]]$ID <- as.character(i))
            names(initialCalcRules) <- sapply(initialCalcRules, function(rule) rule$ID)

            allCalcRules <- makeCalcRules(initialCalcRules, rhsOriginalRules, downstreamRules,
                                              recurseFracturing = sorted)
            sorted <- setSortIDs(allCalcRules)
            
            if(!sorted) {  ## SSM case
                ## Handle standard SSM case of lag +1 or -1, with one or more calcRules in the cycle.

                ## This inserts vectors of sortIDs for the calcRules in the cycle.
                allCalcRules <- processCyclicRules(allCalcRules, self)
                ## Now assign remaining sortIDs (i.e., to various parent calcRules that formerly had `Inf` as `sortID`).
                sorted <- setSortIDs(allCalcRules)

                if(!sorted) {  # Complicated SSM-type cases or true cycles.
                    ## Fully fracture to try to handle complicated SSM cases.
                    warning("Detected state-space type structure or cycle in model graph. Attempting to determine graph structure for non-cyclic cases. This may take some time. You may wish to alert the NIMBLE development team of your use case so that handling of such cases can be improved.")
                    allCalcRules <- makeCalcRules(initialCalcRules, rhsOriginalRules, downstreamRules,
                                                      recurseFracturing = TRUE)
                    sorted <- setSortIDs(allCalcRules)
                    if(!sorted)
                        stop("Cycle found in model graph. NIMBLE does not allow cyclic models.")
                }
            }

            ## Set `top` and `end` flags in each calcRule.
            setEndRules(allCalcRules)
            setTopRules(allCalcRules)

            ## Set up nested lists indexed by varName
            topRules <<- newVarRules(allCalcRules, type = 'top')
            endRules <<- newVarRules(allCalcRules, type = 'end')
            latentRules <<- newVarRules(allCalcRules, type = 'latent')
            calcRules <<- newVarRules(allCalcRules)
            declRules <<- newVarRules(declRules)
                        
            invisible(NULL)
        },
        
        initializeContexts = function() {
            contextClassObject <- modelContextClass$new()
            contexts[[1]] <<- contextClassObject
            invisible(NULL)
        }
    )
)


## Core graph and node querying functions in the model API.
## These are standalone functions for now, but may become
## part of model class. That said, more naturally part of modelDef class.

## TODO: look into combining results - duplication only deals with complete overlap, e.g., from `y[i]~dnorm(mu,sigma)`
## getDependencies(c('mu','sigma'))

## Note: `getDependencies` and `getParents` cannot handle `stochOnly` or `determOnly`
## because a given varRange result for getParents could be partially stochastic and
## partially deterministic. Instead a user would pass the result through `getNodes()`.
## Similarly, filtering by RHSonly will be done in `getNodes()`.

## Note: data-related flags not handled as that relates to flags on a model
## and not part of modelDef.

getDependencies <- function(modelDef, nodes,
                            self = TRUE,
                            downstream = FALSE, immediateOnly = FALSE) {
    traverseGraph(modelDef$downstreamRules, modelDef$declRules, nodes = nodes,
                  down = TRUE, self = self, 
                  follow = downstream, immediateOnly = immediateOnly)
    
}

getParents <- function(modelDef, nodes,
                            self = FALSE,
                            upstream = FALSE, immediateOnly = FALSE) {
    traverseGraph(modelDef$upstreamRules, modelDef$declRules, nodes = nodes,
                  down = FALSE, self = self, 
                  follow = upstream, immediateOnly = immediateOnly)
}


## Determine nodes of interest, potentially of particular types.
## Incorporates functionality formerly in `getNodeNames` and `expandNodeNames`
## TODO: data-related flags not handled here and presumably can't be handled
## here as the data are property of model, not of graph.
getNodes <- function(modelDef, nodes = NULL,
                     stochOnly = FALSE, determOnly = FALSE,
                     includeData = TRUE, dataOnly = FALSE,
                     includeRHSonly = FALSE,
                     topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE) {
    ## `nodes` may contain one or more varRanges or varNames.
    
    if(topOnly + latentOnly + endOnly > 1)
        stop("getNodes: only one of `topOnly`, `latentOnly`, `endOnly` can be `TRUE`.")

    if(is.null(nodes)) {
        nodes <- names(modelDef$declRules)
        if(includeRHSonly)
            nodes <- c(nodes, names(modelDef$rhsOnlyRules))
    } else {
        if(is(nodes, 'varRangeClass'))
            nodes <- list(nodes) 
        if(!all(is.character(nodes) | sapply(nodes, function(node) is(node, 'varRangeClass'))))
            stop("getNodes: `nodes` must be variable names or variable ranges.")
    }
    
    if(!topOnly && !latentOnly && !endOnly) 
        result <- lapply(nodes, function(node) applyRules(modelDef$declRules, node))
        
    if(topOnly) result <- lapply(nodes, function(node) applyRules(modelDef$topRules, node))
    if(latentOnly) result <- lapply(nodes, function(node) applyRules(modelDef$latentRules, node))
    if(endOnly) result <- lapply(nodes, function(node) applyRules(modelDef$endRules, node))

    result <- flatten(result)  ## Flatten the result so don't have nested list.

    if(includeRHSonly) {
        rhsResult <- lapply(nodes, function(node) applyRules(modelDef$rhsOnlyRules, node))
        result <- c(result, flatten(rhsResult))
    }

    if(stochOnly)
        result <- result[sapply(result, function(nodeRange) nodeRange$declRule$stoch)]
    if(determOnly)
        result <- result[!sapply(result, function(nodeRange) nodeRange$declRule$stoch)]

    if(!length(result)) return(NULL)
    
    return(removeDuplicateVarRanges(result))

}



## Evaluates `if` statements in model code to generate actual model code
## without any `if` statements.
codeProcessIfThenElse <- function(code, constants, envir = parent.frame()) {
    if(is.list(constants))
        constants <- list2env(constants, parent = envir)
    
    codeLength <- length(code)
    if(is.name(code))
        stop("Incomplete declaration found: '", safeDeparse(code), "'.")
        
    if(code[[1]] == '{') {
        if(codeLength > 1)
            for(i in 2:codeLength)
                code[[i]] <- codeProcessIfThenElse(code[[i]], constants, envir)
        return(code)
    }
    
    if(code[[1]] == 'for') {
        code[[4]] <- codeProcessIfThenElse(code[[4]], constants, envir)
        return(code)
    }
    
    if(code[[1]] == 'if') {
         evaluatedCondition <- try(eval(code[[2]], constants), silent = TRUE)
        if(inherits(evaluatedCondition, "try-error")) 
            stop("codeProcessIfThenElse: cannot evaluate condition of `if` statement: `",
                 safeDeparse(code[[2]]),
                 "`.\nCondition must be able to be evaluated based on values in `constants` or environment from which model is created.")
        if(evaluatedCondition) {
            return(codeProcessIfThenElse(code[[3]], constants, envir))
        } else {
            if(length(code) == 4)
                return(codeProcessIfThenElse(code[[4]], constants, envir))
            else return(quote({}))
        }
    } else return(code)
}
