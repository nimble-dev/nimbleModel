## A class representing the model, including the model definition, containing
## static information, data information, and methods for querying the model
## structure.

## Will need to do some work to extend this to get full current behavior
## where the model is created by `nimbleModel` and the custom model class
## contains fields for the different variables.

modelClass <- R6Class(
    classname = "modelClass",
    portable = FALSE,
    public = list(
        name = '',
        modelDef = NULL,
        dataRules = NULL,
        nondataRules = NULL,
        predictiveRules = NULL,
        nonpredictiveRules = NULL,

        defaultModelValues = NULL,
        origData = list(),
        origInits = list(),
        nimbleProject = NULL,
        
        initialize = function(code = NULL, name = NULL, constants = list(), dimensions = list(),
                              inits = list(), data = list(), userEnv = parent.frame()) {
            name <<- name
            if(length(constants) && sum(names(constants) == ""))
                stop("modelClass: 'constants' must be a named list")
            if(length(dimensions) && sum(names(dimensions) == ""))
                stop("modelClass: 'dimensions' must be a named list")
            if(length(inits) > 0 && is.list(inits[[1]])) {
                messageIfVerbose('  [Note] Detected JAGS-style initial values, provided as a list of lists. Using the first set of initial values')
                inits <- inits[[1]]
            }
            if(length(inits)) {
                unnamed <- which(names(inits) == "")
                if(length(unnamed) || is.null(names(inits))) {
                    warning("One or more unnamed elements found in inits.")
                    if(length(unnamed))
                        inits <- inits[-unnamed] else inits <- list()
                }
            }


            if(length(data) && sum(names(data) == ""))
                stop("modelClass: 'data' must be a named list")
            if(any(!sapply(data, function(x) {
                is.numeric(x) || is.logical(x) ||
                    (is.data.frame(x) && all(sapply(x, 'is.numeric'))) })))
                stop("modelClass: elements of 'data' must be numeric")
            
            modelDef <<- modelDefClass$new(code, constants = constants,
                                           dimensions = dimensions, inits = inits,
                                           data = data, userEnv = userEnv)


            dataVarIndices <- names(constants) %in% modelDef$varNames & !names(constants) %in% names(data)  # don't overwrite anything in 'data'
            if(sum(names(constants) %in% names(data)))
                messageIfVerbose("  [Note] Found the same variable(s) in both 'data' and 'constants'; using variable(s) from 'data'.\n")
            if(sum(dataVarIndices)) {
                data <- c(data, constants[dataVarIndices])
                messageIfVerbose("  [Note] Adding '", paste(names(constants)[dataVarIndices], collapse = ', '), "' as data for building model.")
            }

            makeDataRules(data)
            makePredictiveRules()

            ## Do this once we have a custom model class with fields we can assign into.
            ## setData(data)
            ## setInits(inits)
            
        },

        makeDataRules = function(data) {
            nms <- names(data)
            dataRules <<- lapply(seq_along(nms), function(i)
                varRulesClass$new(newDataRules(data[[i]], nms[i]), varName = nms[i]))
            dataRules <<- dataRules[!sapply(dataRules, function(oneVarRules) is.null(oneVarRules$rules))]
            names(dataRules) <<- sapply(dataRules, `[[`, 'varName')
            nondataRules <<- lapply(seq_along(nms), function(i)
                varRulesClass$new(newDataRules(data[[i]], nms[i], nondata = TRUE), varName = nms[i]))
            nondataRules <<- nondataRules[!sapply(nondataRules, function(oneVarRules) is.null(oneVarRules$rules))]
            names(nondataRules) <<- sapply(nondataRules, `[[`, 'varName')
        },


        ## Start from ranges based on dataRules and walk upwards,
        ## excluding any parents of such ranges.
        ## For now, this doesn't use any notion of "touched", so there is duplicative
        ## walking up the tree for nodes with multiple data descendants, but given
        ## graph processing is declaration-based, this doesn't seem like an efficiency
        ## concern, particularly since `candidateRules` will progressively shrink.
        makePredictiveRules = function() {
            ## predictive rules
            candidateRules <- unlist(lapply(modelDef$calcRules, function(oneVarRules) {
                stoch <- sapply(oneVarRules$rules, function(rule) rule$declRule$stoch)
                return(oneVarRules$rules[stoch])
            })) # `unlist` removes length-0 entries.
            candidateRules <- newVarRules(candidateRules)
            
            dataRanges <- unlist(lapply(dataRules, function(oneVarDataRules)
                lapply(oneVarDataRules$rules, function(dataRule)
                    dataRule$rule$apply(dataRule$varName))))
            predictiveRules <<- excludeFromPredictiveRules(modelDef, dataRanges, candidateRules)

            ## nonpredictive rules
            candidateRules <- unlist(lapply(modelDef$calcRules, function(oneVarRules) {
                stoch <- sapply(oneVarRules$rules, function(rule) rule$declRule$stoch)
                return(oneVarRules$rules[stoch])
            })) # `unlist` removes length-0 entries.
            candidateRules <- newVarRules(candidateRules)

            for(oneVarPredictiveRules in predictiveRules)
                for(predictiveRule in oneVarPredictiveRules$rules) {
                    predictiveRange <- predictiveRule$fullRange
                    varName <- predictiveRule$varName
                    tmp <- unlist(lapply(candidateRules[[varName]]$rules, exclude, predictiveRange))
                    tmp <- tmp[!sapply(tmp, is.null)]
                    if(length(tmp)) {
                        candidateRules[[varName]] <- varRulesClass$new(tmp, varName)
                    } else candidateRules[[varName]] <- NULL
                }

            nonpredictiveRules <<- candidateRules   
        },

        isStoch = function(nodeRanges) {
            if(!is.list(nodeRanges))
                nodeRanges <- list(nodeRanges)
            if(!all(sapply(nodeRanges, inherits, "nodeRangeClass")))
                stop("isStoch: input must be a `nodeRange` or list of `nodeRange`s") 
            sapply(nodeRanges, function(x) x$declRule$stoch)
        }
    )
)


## Determine nodes of interest, potentially of particular types.
## Incorporates functionality formerly in `getNodeNames` and `expandNodeNames`
getNodes <- function(model, nodes = NULL,
                     stochOnly = FALSE, determOnly = FALSE,
                     includeData = TRUE, dataOnly = FALSE,
                     includePredictive = TRUE, predictiveOnly = FALSE,
                     includeRHSonly = FALSE,
                     topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE) {
    ## `nodes` may contain one or more varRanges or varNames.
    if(topOnly + latentOnly + endOnly > 1)
        stop("only one of `topOnly`, `latentOnly`, `endOnly` can be `TRUE`.")

    if(is.null(nodes)) {
        nodes <- names(model$modelDef$declRules)
        if(includeRHSonly && !stochOnly && !determOnly)
            nodes <- c(nodes, names(model$modelDef$rhsOnlyRules))
    } else {
        if(inherits(nodes, 'varRangeClass'))
            nodes <- list(nodes) 
        if(!all(is.character(nodes) | sapply(nodes, function(node) inherits(node, 'varRangeClass'))))
            stop("`nodes` must be variable names or `varRange`s.")
    }

    ## Filter out; result is varRanges so do this before applying later rules, which produce nodeRanges.
    if(dataOnly) 
        nodes <- flatten(lapply(nodes, function(node) applyRules(model$dataRules, node)))
    if(!includeData) {
        dataNodes <- sapply(nodes, getVarName) %in% names(model$dataRules)  
        nodes <- c(nodes[!dataNodes], flatten(lapply(nodes[dataNodes],
                                                     function(node) applyRules(model$nondataRules, node))))
    }        

    if(predictiveOnly)
        nodes <- flatten(lapply(nodes, function(node) applyRules(model$predictiveRules, node)))
    if(!includePredictive)
        nodes <- flatten(lapply(nodes, function(node) applyRules(model$nonpredictiveRules, node)))
    
    if(!topOnly && !latentOnly && !endOnly) 
        result <- lapply(nodes, function(node) applyRules(model$modelDef$declRules, node))
        
    if(topOnly) result <- lapply(nodes, function(node) applyRules(model$modelDef$topRules, node))
    if(latentOnly) result <- lapply(nodes, function(node) applyRules(model$modelDef$latentRules, node))
    if(endOnly) result <- lapply(nodes, function(node) applyRules(model$modelDef$endRules, node))

    result <- flatten(result)  ## Flatten the result so don't have nested list.

    if(stochOnly)
        result <- result[sapply(result, function(nodeRange) nodeRange$declRule$stoch)]
    if(determOnly)
        result <- result[!sapply(result, function(nodeRange) nodeRange$declRule$stoch)]

    if(includeRHSonly && !stochOnly && !determOnly) {  # RHSonly are considered neither determ not stoch.
        rhsResult <- lapply(nodes, function(node) applyRules(model$modelDef$rhsOnlyRules, node))
        result <- c(result, flatten(rhsResult))
    }

    if(!length(result)) return(NULL)
    
    return(removeDuplicateVarRanges(result))

}

