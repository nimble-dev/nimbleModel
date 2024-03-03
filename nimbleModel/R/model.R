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
            nondataRules <<- lapply(seq_along(nms), function(i)
                varRulesClass$new(newDataRules(data[[i]], nms[i], nondata = TRUE), varName = nms[i]))
            nondataRules <<- nondataRules[!sapply(nondataRules, function(oneVarRules) is.null(oneVarRules$rules))]
        },


        ## Start from ranges based on dataRules and walk upwards,
        ## excluding any parents of such ranges.
        ## For now, this doesn't use any notion of "touched", so there is duplicative
        ## walking up the tree for nodes with multiple data descendants, but given
        ## graph processing is declaration-based, this doesn't seem like an efficiency
        ## concern, particularly since `candidateRules` will progressively shrink.
        makePredictiveRules = function() {
            candidateRules <- unlist(lapply(modelDef$calcRules, function(oneVarRules) {
                stoch <- sapply(oneVarRules$rules, function(rule) rule$declRule$stoch)
                return(oneVarRules$rules[stoch])
            })) # `unlist` removes length-0 entries.
            candidateRules <- newVarRules(candidateRules)
            
            dataRanges <- sapply(dataRules, function(oneVarDataRules)
                lapply(oneVarDataRules$rules, function(dataRule)
                    dataRule$rule$apply()))
            predictiveRules <<- excludeFromPredictiveRules(modelDef, dataRanges, candidateRules)
        }
    
    )
)
