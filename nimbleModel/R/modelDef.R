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
            declRules <- lapply(declInfo, function(x) x$declRule)
            rhsOriginalRules <- unlist(lapply(declInfo, function(x) x$rhsOriginalRules))
            allDownstreamRules <- unlist(lapply(declInfo, function(x) x$downstreamRules))
            varNames <- sapply(allDownstreamRules, function(rule)
                rule$parentVar)
            downstreamRules <<- lapply(unique(varNames), function(nm)
                allDownstreamRules[varNames == nm])
            names(downstreamRules) <<- unique(varNames)
            calcRules <<- generateCalcRules(declRules, rhsOriginalRules, downstreamRules)
            rhsOnlyRules <<- generateRHSonlyRules(rhsOriginalRules, declRules)
            setSortIDs(calcRules)  ## do before top/end to catch cycles
            setEndNodes(calcRules)
            setTopNodes(calcRules)
            ## setLatentNodes(calcRules)
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
