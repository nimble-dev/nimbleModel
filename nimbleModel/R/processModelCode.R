## Modified from old modelDefClass$processBUGScode

## 2022-08-14: this has been transformed into modelDef$processModelCode() method.
processModelCode_impl <- function(modelDef = modelDefClass$new(),
                                  code = NULL,
                                  contextID = 1,
                                  lineNumber = 0,
                                  userEnv) {
    ## uses BUGScode, sets fields: contexts, declInfo$code, declInfo$contextID.
    ## all processing of code is done by BUGSdeclClass$setup(code, contextID).
    ## all processing of contexts is done by BUGScontextClass$setup()
    recursiveCall <- lineNumber != 0
    if(is.null(code)) {
        code <- modelDef$modelCode
        modelDef$declInfo <<- list()
    }
    for(i in 1:length(code)) {
        if(code[[i]] == '{')
            if(length(code[[i]])==1)
                next  ## skip { lines
        lineNumber <- lineNumber + 1
        if(code[[i]][[1]] == '~' ||
           code[[i]][[1]] == '<-') {  ## a BUGS declaration
            iAns <- length(modelDef$declInfo) + 1
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
                                       modelDef$contexts[[contextID]],
                                       lineNumber)
            modelDef$declInfo[[iAns]] <<- modelDeclClassObject
        }
        if(code[[i]][[1]] == 'for') {
            ## e.g. (for i in 1:N).  New context (for-loop info) needed
            indexVarExpr <- code[[i]][[2]]   ## This is the `i`
            if(length(modelDef$contexts) > 0) {
                if(as.character(indexVarExpr) %in%
                   modelDef$contexts[[contextID]]$indexVarNames)
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
            
            nextContextID <- length(modelDef$contexts) + 1
            forCode <- code[[i]][1:3]        ## This is the (for i in 1:N) without the code block
            forCode[[3]] <- indexRangeExpr
            singleContexts <- c(
                if(contextID == 1) NULL
                else modelDef$contexts[[contextID]]$singleContexts,
                list(modelSingleContext(
                    indexVarExpr = indexVarExpr,       ## Add the new context
                    indexRangeExpr = indexRangeExpr,
                    forCode = forCode)
                    )
            )
            modelContextClassObject <- modelContextClass$new()
            modelContextClassObject$setup(singleContexts = singleContexts)
            modelDef$contexts[[nextContextID]] <<- modelContextClassObject
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
                processModelCode_impl(
                    modelDef,
                    recurseCode,
                    nextContextID,
                    lineNumber = lineNumber,
                    userEnv = userEnv)
        }
        if(code[[i]][[1]] == '{') {
            ## recursive call to a block contained in a {},
            ## perhaps as a result of processCodeIfThenElse
            lineNumber <-
                processModelCode_impl(
                    modelDef,
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
    if(recursiveCall)
        return(lineNumber)
    else
        return(modelDef)
}

reprioritizeColonOperator <- function(code) {
    split.code <- strsplit(deparse(code), ":")
    if(length(split.code[[1]]) == 2)
        return(
            parse(
                text = paste0("(",
                              split.code[[1]][1],
                              "):(",
                              split.code[[1]][2],
                              ")"),
                keep.source = FALSE)[[1]])
    if(length(split.code[[1]]) > 2)
        stop(paste0('Error with this code: ',
                    deparse(code))
             )
    return(code)
}
