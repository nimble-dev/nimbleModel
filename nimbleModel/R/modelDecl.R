## Class for representing information in a single declaration, replacing `BUGSdeclClass`.
## Class methods process the code to generate information about the node dependencies
## set up by the declaration.

## There are components not yet moved from current nimble that will be needed.
## These will probably include targetIndexNamePieces and parentIndexNamePieces.
## They will probably not include the various *replacements*.

modelDeclClass <- R6Class(
    classname = 'modelDeclClass',
    portable = FALSE,
    public = list(
        ## In current NIMBLE we hold `contextID` here, but context itself is needed.
        context = NULL,
        sourceLineNumber = NULL,
        code = NULL,
        constants = NULL,
        stoch = NULL,
        distributionName = NULL,
        valueExpr = NULL,
        targetExpr = NULL,
        transExpr = NULL,
        indexExpr = NULL,
        targetVarExpr = NULL,
        targetNodeExpr = NULL,
        targetVarName = NULL,
        targetNodeName = NULL,
        indexVariableExprs = NULL,
        truncated = NULL,
        boundExprs = NULL,
        symbolicParentNodes = NULL,
        downstreamRules = NULL,
        upstreamRules = NULL,
        rhsOriginalRules = NULL,
        declRule = NULL,

        replacements = NULL,
        codeReplaced = NULL,
        symbolicParentNodesReplaced = NULL,
        logProbNodeExpr = NULL,
        replacementNameExprs = NULL,
        altParamExprs = NULL,
        targetExprReplaced = NULL,
        valueExprReplaced = NULL,
        rhsVars = NULL,
        targetIndexNamePieces = NULL,
        parentIndexNamePieces = NULL,
        dynamicIndexInfo = NULL,

        ## Figure out the parts of the declaration.
        ## TODO: any reason not to have `$setup` become `$initialize`?
        ## TODO: does setup need `userEnv` input?
        setup = function(code,
                         context,
                         constants,
                         sourceLineNumber,
                         truncated = FALSE,
                         boundExprs = NULL) {
            modelDeclClass_setup(self,
                                 code,
                                 context,
                                 constants,
                                 sourceLineNumber,
                                 truncated,
                                 boundExprs)
        },

        ## Create declRule and declaration-specific graph and RHS rules.
        processDecl = function(nimFunNames, envir = .GlobalEnv) {
            declRule <<- declRuleClass$new(code, sourceLineNumber, context, constants)
            makeSymbolicParentNodes(nimFunNames, envir)
            invisible(NULL)
        },

        ## Determine RHS pieces.
        makeSymbolicParentNodes = function(nimFunNames, envir = .GlobalEnv) {
            constantsNamesList <- lapply(names(constants), as.name)
            symbolicParentNodes <<-
                unique(
                    getSymbolicParentNodes(valueExpr,
                                           constantsNamesList,
                                           context$indexVarExprs,
                                           nimFunNames,
                                           envir)
                )
            invisible(NULL)
        },
        
        makeGraphRules = function() {
            downstreamRules <<- vector('list',
                                      length(symbolicParentNodes))
            upstreamRules <<- vector('list',
                                     length(symbolicParentNodes))
            
            for(i in seq_along(symbolicParentNodes)) {
                downstreamRules[[i]] <<-
                    graphRuleClass$new(targetNodeExpr,
                                       symbolicParentNodes[[i]],
                                       context,
                                       constants,
                                       stoch)
                upstreamRules[[i]] <<-
                    graphRuleClass$new(symbolicParentNodes[[i]],
                                       targetNodeExpr,
                                       context,
                                       constants,
                                       stoch)
                
            }
            invisible(NULL)
        },

        ## Make an initial RHSrule for each RHS piece. 
        makeRHSoriginalRules = function() {
            rhsOriginalRules <<- vector('list',
                                      length(symbolicParentNodes))
            for(i in seq_along(symbolicParentNodes)) {
                rhsOriginalRules[[i]] <<-
                    rhsRuleClass$new(symbolicParentNodes[[i]], NULL, context, constants)
            }
            invisible(NULL)
        },

        setIndexVariableExprs = function(exprs) {
            indexVariableExprs <<- exprs
            invisible(NULL)
        },

        genReplacementsAndCodeReplaced = function(nimFunNames, envir = .GlobalEnv) {
            constantsNamesList <- lapply(names(constants), as.name)
            replacementsAndCode <-
                genReplacementsAndCodeRecurse(code,
                                              c(constantsNamesList, context$indexVarExprs),
                                              nimFunNames,
                                              envir = envir)
            replacements <<- replacementsAndCode$replacements
            codeReplaced <<- replacementsAndCode$codeReplaced
            
            if(stoch) {
                logProbNodeExprAndReplacements <-
                    genLogProbNodeExprAndReplacements(code,
                                                      codeReplaced,
                                                      context$indexVarExprs)
                logProbNodeExpr <<-
                    logProbNodeExprAndReplacements$logProbNodeExpr
                replacements <<-
                    c(replacements, logProbNodeExprAndReplacements$replacements)
            } else logProbNodeExpr <<- NULL
            
            replacementNameExprs <<-
                lapply(
                    as.list(names(replacements)),
                    as.name
                )
            names(replacementNameExprs) <<- names(replacements)
            invisible(NULL)
        },

        genAltParamsModifyCodeReplaced = function() {
            altParamExprs <<- list()
            if(stoch) {
                RHSreplaced <- codeReplaced[[3]]
                if(length(RHSreplaced) > 1) { # It actually has argument(s).
                    paramNamesAll <- names(RHSreplaced)
                    paramNamesDotLogicalVector <- grepl('^\\.', paramNamesAll)
                    ## Remove all parameters whose name begins with '.' from distribution.
                    RHSreplacedWithoutDotParams <-
                        RHSreplaced[!paramNamesDotLogicalVector]
                    codeReplaced[[3]] <<- RHSreplacedWithoutDotParams
                    
                    altParamExprs <<-
                        if(any(paramNamesDotLogicalVector))
                            as.list(RHSreplaced[paramNamesDotLogicalVector])
                        else
                            list()
                    ## Remove the '.' from each name.
                    names(altParamExprs) <<-
                        gsub('^\\.', '', names(altParamExprs))    
                }
            }
            invisible(NULL)
        },

        genBounds = function() {
            boundExprs <<- list()
            if(stoch) {
                RHSreplaced <- codeReplaced[[3]]
                if(length(RHSreplaced) > 1) { # It actually has argument(s).
                    boundNames <- c('lower_', 'upper_')
                    boundExprs <<- as.list(RHSreplaced[boundNames])
                    if(truncated) {  # Check for user-provided constant bounds inconsistent with distribution range.
                        distName <- as.character(RHSreplaced[[1]])
                        distRange <- getDistributionInfo(distName)$range
                        if(is.numeric(boundExprs$lower_) &&
                           is.numeric(distRange$lower) &&
                           is.numeric(boundExprs$upper_) &&
                           is.numeric(distRange$upper) &&
                           boundExprs$lower_ <= distRange$lower &&
                           boundExprs$upper_ >= distRange$upper)  # User-specified bounds are irrelevant.
                            truncated <<- FALSE
                        
                        if(is.numeric(boundExprs$lower_) &&
                           is.numeric(boundExprs$upper_) &&
                           boundExprs$lower_ >= boundExprs$upper_)
                            messageIfVerbose("   [Warning] Lower bound is greater than or equal to upper bound in `",
                                           safeDeparse(codeReplaced),
                                           "`. Proceeding anyway, but this is likely to cause numerical issues.")
                        if(is.numeric(boundExprs$lower_) &&
                           is.numeric(distRange$lower) &&
                           boundExprs$lower_ < distRange$lower) {
                            messageIfVerbose("   [Warning] Lower bound is less than or equal to distribution lower bound in `",
                                           safeDeparse(codeReplaced), "`. Ignoring user-provided lower bound.")
                            boundExprs$lower_ <<- distRange$lower
                            codeReplaced[[3]]['lower_'] <<- distRange$lower
                        }
                        if(is.numeric(boundExprs$upper_) &&
                           is.numeric(distRange$upper) &&
                           boundExprs$upper_ > distRange$upper) {
                            messageIfVerbose("   [Warning] Upper bound is greater than or equal to distribution upper bound in `",
                                           safeDeparse(codeReplaced),
                                           "`. Ignoring user-provided upper bound.")
                            boundExprs$upper_ <<- distRange$upper
                            codeReplaced[[3]]['upper_'] <<- distRange$upper
                        }
                    }
                    if(!truncated) {
                        boundNamesLogicalVector <-
                            names(RHSreplaced) %in% boundNames
                        RHSreplacedWithoutBounds <-
                            RHSreplaced[!boundNamesLogicalVector]    
                        codeReplaced[[3]] <<- RHSreplacedWithoutBounds
                    }
                }
            }
            invisible(NULL)
        },
        
        genReplacedTargetValueAndParentInfo = function(nimFunNames, envir = .GlobalEnv) {
            constantsNamesList <- lapply(names(constants), as.name)

            ## This assumes codeReplaced is there.
            ## Generate hasBracket info.
            targetExprReplaced <<- codeReplaced[[2]]
            ## `targetExprReplaced` shouldn't have any link functions at this point.
            valueExprReplaced <<- codeReplaced[[3]]
            if(stoch)
                distributionName <<- as.character(valueExprReplaced[[1]])
            else
                distributionName <<- NA
            
            symbolicParentNodesReplaced <<-
                unique(
                    getSymbolicParentNodes(valueExprReplaced,
                                           constantsNamesList,
                                           c(context$indexVarExprs,
                                             replacementNameExprs),
                                           nimFunNames,
                                           context = context,
                                           envir = envir)
                )
            if(!nimbleOptions()$allowDynamicIndexing) {
                rhsVars <<-
                    unlist(
                        lapply(
                            symbolicParentNodesReplaced,
                            function(x) 
                                if(length(x) == 1)
                                    as.character(x)
                                else
                                    as.character(x[[2]])
                        )
                    )
            } else {
                ## This use of symbolicParentNodes and not
                ## symbolicParentNodesReplaced deals with fact that 'd' is
                ## inserted in front of digits in symbolicParentNodesReplaced
                ## in naming when we have something like k[9-i] but not in
                ## symbolicParentNodes or in varInfo names.
                rhsVars <<- unlist(
                    lapply(
                        symbolicParentNodes[seq_along(symbolicParentNodesReplaced)],
                        function(x) {
                            x <- stripIndexWrapping(x) ## handles dynamic index wrapping
                            if(length(x) == 1) as.character(x) else as.character(x[[2]])
                        })
                )
            }

            ## Note that makeIndexNamePieces is designed only for indices that
            ##     are a single name or number, a `:` operator with single
            ##     name or number for each argument, or an NA (for a dynamic
            ##     index).  This relies on the fact that any expression will
            ##     have been lifted by this point and what it has been
            ##     replaced with is simply a name.  This means
            ##     makeIndexNamePieces can include a diagnostic.
            targetIndexNamePieces <<-
                try(
                    if(length(targetExprReplaced) > 1)
                        lapply(targetExprReplaced[-c(1,2)],
                               makeIndexNamePieces)
                    else
                        NULL
                )
            if(inherits(targetIndexNamePieces, 'try-error'))
                stop("genReplacedTargetValueAndParentInfo: Cannot process `",
                     safeDeparse(targetExprReplaced), "`.",
                     call. = FALSE)
            if(!nimbleOptions()$allowDynamicIndexing) {
                parentIndexNamePieces <<-
                    lapply(symbolicParentNodesReplaced,
                           function(x)
                               if(length(x) > 1)
                                   lapply(x[-c(1,2)],
                                          makeIndexNamePieces)
                               else
                                   NULL
                           )
            } else
                parentIndexNamePieces <<-
                    lapply(symbolicParentNodesReplaced,
                           function(x) {
                               x <- stripIndexWrapping(x)
                               if(length(x) > 1)
                                   lapply(x[-c(1,2)], makeIndexNamePieces)
                               else
                                   NULL
                           }
                           )
            invisible(NULL)
        },

        insertFullIndexingForDynamicallyIndexedParents = function() {
            dynamicIndexInfo <<- list()
            for(iSPN in seq_along(symbolicParentNodesReplaced)) {
                symbolicParent <- symbolicParentNodesReplaced[[iSPN]]
                dynamicIndices <- detectDynamicIndexes(symbolicParent)
                ## We do not yet check bounds of inner indexes in nested indexing. To do so we need to
                ## find dynamic indexing within a USED_IN_INDEX() and add to dynamicIndexInfo;
                ## then in nodeFunctions we need nested if statements so inner index is checked first.
                ## That being said, compiled execution will error out with appropriate out of bounds error
                ## because C++ will put an out-of-bound value in for 'k' in k[d[0]] or k[d[1342134]].
                if(any(dynamicIndices)) {
                    indexedVar <- stripUnknownIndexFromVarName(safeDeparse(symbolicParent[[2]], warn = TRUE))
                    numSPNR <- length(symbolicParentNodesReplaced)
                    for(iIndex in which(dynamicIndices)) {
                        lower <- varInfo[[indexedVar]]$mins[iIndex]
                        upper <- varInfo[[indexedVar]]$maxs[iIndex]                        
                        dynamicIndexInfo[[length(declInfo[[iDI]]$dynamicIndexInfo) + 1]] <<-
                            list(indexCode = stripDynamicallyIndexedWrapping(symbolicParent[[2+iIndex]]),
                                 lower = lower,
                                 upper = upper)
                        fullExtent <- substitute(A:B, list(A = lower, B = upper))
                        symbolicParentNodes[[iSPN]][[2+iIndex]] <<- fullExtent
                        if(iSPN <= numSPNR)
                            symbolicParentNodesReplaced[[iSPN]][[2+iIndex]] <<- fullExtent
                    }
                }
            }
            symbolicParentNodes <<- lapply(symbolicParentNodes, stripIndexWrapping)
            symbolicParentNodesReplaced <<- lapply(symbolicParentNodesReplaced, stripIndexWrapping)
            invisible(NULL)
        }
    )
)

## Determines the parts of a declaration from the raw `code`.
modelDeclClass_setup <- function(modelDecl,
                                 code,
                                 context,
                                 constants,
                                 sourceLineNumber,
                                 truncated = FALSE,
                                 boundExprs = NULL) {

    modelDecl$constants <- constants
    modelDecl$context <- context
    modelDecl$sourceLineNumber <- sourceLineNumber
    modelDecl$code <- code
    modelDecl$truncated <- truncated
    modelDecl$boundExprs <- boundExprs
    
    if(code[[1]] == '~') {
        modelDecl$stoch <- TRUE
        ## Check for legitimate densities or for truncation.
        if(!is.call(code[[3]]) ||
           (!any(code[[3]][[1]] == getAllDistributionsInfo('namesVector')) &&
            code[[3]][[1]] != "T" &&
            code[[3]][[1]] != "I"))
            stop("modelDeclClass$new: Improper syntax for stochastic declaration: `", deparse(code), "`.")
    } else if(code[[1]] == '<-') {
        modelDecl$stoch <- FALSE
    } else 
        stop("modelDeclClass$new: Improper syntax for declaration: `", deparse(code), "`.")
    
    targetExpr <- code[[2]]
    valueExpr <- code[[3]]
    
    if(modelDecl$stoch)
        modelDecl$distributionName <- as.character(valueExpr[[1]])
    else
        modelDecl$distributionName <- NA

    transExpr <- NULL
    indexExpr <- NULL
        
    if(length(targetExpr) > 1) {
        ## There is a tranformation and/or a subscript.
        if(targetExpr[[1]] == '[') {
            ## It is a subscript only.
            indexExpr <- as.list(targetExpr[-c(1,2)]) 
            targetVarExpr <- targetExpr[[2]]
            targetNodeExpr <- targetExpr
        } else {
            ## There is a transformation, possibly with a subscript.
            transExpr <- targetExpr[[1]]
            targetNodeExpr <- targetExpr[[2]]
            if(length(targetNodeExpr)>1) {
                ## There are subscripts inside the transformation
                if(targetNodeExpr[[1]] != '[') 
                    stop("modelDeclClass$new: Invalid subscripting for `", deparse(targetExpr), "`.")
                indexExpr <- as.list(targetNodeExpr[-c(1,2)])
                targetVarExpr <- targetNodeExpr[[2]]
            } else {
                targetVarExpr <- targetNodeExpr
            }
        }
    } else {
        ## No tranformation or subscript present.
        targetVarExpr <- targetExpr
        targetNodeExpr <- targetVarExpr
    }

    modelDecl$targetExpr <- targetExpr
    modelDecl$valueExpr <- valueExpr
    modelDecl$transExpr <- transExpr
    modelDecl$indexExpr <- indexExpr
    modelDecl$targetVarExpr <- targetVarExpr
    modelDecl$targetNodeExpr <- targetNodeExpr
    modelDecl$targetVarName <- deparse(targetVarExpr)
    modelDecl$targetNodeName <- deparse(targetNodeExpr)
    invisible(NULL)
}

genReplacementsAndCodeRecurse <- function(code,
                                          constAndIndexNames,
                                          nimbleFunctionNames,
                                          replaceVariableLHS = TRUE,
                                          envir = .GlobalEnv) {
    if(is.numeric(code) || is.logical(code) ||
       (nimbleOptions()$allowDynamicIndexing &&
                       length(code) > 1 &&
                       code[[1]] == '.DYN_INDEXED')
       )
        ## Check for .DYN_INDEXED deals with processing of code when
        ## we add unknownIndex declarations.
        return(list(codeReplaced = code,
                    replacements = list(),
                    replaceable = TRUE))
    cLength <- length(code)
    if(cLength == 1) {
        if(is.name(code)) {
            if(any(code == constAndIndexNames) && replaceVariableLHS)
                return(replaceAllCodeSuccessfully(code))
            else
                return(list(codeReplaced = code,
                            replacements = list(),
                            replaceable = FALSE))
        }
    }
    if(is.call(code)) {
        indexingBracket <- code[[1]] == '['
        if(indexingBracket) {
            if(is.call(code[[2]])) indexingBracket <- FALSE # Treat like any other function.
        }
        if(indexingBracket) { 
            contents <- lapply(
                code[-c(1,2)],
                function(x)
                    genReplacementsAndCodeRecurse(x,
                                                  constAndIndexNames,
                                                  nimbleFunctionNames,
                                                  envir = envir)
            )
            contentsCodeReplaced <-
                lapply(contents, function(x) x$codeReplaced)
            contentsReplacements <-
                lapply(contents, function(x) x$replacements)
            contentsReplaceable  <-
                unlist(lapply(contents, function(x) x$replaceable))
            if(replaceVariableLHS) {
                variable <-
                    genReplacementsAndCodeRecurse(code[[2]],
                                                  constAndIndexNames,
                                                  nimbleFunctionNames,
                                                  envir = envir)
                if(variable$replaceable &&
                   all(contentsReplaceable))
                    return(replaceAllCodeSuccessfully(code))
            }
            return(replaceWhatPossible(code,
                                    contentsCodeReplaced,
                                    contentsReplacements,
                                    contentsReplaceable,
                                    startingAt=3))
        }
        assignment <- any(code[[1]] == c('<-', '~'))
        if(cLength > 1) {
            if(assignment) {
                ## In an assignment, prevent the outermost variable on the LHS from being replaced.
                contents <-
                    c(
                        list(
                            genReplacementsAndCodeRecurse(code[[2]],
                                                          constAndIndexNames,
                                                          nimbleFunctionNames,
                                                          replaceVariableLHS = FALSE,
                                                          envir = envir)
                        ),
                        lapply(
                            code[-c(1,2)],
                            function(x)
                                genReplacementsAndCodeRecurse(x,
                                                              constAndIndexNames,
                                                              nimbleFunctionNames,
                                                              envir = envir))
                    )
            } else {
                contents <- lapply(
                    code[-1],
                    function(x)
                        genReplacementsAndCodeRecurse(x,
                                                      constAndIndexNames,
                                                      nimbleFunctionNames,
                                                      envir = envir))
            }
            contentsCodeReplaced <- lapply(contents, function(x) x$codeReplaced)
            contentsReplacements <- lapply(contents, function(x) x$replacements)
            contentsReplaceable  <- unlist(lapply(contents, function(x) x$replaceable))
            allContentsReplaceable <- all(contentsReplaceable)
        } else {
            contentsCodeReplaced <- list()
            contentsReplacements <- list()
            contentsReplaceable  <- list()
            allContentsReplaceable <- TRUE
        }
        ## Do not replace if it is from a special set of functions
        ## or is a nimbleFunction (specifically, an RCfunction).
        funName <- safeDeparse(code[[1]], warn = TRUE)
        if(funName %in% functionsThatShouldNeverBeReplacedInModelCode ||
                (exists(funName, envir) && is.rcf(get(funName, envir))))             
           return(replaceWhatPossible(code,
                                    contentsCodeReplaced,
                                    contentsReplacements,
                                    contentsReplaceable,
                                    startingAt=2))
        if(assignment)
            return(replaceWhatPossible(code,
                                    contentsCodeReplaced,
                                    contentsReplacements,
                                    contentsReplaceable,
                                    startingAt=2))
        isRfunction <- !code[[1]] %in% nimbleFunctionNames
        isRonly <-
            isRfunction &
            !checkNimbleOrRfunctionNames(safeDeparse(code[[1]], warn = TRUE), envir)
        if(safeDeparse(code[[1]], warn = TRUE) == '$')
            isRonly <- FALSE
        if(isRonly & !allContentsReplaceable)
            stop("genReplacementsAndCodeRecurse: R function `", 
                        safeDeparse(code[[1]]),
                        "` has non-replaceable node values as arguments. It must be a nimbleFunction.")
        if(isRfunction & allContentsReplaceable)
            return(replaceAllCodeSuccessfully(code))
        return(replacePossible(code,
                                contentsCodeReplaced,
                                contentsReplacements,
                                contentsReplaceable,
                                startingAt = 2))
    }
    stop("genReplacementsAndCodeRecurse: processing error in `", safeDeparse(code), "`.")
}

genLogProbNodeExprAndReplacements <- function(code,
                                              codeReplaced,
                                              indexVarExprs) {
    logProbNodeExpr <- codeReplaced[[2]]   # Initially, use the replaced version.
    replacements <- list()
    
    if(length(logProbNodeExpr) == 1) {  # No indexing present.
        logProbNodeExpr <- as.name(makeLogProbName(logProbNodeExpr))   
    } else {  # Indexing on the LHS node.
        if(logProbNodeExpr[[1]] != '[')
            stop("genLogProbNodeExprAndReplacements: cannot process `", safeDeparse(logProbNodeExpr), "`.")
        logProbNodeExpr[[2]] <- as.name(makeLogProbName(logProbNodeExpr[[2]]))
        
        origLHS <- code[[2]]
        for(i in seq_along(origLHS)[-c(1,2)]) {
            origIndex <- origLHS[[i]]
            if(is.vectorized(origIndex)) {
                if(any(indexVarExprs %in% all.vars(origIndex))) {
                    ## The vectorized index includes a loop-indexing
                    ## variable; we will create a replacement, for a
                    ## memberData, for each nodeFunction.
                    replacementExpr <- substitute(min(EXPR),
                                                  list(EXPR=origIndex))
                    replacementName <- Rname2CppName(replacementExpr,
                                                     colonsOK = TRUE)
                    logProbNodeExpr[[i]] <- as.name(replacementName)
                    replacements[[replacementName]] <- replacementExpr
                } else {
                    ## No loop-indexing variables present in the vectorized index.
                    ## his index should be constant for all instances of this nodeFunction.
                    logProbIndexValue <- as.numeric(min(eval(origIndex)))   # eval() should not cause an error...
                    logProbNodeExpr[[i]] <- logProbIndexValue
                }
            }
        }
    }
    return(list(logProbNodeExpr = logProbNodeExpr,
         replacements = replacements))
}


replaceAllCodeSuccessfully <- function(code) {
    deparsedCode <- Rname2CppName(code, colonsOK = TRUE)
    replacements <- list()
    replacements[[deparsedCode]] <- code
    return(list(codeReplaced = as.name(deparsedCode),
                replacements = replacements,
                replaceable = TRUE))
}

replaceWhatPossible <- function(code,
                             contentsCodeReplaced,
                             contentsReplacements,
                             contentsReplaceable,
                             startingAt,
                             replaceable = FALSE) {
    replacements <- list()
    codeReplaced <- code
    if(length(code) >= startingAt)
        for(i in seq_along(contentsReplaceable)) {
            replacements <- c(replacements,
                              contentsReplacements[[i]])
            codeReplaced[[i+startingAt-1]] <- contentsCodeReplaced[[i]]
        }
    replacements <- replacements[unique(names(replacements))]
    return(list(codeReplaced = codeReplaced,
             replacements = replacements,
             replaceable = replaceable))
}

functionsThatShouldNeverBeReplacedInModelCode <- c(':','nimC','nimRep','nimSeq', 'diag',
                                                  'nimNumeric', 'nimMatrix', 'nimArray')
