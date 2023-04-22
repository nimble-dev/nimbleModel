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
        context = NULL,
        sourceLineNumber = NULL,
        code = NULL,
        stoch = NULL,
        distributionName = NA,
        valueExpr = NULL,
        targetExpr = NULL,
        transExpr = NULL,
        indexExpr = NULL,
        targetNodeExpr = NULL,
        targetVarName = NULL,
        targetNodeName = NULL,
        truncated = NULL,
        boundExprs = 'ANY',
        symbolicParentNodes = NULL,
        downstreamRules = NULL,
        upstreamRules = NULL,
        rhsOriginalRules = NULL,
        declRule = NULL,

        replacements = NULL,
        codeReplaced = NULL,
        logProbNodeExpr = NULL,
        replacementNameExprs = NULL,
        altParamExprs = NULL,
        dynamicIndexInfo = NULL,

        ## Determines the parts of a declaration from the raw `code`.
        ## TODO: does setup need `userEnv` input?
        initialize = function(code,
                         context, 
                         sourceLineNumber,
                         truncated = FALSE,
                         boundExprs = NULL) {

            context <<- context
            sourceLineNumber <<- sourceLineNumber
            code <<- code
            truncated <<- truncated
            boundExprs <<- boundExprs
            
            if(code[[1]] == '~') {
                stoch <<- TRUE
                ## Check for legitimate densities or for truncation.
                if(!is.call(code[[3]]) ||
                   (!any(code[[3]][[1]] == getAllDistributionsInfo('namesVector')) &&
                    code[[3]][[1]] != "T" &&
                    code[[3]][[1]] != "I"))
                    stop("modelDeclClass$new: Improper syntax for stochastic declaration: `", deparse(code), "`.")
            } else if(code[[1]] == '<-') {
                stoch <<- FALSE
            } else 
                stop("modelDeclClass$new: Improper syntax for declaration: `", deparse(code), "`.")
            
            targetExpr <<- code[[2]]
            valueExpr <<- code[[3]]
            
            if(stoch)
                distributionName <<- as.character(valueExpr[[1]])

            transExpr <<- NULL
            indexExpr <<- NULL
            
            if(length(targetExpr) > 1) {
                ## There is a tranformation and/or a subscript.
                if(targetExpr[[1]] == '[') {
                    ## It is a subscript only.
                    indexExpr <<- as.list(targetExpr[-c(1,2)]) 
                    targetVarExpr <- targetExpr[[2]]
                    targetNodeExpr <<- targetExpr
                } else {
                    ## There is a transformation, possibly with a subscript.
                    transExpr <<- targetExpr[[1]]
                    targetNodeExpr <<- targetExpr[[2]]
                    if(length(targetNodeExpr)>1) {
                        ## There are subscripts inside the transformation
                        if(targetNodeExpr[[1]] != '[') 
                            stop("modelDeclClass$new: Invalid subscripting for `", deparse(targetExpr), "`.")
                        indexExpr <<- as.list(targetNodeExpr[-c(1,2)])
                        targetVarExpr <- targetNodeExpr[[2]]
                    } else {
                        targetVarExpr <- targetNodeExpr
                    }
                }
            } else {
                ## No tranformation or subscript present.
                targetVarExpr <- targetExpr
                targetNodeExpr <<- targetVarExpr
            }
            targetVarName <<- safeDeparse(targetVarExpr, warn = TRUE)
            targetNodeName <<- safeDeparse(targetNodeExpr, warn = TRUE)

        },

        ## Create declRule and declaration-specific graph and RHS rules.
        processDecl = function(nimFunNames, constants = list(), envir) {
            declRule <<- declRuleClass$new(code, sourceLineNumber, context, constants)
            makeSymbolicParentNodes(nimFunNames, constants, envir)
            invisible(NULL)
        },

        ## Determine RHS pieces.
        makeSymbolicParentNodes = function(nimFunNames, constants = list(), envir) {
            constantsNamesList <- lapply(names(constants), as.name)
            symbolicParentNodes <<-
                unique(
                    getSymbolicParentNodes(valueExpr,
                                           constantsNamesList,
                                           context$indexVarExprs,
                                           nimFunNames,
                                           envir = envir)
                )
            invisible(NULL)
        },
        
        makeGraphRules = function(constants = list()) {
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
        makeRHSoriginalRules = function(constants = list()) {
            rhsOriginalRules <<- vector('list',
                                      length(symbolicParentNodes))
            for(i in seq_along(symbolicParentNodes)) {
                rhsOriginalRules[[i]] <<-
                    rhsRuleClass$new(symbolicParentNodes[[i]], NULL, context, constants)
            }
            invisible(NULL)
        },

        genReplacementsAndCodeReplaced = function(nimFunNames, constants = list(), envir) {
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

        genAltParams = function() {
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
        
        replaceDynamicIndexingInParents = function(varInfo) {
            dynamicIndexInfo <<- list()
            for(iSPN in seq_along(symbolicParentNodes)) {
                symbolicParent <- symbolicParentNodes[[iSPN]]
                dynamicIndices <- detectDynamicIndices(symbolicParent)
                ## We do not yet check bounds of inner indexes in nested indexing. To do so we need to
                ## find dynamic indexing within a USED_IN_INDEX() and add to dynamicIndexInfo;
                ## then in nodeFunctions we need nested if statements so inner index is checked first.
                ## That being said, compiled execution will error out with appropriate out of bounds error
                ## because C++ will put an out-of-bound value in for 'k' in k[d[0]] or k[d[1342134]].
                if(any(dynamicIndices)) {
                    indexedVar <- safeDeparse(symbolicParent[[2]])
                    for(iIndex in which(dynamicIndices)) {
                        lower <- varInfo[[indexedVar]]$mins[iIndex]
                        upper <- varInfo[[indexedVar]]$maxs[iIndex]                        
                        dynamicIndexInfo[[length(dynamicIndexInfo) + 1]] <<-
                            list(indexCode = stripIndexWrapping(symbolicParent[[2+iIndex]]),
                                 lower = lower,
                                 upper = upper)
                        fullExtent <- substitute(A:B, list(A = lower, B = upper))
                        ## Indexing code not needed anymore.
                        symbolicParentNodes[[iSPN]][[2+iIndex]] <<- fullExtent
                    }
                }
            }
            symbolicParentNodes <<- lapply(symbolicParentNodes, stripIndexWrapping)
            invisible(NULL)
        }
    )
)


genReplacementsAndCodeRecurse <- function(code,
                                          constAndIndexNames,
                                          nimbleFunctionNames,
                                          replaceVariableLHS = TRUE,
                                          envir) {
    if(is.numeric(code) || is.logical(code))
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
                                    startingAt = 2))
        isRfunction <- !any(code[[1]] == nimbleFunctionNames) # Can't use `%in%` as nFN is a list.
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
        return(replaceWhatPossible(code,
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


detectDynamicIndices <- function(expr) {
    if(length(expr) == 1 || expr[[1]] != "[") return(FALSE) 
    return(sapply(expr[3:length(expr)], isDynamicIndex)) 
}

functionsThatShouldNeverBeReplacedInModelCode <- c(':','nimC','nimRep','nimSeq', 'diag',
                                                  'nimNumeric', 'nimMatrix', 'nimArray')
