## Class for representing information in a single declaration, replacing `BUGSdeclClass`.
## Class methods process the code to generate information about the dependencies
## set up by the declaration.

modelDeclClass <- R6Class(
    classname = 'modelDeclClass',
    portable = FALSE,
    public = list(
        context = NULL,           # FUTURE: might just use declRule$context
        sourceLineNumber = NULL,
        stoch = FALSE,            # Need this here as used before declRule created.
        code = NULL,
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
        calculateCode = NULL,
        altParamExprs = NULL,
        dynamicIndexInfo = NULL,

        ## Determines the parts of a declaration from the raw `code`.
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
            
            targetExpr <<- code[[2]]
            valueExpr <<- code[[3]]

            if(code[[1]] == '~') {
                ## Check for legitimate densities or for truncation.
                if(!is.call(code[[3]]) ||
                   (!any(code[[3]][[1]] == getAllDistributionsInfo('namesVector')) &&
                    code[[3]][[1]] != "T" &&
                    code[[3]][[1]] != "I"))
                    stop("improper syntax for stochastic declaration: `", safeDeparse(code), "`.")
                distributionName <<- as.character(valueExpr[[1]])
                stoch <<- TRUE
            } else if(code[[1]] != '<-') {
                stop("improper syntax for declaration: `", safeDeparse(code), "`.")
            }                 
            
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
                            stop("invalid subscripting for `", safeDeparse(targetExpr), "`.")
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

        ## We require multivariate parameters be defined as separate deterministic variables.
        checkMultivarExpr = function() {
            if(!stoch) return(NULL)
            types <- nimble:::distributions[[distributionName]]$types
            if(is.null(types)) return(NULL)
            if(length(valueExpr) > 1) {
                for(k in 2:length(valueExpr)) {
                    paramName <- names(valueExpr)[k]
                    nDim <- types[[paramName]][['nDim']]
                    if(is.numeric(nDim) && nDim == 0) return(NULL)
                    if(checkForExpr(valueExpr[[k]])) {
                        ## Draft gentler warning for possible future adoption: message("Warning about parameter '", names(decl$valueExpr)[k], "' of distribution '", dist, "': This multivariate parameter is provided as an expression. If this is a costly calculation, try making it a separate model declaration for it to improve efficiency.")
                        stop("error with parameter `", names(valueExpr)[k], "` of distribution `",
                             distributionName, "`: multivariate parameters cannot be expressions; please define the expression as a separate deterministic variable\n",
                             "and use that variable as the parameter.")  
                    }
                }
            }
            invisible(NULL)
        },

        ## Create declRule and symbolic RHS pieces.
        processDecl = function(nimFunNames, constants = list(), envir) {
            declRule <<- declRuleClass$new(self, sourceLineNumber, context, constants)
            makeSymbolicParentNodes(nimFunNames, constants, envir)
            invisible(NULL)
        },

        ## Determine RHS pieces (expressed symbolically), needed to create graphRules.
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

        ## Make all up- and down-stream rules for the declaration.
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
                                       declRule$decl$stoch)
                upstreamRules[[i]] <<-
                    graphRuleClass$new(symbolicParentNodes[[i]],
                                       targetNodeExpr,
                                       context,
                                       constants,
                                       declRule$decl$stoch)
                
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
        
        genAltParams = function() {
            altParamExprs <<- list()
            calculateCode <<- code
            if(declRule$decl$stoch) {
                RHSreplaced <- code[[3]]
                if(length(RHSreplaced) > 1) { # It actually has argument(s).
                    paramNamesAll <- names(RHSreplaced)
                    paramNamesDotLogicalVector <- grepl('^\\.', paramNamesAll)
                    ## Remove all parameters whose name begins with '.' from distribution.
                    RHSreplacedWithoutDotParams <-
                        RHSreplaced[!paramNamesDotLogicalVector]
                    calculateCode[[3]] <<- RHSreplacedWithoutDotParams
                    
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
            if(declRule$decl$stoch) {
                RHSreplaced <- calculateCode[[3]]
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
                                           safeDeparse(calculateCode),
                                           "`. Proceeding anyway, but this is likely to cause numerical issues.")
                        if(is.numeric(boundExprs$lower_) &&
                           is.numeric(distRange$lower) &&
                           boundExprs$lower_< distRange$lower) {
                            messageIfVerbose("   [Warning] Lower bound is less than or equal to distribution lower bound in `",
                                           safeDeparse(calculateCode), "`. Ignoring user-provided lower bound.")
                            boundExprs$lower_ <<- distRange$lower
                            calculateCode[[3]]['lower_'] <<- distRange$lower
                        }
                        if(is.numeric(boundExprs$upper_) &&
                           is.numeric(distRange$upper) &&
                           boundExprs$upper_ > distRange$upper) {
                            messageIfVerbose("   [Warning] Upper bound is greater than or equal to distribution upper bound in `",
                                           safeDeparse(calculateCode),
                                           "`. Ignoring user-provided upper bound.")
                            boundExprs$upper_ <<- distRange$upper
                            calculateCode[[3]]['upper_'] <<- distRange$upper
                        }
                    }
                    if(!truncated) {
                        boundNamesLogicalVector <-
                            names(RHSreplaced) %in% boundNames
                        RHSreplacedWithoutBounds <-
                            RHSreplaced[!boundNamesLogicalVector]    
                        calculateCode[[3]] <<- RHSreplacedWithoutBounds
                    }
                }
            }
            invisible(NULL)
        },

        ## Replace dynamic indexing with maximal range (e.g., mu[k[i]] -> mu[1:2147483647],
        ## needed for setting up graphRules, as we don't dynamically determine dependents.
        replaceDynamicIndexingInParents = function(varInfo) {
            dynamicIndexInfo <<- list()
            symbolicParentNodes <<- lapply(symbolicParentNodes, stripIndexWrapping)
            for(iSPN in seq_along(symbolicParentNodes)) {
                symbolicParent <- symbolicParentNodes[[iSPN]]
                dynamicIndices <- detectDynamicIndices(symbolicParent)
                ## We do not yet check bounds of inner indexes in nested indexing.
                ## In nodeFunctions we would need nested if statements so inner index is checked first.
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
            invisible(NULL)
        },

        buildFunctions = function() {
            declRule$buildFunctions(calculateCode, genLogProbExpr())
        },

        genLogProbExpr = function() {
            if(declRule$decl$stoch) {
                logProbExpr <- code[[2]]   
                if(length(logProbExpr) == 1) {  # No indexing present.
                    logProbExpr <- as.name(makeLogProbName(logProbExpr))   
                } else {  # Indexing on the LHS node.
                    if(logProbExpr[[1]] != '[')
                        stop("cannot process `", safeDeparse(logProbExpr), "`.")
                    logProbExpr[[2]] <- as.name(makeLogProbName(logProbExpr[[2]]))
                    
                    origLHS <- code[[2]]
                    for(i in seq_along(origLHS)[-c(1,2)]) {
                        origIndex <- origLHS[[i]]
                        if(is.vectorized(origIndex)) {
                            if(any(context$indexVarExprs %in% all.vars(origIndex))) {
                                ## The vectorized index includes a loop-indexing
                                ## variable; we will create a replacement, for a
                                ## memberData, for each nodeFunction.
                                if(origIndex[[1]] == ":") {
                                    logProbExpr[[i]] <- origIndex[[2]] # generally the minimum
                                } else stop("unexpected input in `", safeDeparse(origIndex), "`.")
                            } else {
                                ## No loop-indexing variables present in the vectorized index.
                                ## This index should be constant for all instances of this nodeFunction.
                                logProbIndexValue <- as.numeric(min(eval(origIndex)))   # eval() should not cause an error...
                                logProbExpr[[i]] <- logProbIndexValue
                            }
                        }
                    }
                }
            } else logProbExpr <- NULL
            return(logProbExpr)
        }        
    )
)


checkForExpr <- function(expr) {
    if(length(expr) == 1 && (inherits(expr, "name") || inherits(expr, "numeric")))
        return(FALSE)
    if(!safeDeparse(expr[[1]], warn = TRUE) == '[')
        return(TRUE)
    ## Recurse only on the first argument of the `[`.
    return(checkForExpr(expr[[2]]))
    ## Previously we recursed more completely.  Now we stop because expressions
    ## inside `[` are allowed.
    ## if(!deparse(expr[[1]]) %in% c('[', ':')) return(TRUE)
    ## for(i in 2:length(expr)) 
    ##     if(checkForExpr(expr[[i]])) output <- TRUE
    ## return(output)
}
