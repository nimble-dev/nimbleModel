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
        makeRules = function(nimFunNames) {
            ## Placeholder to get things going. For now assume `code` is simple cases
            ## that can be handed by declRuleClass initialization.
            declRule <<- declRuleClass$new(code, sourceLineNumber, context, constants)

            makeSymbolicParentNodes(nimFunNames)
            makeGraphRules()
            makeRHSoriginalRules()
        },

        ## Determine RHS pieces.
        makeSymbolicParentNodes = function(nimFunNames) {
            constantsNamesList <- lapply(names(constants), as.name)
            symbolicParentNodes <<-
                unique(
                    getSymbolicParentNodes(valueExpr,
                                           constantsNamesList,
                                           context$indexVarExprs,
                                           nimFunNames)
                ) 
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
        },

        ## Make an initial RHSrule for each RHS piece. 
        makeRHSoriginalRules = function() {
            rhsOriginalRules <<- vector('list',
                                      length(symbolicParentNodes))
            for(i in seq_along(symbolicParentNodes)) {
                rhsOriginalRules[[i]] <<-
                    rhsRuleClass$new(symbolicParentNodes[[i]], NULL, context, constants)
            }
        },

        setIndexVariableExprs = function(exprs) {
            indexVariableExprs <<- exprs
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

