## Modified from old BUGSdeclClass

## modelDeclClass replaces BUGSdeclClass.
## There are components not yet moved from current nimble that will be needed.
## I think these will include targetIndexNamePieces and parentIndexNamePieces.
## I don't think these will include the various *replacements*.
modelDeclClass <- R6Class(
    classname = 'modelDeclClass',
    portable = FALSE,
    public = list(
        ## In current nimble we record contextID here.
        ## We really need the context itself,
        ## so I switched to recording that.
        context = NULL,
        sourceLineNumber = NULL,
        code = NULL,
        type = NULL,
        valueExpr = NULL,
        targetExpr = NULL,
        transExpr = NULL,
        indexExpr = NULL,
        targetVarExpr = NULL,
        targetNodeExpr = NULL,
        targetVarName = NULL,
        targetNodeName = NULL, 
        truncated = NULL,
        boundExprs = NULL,
        symbolicParentNodes = NULL,
        downstreamRules = NULL,
        upstreamRules = NULL,
        rhsOriginalRules = NULL,
        declRule = NULL,  # placeholder that modelDecl contains the declRule
        ## TODO: clean this up and determine relationship between modelDecl and declRule
        
        setup = function(code,
                         context,
                         constants = list(),
                         sourceLineNum,
                         truncated = FALSE,
                         boundExprs = NULL) {
            modelDeclClass_setup(self,
                                 code,
                                 context,
                                 sourceLineNum,
                                 truncated,
                                 boundExprs)
            ## Placeholder to get things going. For now assume 'code' is simple cases
            ## that can be handed by declRuleClass initialization.
            declRule <<- declRuleClass$new(code, sourceLineNum, context, constants)
        },
        process = function(constants, nimFunNames) {
            genSymbolicParentNodes(constants, nimFunNames)
            makeGraphRules(constants)
            makeRHSoriginalRules(constants)
        },
        genSymbolicParentNodes = function(constants,
                                          nimFunNames) {
            constantsNamesList <<- lapply(ls(constants), as.name)
            indexVarExprs <- if(is.null(context))
                                 list()
                             else
                                 context$indexVarExprs
            symbolicParentNodes <<-
                unique(
                    getSymbolicParentNodes(valueExpr,
                                           constantsNamesList,
                                           indexVarExprs,
                                           nimFunNames)
                ) 
        },
        makeGraphRules = function(constants) {
            if(is.null(symbolicParentNodes))
                genSymbolicParentNodes(constants,
                                       context$indexVarExprs)
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
                                       declRule$stoch)
                upstreamRules[[i]] <<-
                    graphRuleClass$new(symbolicParentNodes[[i]],
                                       targetNodeExpr,
                                       context,
                                       constants,
                                       declRule$stoch)
                
            }
        },

        makeRHSoriginalRules = function(constants) {
            if(is.null(symbolicParentNodes))
                genSymbolicParentNodes(constants,
                                       context$indexVarExprs)
            rhsOriginalRules <<- vector('list',
                                      length(symbolicParentNodes))
            for(i in seq_along(symbolicParentNodes)) {
                rhsOriginalRules[[i]] <<-
                    rhsRuleClass$new(symbolicParentNodes[[i]], i, context, constants)
            }
        }
    )
)

modelDeclClass_setup <- function(modelDecl,
                                 code,
                                 context,
                                 sourceLineNum,
                                 truncated = FALSE,
                                 boundExprs = NULL) {
    ## Argument 'context' is used to set field: context.
    ## Argument 'code' is used to set the fields:
    ##  code
    ##  targetExpr, valueExpr
    ##  targetVarExpr, targetNodeExpr
    ##  targetVarName, targetNodeName
        
    modelDecl$context <- context
    modelDecl$sourceLineNumber <- sourceLineNum
    modelDecl$code <- code
    modelDecl$truncated <- truncated
    modelDecl$boundExprs <- boundExprs
    
    if(code[[1]] == '~') {
        modelDecl$type <- 'stoch'
        
        if(!is.call(code[[3]]) ||
           (!any(code[[3]][[1]] == getAllDistributionsInfo('namesVector')) &&
            code[[3]][[1]] != "T" &&
            code[[3]][[1]] != "I"))
            stop(
                paste0('Improper syntax for stochastic declaration: ',
                       deparse(code))
            )
    } else if(code[[1]] == '<-') {
        modelDecl$type <- 'determ'
    } else {
        stop(paste0('Improper syntax for declaration: ',
                    deparse(code))
             )
    }
    
    targetExpr <- code[[2]]
    valueExpr <- code[[3]]
    
    transExpr <- NULL
    indexExpr <- NULL
        
    if(length(targetExpr) > 1) {
        ## There is a tranformation and/or a subscript
        if(targetExpr[[1]] == '[') {
            ## It is a subscript only
            indexExpr <- as.list(targetExpr[-c(1,2)]) 
            targetVarExpr <- targetExpr[[2]]
            targetNodeExpr <- targetExpr
        } else {
            ## There is a transformation, possibly with a subscript
            transExpr <- targetExpr[[1]]
            targetNodeExpr <- targetExpr[[2]]
            if(length(targetNodeExpr)>1) {
                ## There are subscripts inside the transformation
                if(targetNodeExpr[[1]] != '[') {
                    print(paste("Invalid subscripting for",
                                deparse(targetExpr)))
                }
                indexExpr <- as.list(targetNodeExpr[-c(1,2)])
                targetVarExpr <- targetNodeExpr[[2]]
            } else {
                targetVarExpr <- targetNodeExpr
            }
        }
    } else {
        ## no tranformation or subscript
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
}

