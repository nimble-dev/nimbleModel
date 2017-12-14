## Modified from old BUGSdeclClass

modelDeclClass <- R6Class(
    classname = 'modelDeclClass',
    portable = FALSE,
    public = list(
        contextID = NULL,
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
        setup = function(code,
                         contextID,
                         sourceLineNum,
                         truncated = FALSE,
                         boundExprs = NULL) {
            modelDeclClass_setup(self,
                                 code,
                                 contextID,
                                 sourceLineNum,
                                 truncated,
                                 boundExprs)
        },
        genSymbolicParentNodes = function(constantsNamesList,
                                          context,
                                          nimFunNames) {
    ## sets the field symbolicparentNodes
            symbolicParentNodes <<-
                unique(
                    getSymbolicParentNodes(valueExpr,
                                           constantsNamesList,
                                           context$indexVarExprs,
                                           nimFunNames,
                                           contextID = contextID)
                ) 
        },
        makeDownstreamRules = function() {
            
        }
    )
)

getAllDistributionsInfo <- function(...) {
    message('Set up getAllDistributionsInfo')
    'none'
}

modelDeclClass_setup <- function(modelDecl,
                                 code,
                                 contextID,
                                 sourceLineNum,
                                 truncated = FALSE,
                                 boundExprs = NULL) {
    ## Argument 'contextID' is used to set field: contextID.
    ## Argument 'code' is used to set the fields:
    ##  code
    ##  targetExpr, valueExpr
    ##  targetVarExpr, targetNodeExpr
    ##  targetVarName, targetNodeName
        
    modelDecl$contextID <- contextID
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

