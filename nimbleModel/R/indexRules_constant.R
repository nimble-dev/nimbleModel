indexRuleClass_constant <- R6Class(
    classname = "indexRuleClass_constant",
    inherit = indexRuleClass,
    portable = FALSE,
    public = list(
        setupResults = NULL,
        initialize = function(toIndexExprList,
                              fromIndexExprList,
                              context,
                              constants = list()
                              ) {
            ## Rule only applicable if no RHS indexing since no relationship of RHS to LHS indexing,
            ## so graphRule processing needs to pass in nothing for RHS.
            if(length(fromIndexExprList) || length(context$singleContexts))
                return()
            else 
                setupResults <<-
                    indexRule_constant_setup(toIndexExprList,
                                             fromIndexExprList,
                                             context,
                                             constants
                                             )
        },

        apply_indexRange = function(fromIndexRange, ...) {
            ## Checking of from indexing done in graphRule processing via constraints
            return(setupResults$constant)
        },
        apply = function(from, ...) {
            if(inherits(from, 'varRangeClass'))
                ##apply_varRange(from, ...)
                stop('an index rule should be applied to an indexRange')
            else
                apply_indexRange(from, ...)
        },

        get_max = function() {
            return(NULL)
        }
    )
)


indexRule_constant_setup <- function(toIndexExprList,
                                      fromIndexExprList,
                                      context,
                                      constants = list()
                                      ) {
    if(is.list(constants))
        constants <- list2env(constants)

    ##  only allow a single index slot 
    if(length(toIndexExprList) > 1)
        return(NULL)

    if(!length(toIndexExprList)) {
        toConstant <- indexRange_none()
    } else {
        toIndexExpr <- toIndexExprList[[1]]
        
        if(length(toIndexExpr) == 3 && toIndexExpr[[1]] == ':') {
            toConstant <- indexRange_sequence(list(eval(toIndexExpr[[2]], envir = constants),
                                                eval(toIndexExpr[[3]], envir = constants)))
                                        # resultExpr <- substitute(A:B, list(A = input_range[1], B = input_range[2]))
        } else if(length(toIndexExpr) == 1) {
            toConstant <- indexRange_scalar(eval(toIndexExpr, envir = constants))
        } else stop("indexRule_constant_setup: input error in ", deparse(toIndexExprList))
    }
    return(list(constant = toConstant))
}
