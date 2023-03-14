## An originalIndexingRuleClass object represents the relationship
## between the loop indexing and the indexing of a LHS variable,
## such as giving the values of `i` when provided a varRange for `y` in
## `for(i in 1:n) y[i-2] <- 1`


originalIndexingRuleClass <- R6Class(
    classname = "originalIndexingRuleClass",
    portable = FALSE,
    public = list(
        graphRule = NULL,
        varName = character(),
        
        initialize = function(LHS,
                              context,
                              constants = list()) {
            varNameExpr <- ifelse(length(LHS) == 1, LHS, LHS[[2]])
            varName <<- deparse(varNameExpr)
            if(length(context$indexVarNames)) {
                dummyLHS <- parse(text = paste0(varName, "[",
                                                paste(context$indexVarNames, collapse = ","),
                                                "]"))[[1]]
            } else dummyLHS <- varNameExpr
            graphRule <<- graphRuleClass$new(dummyLHS,
                                             LHS,
                                             context,
                                             constants)
        },

        ## Produces a varRange, though it's not really a range for a variable
        ## but rather a range for the indices.
        apply = function(fromVarRange) {
            graphRule$apply(fromVarRange)
        }
    )
)


