## An originalIndexingRuleClass object represents the relationship
## between the loop indexing and the indexing of a LHS variable,
## such as giving the values of `i` when provided a varRange for `y` in
## `for(i in 5:n) y[i-2] <- 1`, such that `y[7:9]` would give `i=9:11`.


originalIndexingRuleClass <- R6Class(
    classname = "originalIndexingRuleClass",
    portable = FALSE,
    public = list(
        graphRule = NULL,
        varName = character(),
        
        initialize = function(LHS,
                              context,
                              constants = list()) {
            varName <<- getVarName(LHS)
            if(length(context$indexVarNames)) {
                ## Exclude indices not used in lifted expression, e.g., `i` in `y[i,j] ~ dnorm(mu[i], var = sigma2[j])`
                indexVarNames <- context$indexVarNames
                indexVarNames <- indexVarNames[indexVarNames %in% all.vars(LHS)]
                indexing <- if(length(indexVarNames))
                                paste0("[", paste(indexVarNames, collapse = ","), "]") else ""
                dummyLHS <- parse(text = paste0(varName, indexing))[[1]]
                ## Unused singleContexts will be removed in graphRuleClass$new().
            } else dummyLHS <- as.name(varName)
            graphRule <<- graphRuleClass$new(dummyLHS,
                                             LHS,
                                             context,
                                             constants)
        },

        ## Produces a varRange, though it's not really a range for a variable
        ## but rather a range for the indices.
        ## Do not remove duplicates because in generation of `calcRange`s there
        ## can be cases where we need duplicated values in order to have correct
        ## number of logProbs.
        apply = function(fromVarRange) {
            graphRule$apply(fromVarRange, removeDuplicates = FALSE)
        }
    )
)


