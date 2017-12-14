## A varRule manages a set of graphRules, which together
## comprise all the edges for a variable.

varRuleClass <- R6Class(
    classname = "varRuleClass",
    portable = FALSE,
    public = list(
        graphRules = NULL,
        initialize = function(graphRules = list()) {
            graphRules <<- graphRules
        },
        addRules = function(graphRules = list()) {
            graphRules <<- c(self$graphRules, graphRules)
        },
        apply = function(fromVarRange) {
            lapply(graphRules,
                   function(x) x$apply(fromVarRange))
        }
    )
)
