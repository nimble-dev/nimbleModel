## A varRule manages a set of rules (graphRules, varRules, topRules, etc.), which together
## comprise all the rules for a variable.

varRuleClass <- R6Class(
    classname = "varRuleClass",
    portable = FALSE,
    public = list(
        rules = NULL,
        varName = character(),
        initialize = function(rules = list()) {
            rules <<- rules
            ## graphRules don't have varName; not clear if we want to check for consistency anyway.
            ## nm <- unique(sapply(rules), function(rule) rule$varName)
            ## if(length(nm) != 1)
            ##    stop("varRuleClass$new: Missing or inconsistent varNames in input list.")
            ## varName <<- nm
        },
        addRules = function(rules = list()) {
            rules <<- c(self$rules, rules)
        },
        apply = function(node) {
            lapply(rules,
                   function(rule) rule$apply(node))
        }
    )
)
