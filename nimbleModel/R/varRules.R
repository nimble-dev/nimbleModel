## A varRules object holds a set of rules (graphRules, topRules, etc.) that together
## comprise all the rules of that type for a variable.

## varRules will generally be stored as elements of a list, with the element name
## being the variable name, for lookup purposes.

varRulesClass <- R6Class(
    classname = "varRulesClass",
    portable = FALSE,
    public = list(
        rules = list(),
        varName = character(),
        
        initialize = function(rules = list(), varName) {
            rules <<- rules
            ## Probably won't be used but useful for clarity.
            ## For a `graphRule`, this will be the input variable.
            varName <<- varName 
        },
        
        apply = function(node) {
            lapply(rules,
                   function(rule) rule$apply(node))
        }
    )
)

## Take a flat list of rules and divide up into one `varRules` per variable.
newVarRules <- function(items, varNames = NULL, type = NULL) {
    if(!all(sapply(items, function(x) inherits(x, "nodeRuleClass") || is(x, "graphRuleClass"))))
        stop("all elements of `items` must be `nodeRule`s or `graphRule`s.")
    if(is.null(varNames)) {
        varNames <- sapply(items, function(item) item$varName)
    } else 
        if(length(varNames) != length(items))
            stop("length of `varNames` must match length of `items`.")
    if(!is.null(type)) {
        include <- sapply(items, function(item) item$isOfType(type))
    } else include <- rep(TRUE, length(items))
    uniqVarNames <- unique(varNames)
    if(inherits(items[[1]], 'varRangeClass')) {
        ## TODO: why would we ever have a varRule of varRanges?
        stop("input cannot be a `varRange`.")
        result <- lapply(uniqVarNames, function(nm)
            items[varNames == nm & include])
    } else  
        result <- lapply(uniqVarNames, function(nm)
            varRulesClass$new(items[varNames == nm & include], varName = nm))
    names(result) <- uniqVarNames
    return(result)
}

## Utility for determining which `varRules` is needed for a node and
## applying it.
applyRules <- function(rules, node) {
    varName <- getVarName(node)  
    if(varName %in% names(rules)) {
        return(rules[[varName]]$apply(node))
    } else return(NULL)
}
