## A varRule holds a set of rules (graphRules, varRules, topRules, etc.) that together
## comprise all the rules of that type for a variable.

## varRules will generally be stored as elements of a list, with the element name
## being the variable name, for lookup purposes.

varRuleClass <- R6Class(
    classname = "varRuleClass",
    portable = FALSE,
    public = list(
        rules = list(),
        varName = character(),
        
        initialize = function(rules = list(), name) {
            rules <<- rules
            ## Probably won't be used but useful for clarity.
            ## For a `graphRule`, this will be the input variable.
            varName <<- name 
        },
        
        apply = function(node) {
            lapply(rules,
                   function(rule) rule$apply(node))
        }
    )
)

## Take a flat list of `nodeRule`s and divide up into one `varRule` per variable.
createVarRule <- function(items, varNames = NULL, type = NULL) {
    if(!all(sapply(items, function(x) is(x, "nodeRuleClass"))))
        stop("createVarRule: all elements of `items` must be `nodeRule`s.")
    if(is.null(varNames)) {
        varNames <- sapply(items, function(item) item$varName)
    } else 
        if(length(varNames) != length(items))
            stop("createVarRule: length of `varNames` must match length of `items`.")
    if(!is.null(type)) {
        include <- sapply(items, function(item) item$is_type(type))
    } else include <- rep(TRUE, length(items))
    uniqVarNames <- unique(varNames)
    if(is(items[[1]], 'varRangeClass')) {
        ## TODO: why would we ever have a varRule of varRanges?
        browser()
        result <- lapply(uniqVarNames, function(nm)
            items[varNames == nm & include])
    } else  
        result <- lapply(uniqVarNames, function(nm)
            varRuleClass$new(items[varNames == nm & include]), varName = nm)
    names(result) <- uniqVarNames
    return(result)
}

