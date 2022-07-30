## open issues:

## y[5:7] ~ dmnorm()
## is there an orig index rule?
## if not, how handle finding calc for y[6:8] (i.e, offset/partial)

originalIndexRuleClass <- R6Class(
    classname = "originalIndexRuleClass",
    portable = FALSE,
    public = list(
        graphRule = NULL,
        initialize = function(LHS,
                              context,
                              constants = list()) {
            if(length(context$indexVarNames)) {
                dummyLHS <- parse(text = paste0("w[",
                                                paste(context$indexVarNames, collapse = ","), "]"))[[1]]
                } else dummyLHS <- quote(w)
            graphRule <<-
                makeGraphIndexRules(dummyLHS,
                                    LHS,
                                    context,
                                    constants)
        },
        apply = function(fromVarRange) {
            applyGraphIndexRules(
                fromVarRange,
                graphRule
            )
        }
    )
)


