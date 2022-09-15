## open issues:

## y[5:7] ~ dmnorm()
## is there an orig index rule?
## if not, how handle finding calc for y[6:8] (i.e, offset/partial)

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
                                                paste(context$indexVarNames, collapse = ","), "]"))[[1]]
                } else dummyLHS <- varNameExpr
            graphRule <<-
                makeGraphRule(dummyLHS,
                                    LHS,
                                    context,
                                    constants)
        },
        apply = function(fromVarRange, varName = NULL) {
            applyGraphRule(
                fromVarRange,
                graphRule,
                varName
            )
        }
    )
)


