## class for single rules
indexRuleClass <- R6Class(
    classname = 'indexRuleClass',
    portable = FALSE,
    public = list(
        name = character(),
        dimTo = integer()
    )
)
