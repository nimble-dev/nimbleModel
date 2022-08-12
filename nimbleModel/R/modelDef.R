modelDefClass <- R6Class(
    classname = "modelDefClass",
    portable = FALSE,
    public = list(
        modelCode = NULL,
        contexts = list(),
        constants = list(),
        declInfo = list(),
        downstreamRules = NULL,
        upstreamRules = NULL,
        initialize = function(modelCode = NULL, constants = list()) {
            modelCode <<- modelCode
            constants <<- constants
            initializeContexts()
        },
        processModelCode = function() {
            processModelCode_impl(self,
                                  modelCode)
        },
        processDecls = function() {
            ## placeholder so we don't need to invoke all our distribution stuff
            nimFunNames <- list(as.name('dnorm'), as.name('dunif'))
            ## placeholder until we add in constants processing
            for(i in seq_along(declInfo)) {
                declInfo[[i]]$process(constants, nimFunNames)
            }

            ## Collect all declRules, rhsOriginalRules, downstreamRules
            ## into lists we can use in processModelGraph.
            ## Do in a setupModel method?
        },
        initializeContexts = function() {
            contextClassObject <- modelContextClass$new()
            contextClassObject$setup(singleContexts = list())
            contexts[[1]] <<- contextClassObject
        }
    )
)
