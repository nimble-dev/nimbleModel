modelDefClass <- R6Class(
    classname = "modelDefClass",
    portable = FALSE,
    public = list(
        modelCode = NULL,
        contexts = list(),
        constantsNamesList = list(),
        declInfo = list(),
        downstreamRules = NULL,
        upstreamRules = NULL,
        initialize = function(modelCode = NULL) {
            modelCode <<- modelCode
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
                declInfo[[i]]$process(constantsNamesList, nimFunNames)
            }
        },
        initializeContexts = function() {
            contextClassObject <- modelContextClass$new()
            contextClassObject$setup(singleContexts = list())
            contexts[[1]] <<- contextClassObject
        }
    )
)
