modelDefClass <- R6Class(
    classname = "modelDefClass",
    portable = FALSE,
    public = list(
        modelCode = NULL,
        contexts = list(),
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
            for(i in seq_along(declInfo)) {
                declInfo[[i]]$process()
            }
        },
        initializeContexts = function() {
            contextClassObject <- modelContextClass$new()
            contextClassObject$setup(singleContexts = list())
            contexts[[1]] <<- contextClassObject
        }
    )
)
