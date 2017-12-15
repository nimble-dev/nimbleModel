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
        },
        processModelCode = function() {
            processModelCode_impl(self,
                                  modelCode)
        },
        processDecls = function() {
            for(i in seq_along(declInfo)) {
                declInfo[[i]]$process()
            }
        }
    )
)
