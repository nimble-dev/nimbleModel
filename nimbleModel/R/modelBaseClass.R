#' @export
modelBase_nClass <- nClass(
    classname = "modelBase_nClass",
    Rpublic = list(
        ## TODO: bring in methods and fields from nimbleModel:::modelClass.
        modelDef = NULL,
        dataRules = NULL,
        nondataRules = NULL,
        predictiveRules = NULL,
        nonpredictiveRules = NULL,
        getVarNames = function(includeLogProb = FALSE, nodeRanges) {
            if(missing(nodeRanges)){
                if(includeLogProb) return(modelDef$varNames)
                else return(names(modelDef$varInfo))
            } else {
                if(!is.list(nodeRanges))
                    nodeRanges <- list(nodeRanges)
                return(unique(sapply(nodeRanges, `[[`, 'varName')))
            }
        },
        getDependencies = function(nodes, self = TRUE, downstream = FALSE, immediateOnly = FALSE) {
            nimbleModel:::getDependencies(modelDef, nodes, self, downstream, immediateOnly)
        }
    ),
    Cpublic = list(
        declList = 'nList(declFxnBase_nClass)',
        ping = nFunction(
            name = "ping",
            function() {return(TRUE); returnType(logical())},
            compileInfo = list(virtual=TRUE)
        ),
        calculate = nFunction(
            ## TODO: What is the difference between having this as Cpublic with separate C_fun and having in R_public?
            name = "calculate",
            function(instrList) {
                cat("In uncompiled calculate\n")
                if(inherits(instrList, 'instr_nClass'))
                    instrList <- list(instrList)
                if(FALSE) {
                   ## TODO: self is a Cpub_uncompiled obj, not full specialized model class.
                   ## So this doesn't work as we need self$modelDef in `makeInstrList()`.
                if(!(is.list(instrList) && inherits(instrList[[1]], 'instr_nClass')))
                    instrList <- makeInstrList(self, instrList)
                }
                logProb <- 0
                ord <- order(unlist(lapply(instrList, function(x) x$sortID)))
                ## This is where uncompiled stepping through the calcInstrList happens.
                for(i in 1:length(ord)) {
                    ## TODO: need to sort out this lookup process.
                    ## nodeIdx <- instr$declID
                    ## nodemember_name <- self$nodeObjNames[nodeIdx] # nodeObjNames is found in the derived class
                    logProb <- logProb + declList[[instrList[[ord[i]]]$declID]]$calculate(instrList[[ord[i]]])
                }
                return(logProb)
            },
            returnType = 'numericScalar',
            compileInfo = list(
                C_fun = function(instrList='nList(instr_nClass)') {
                    logProb <- 0
                    ## For now assuming instructions are in order.
                    for(i in 1:length(instrList)) {
                        ## nodemember_name <- self$nodeObjNames[instrList[[i]]$declID]
                        logProb <- logProb + declList[[instrList[[i]]$declID]]$calculate(instrList[[i]])
                    }
                    return(logProb)
                },
                virtual=TRUE
            )
        )
    ),
    ## See comment above about needing to ensure a virtual destructor
    predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |> file.path("modelBase_nC")),
    compileInfo=list(interface="full",
                     createFromR = FALSE,
                     Hincludes = c('"declFxnBase_nClass_c_.h"'), #, '"calcInstrList_nClass_c_.h"'), # "declFxnBase_nClass_c_.h" needed for package = TRUE
                     needed_units = list("declFxnBase_nClass","instr_nClass"),
                     exportName = "modelBase_nClass_new",
                     packageNames = c(uncompiled="modelBase_nClass_R", compiled="modelBase_nClass")
                     )
)

# Manually add
# # "#include <nCompiler/predef/modelClass_/modelClass_.h>" to that file,
# after the header content.


# nCompile(modelBase_nClass, control=list(generate_predefined=TRUE))

## The two "addModelDollarSign" functions are borrowed directly from nimble.
## This should add model$ in front of any names that are not already part of a '$' expression
