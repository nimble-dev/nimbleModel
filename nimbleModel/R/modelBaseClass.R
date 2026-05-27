#' @export
modelBase_nClass <- nClass(
    classname = "modelBase_nClass",
    Rpublic = list(
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
            nimbleModel::getDependencies(modelDef, nodes, self, downstream, immediateOnly)
        },
        getParents = function(nodes, self = TRUE, upstream = FALSE, immediateOnly = FALSE) {
            nimbleModel::getParents(modelDef, nodes, self, upstream, immediateOnly)
        },
        getNodes = function(nodes, stochOnly = FALSE, determOnly = FALSE,
                     includeData = TRUE, dataOnly = FALSE,
                     includePredictive = TRUE, predictiveOnly = FALSE,
                     includeRHSonly = FALSE,
                     topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE) {
            nimbleModel::getNodes(modelDef, stochOnly, determOnly, includeData, dataOnly,
                     includePredictive, predictiveOnly, includeRHSonly,
                     topOnly, latentOnly, endOnly)
        }, 
        calculate = function(instrList) {
            if(inherits(instrList, 'instr_nClass')) {
              oneInstr <- instrList
              instrList <- nList(instr_nClass)$new()
              instrList$setLength(1)
              instrList[[1]] <- oneInstr
            }
            if(!((inherits(instrList, 'nList') || is.list(instrList)) && inherits(instrList[[1]], 'instr_nClass')))
                instrList <- makeInstrList(self, instrList)
            ## Assume instrList is ordered (it is done `makeInstrList`).
            if(isCompiled())
                return(calculate_impl(instrList))
            logProb <- 0
            for(i in 1:length(instrList)) {
                logProb <- logProb + declFunList[[instrList[[i]]$declID]]$calculate(instrList[[i]])
            }
            return(logProb)
        }
    ),
    Cpublic = list(
        declFunList = 'RcppObject',  # This won't actually be used in C++, but needs to be in Cpublic for accessibility.
        declFunMapping = 'RcppList',  # Not sure what type this should be for use in C++.
        ping = nFunction(
            name = "ping",
            function() {return(TRUE); returnType(logical())},
            compileInfo = list(virtual=TRUE)
        ),
        calculate_impl = nFunction(
            name = "calculate_impl",
            function(instrList) {
                cat("Uncompiled `calculate_impl` should never be called.\n")
                return(0)
            },
            returnType = 'numericScalar',
            compileInfo = list(
                C_fun = function(instrList = 'nList(instr_nClass)') {
                    ## TODO: consider whether instrList will be ordered and/or how C++ will see the decl indexing info.
                    cppLiteral('modelClass_::calculate(instrList);')
                },
                virtual=TRUE
            )
        )
    ),
    ## See comment above about needing to ensure a virtual destructor
    predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |> file.path("modelBase_nC")),
    compileInfo=list(interface="full",
                     createFromR = FALSE,
                     Hincludes = c('"declFunBase_nClass_c_.h"'), 
                     needed_units = list("declFunBase_nClass","instr_nClass"),
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
