#' @export
modelBase_nClass <- nClass(
    classname = "modelBase_nClass",
    Rpublic = list(
        modelDef = NULL,
        dataRules = NULL,
        nondataRules = NULL,
        predictiveRules = NULL,
        nonpredictiveRules = NULL,
        initialize = function(sizes = list(), inits = list(), data = list()) {
           # It is not very easy to set debug onto the initialize function, so
           # here is a magic flag.
           if(isTRUE(.GlobalEnv$.debugModelInit)) browser()
            super$initialize()

            ## TODO: is there a better way to populate declFunNameToIndex in Cpublic?
            declFunNameToIndex <- self$declFunNameToIndex_

            declFunNames <- names(declFunNameToIndex)  
            if(isCompiled()) {
                self$setup_decl_mgmt_from_names(declFunNames)
            } else {
                self$declFunList <- list()
                length(self$declFunList) <- length(declFunNames)
                names(self$declFunList) <- declFunNames
                for(declFunName in declFunNames) {
                    self[[declFunName]] <- eval(as.name(self$CpublicDeclFuns[[declFunName]]))$new()
                    self[[declFunName]]$setModel(self)
                    self$declFunList[[declFunNameToIndex[[declFunName]]]] <- self[[declFunName]]
                }
            }

            ## TODO: create a merge_and_set function that handles all three of the following.
            allSizes <- self$defaultSizes
            if(!missing(sizes))
                for(nm in names(sizes))
                    allSizes[[nm]] <- sizes[[nm]]
            ## TODO: should we handle 0-dim sizes elsewhere?
            allSizes <- allSizes[sapply(allSizes, length) > 0]
            if(length(allSizes)) resize_from_list(allSizes[sapply(allSizes, length) > 0])
            
            allInits <- self$defaultInits
            if(!missing(inits))
                for(nm in names(inits))
                    allInits[[nm]] <- inits[[nm]]
            if(length(allInits)) set_from_list(allInits)
            
            if(missing(inits)) {
                allInits <- self$defaultInits
            } else 
                if(length(inits)) set_from_list(inits)
            
            ## TODO: do we want to handle data differently?
            ## TODO: need to work through not setting as 'data' if values are NA;
            ##   check back against how dataRules work in nimbleModel work.
            allData <- self$defaultData
            if(!missing(inits))
                for(nm in names(inits))
                    allData[[nm]] <- inits[[nm]]
            if(length(allData)) set_from_list(allData)
        },
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
        ## TODO: not working because `nimbleModel::getNodes` needs the model not just modelDef.
        ## Once we integrate modelClass with modelBase_nClass, we should be able to pass `self`.
        getNodes = function(nodes = NULL, stochOnly = FALSE, determOnly = FALSE,
                     includeData = TRUE, dataOnly = FALSE,
                     includePredictive = TRUE, predictiveOnly = FALSE,
                     includeRHSonly = FALSE,
                     topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE) {
            nimbleModel::getNodes(modelDef, nodes, stochOnly, determOnly, includeData, dataOnly,
                     includePredictive, predictiveOnly, includeRHSonly,
                     topOnly, latentOnly, endOnly)
        }, 
        calculate = function(instrList) {
            if(missing(instrList))
              instrList <- getVarNames()
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
        },
        simulate = function(instrList) {
            if(missing(instrList))
              instrList <- getVarNames()
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
                return(simulate_impl(instrList))
            for(i in 1:length(instrList)) {
                declFunList[[instrList[[i]]$declID]]$simulate(instrList[[i]])
            }
        }
    ),
    Cpublic = list(
        ## TODO: using 'RcppObject' was resulting in a symbolTBD error - probably nCompiler issue 186.
        declFunList = 'numericScalar', # 'RcppObject',  # This won't actually be used in C++, but needs to be in Cpublic for accessibility.
        declFunNameToIndex = 'RcppList',  # Not sure what type this should be for use in C++.
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
                    ## NOTE: instrList input will be ordered.
                    cppLiteral('modelClass_::calculate(instrList);')
                },
                virtual=TRUE
            )
        ), 
        simulate_impl = nFunction(
            name = "simulate_impl",
            function(instrList) {
                cat("Uncompiled `simulate_impl` should never be called.\n")
                return(0)
            },
            returnType = 'numericScalar',
            compileInfo = list(
                C_fun = function(instrList = 'nList(instr_nClass)') {
                    cppLiteral('modelClass_::simulate(instrList);')
                },
                virtual=TRUE
            )
        )
    ),
    ## See comment above about needing to ensure a virtual destructor
    predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |> file.path("modelBase_nC")),
    compileInfo=list(interface="full",
                     createFromR = FALSE,
                     Hincludes = c('"declFunBase_nClass_c_.h","instr_nClass_c_.h"'), 
                     needed_units = list("declFunBase_nClass","instr_nClass"),
                     exportName = "modelBase_nClass_new",
                     packageNames = c(uncompiled="modelBase_nClass_R", compiled="modelBase_nClass")
                     )
)

# Manually add
# # "#include <nCompiler/predef/modelClass_/modelClass_.h>" to that file,
# after the header content.


# nCompile(modelBase_nClass, control=list(generate_predefined=TRUE))

