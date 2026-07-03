#' @export
modelBase_nClass <- nClass(
  classname = "modelBase_nClass",
  Rpublic = list(
    modelDef = NULL,
    dataRules = NULL,
    nondataRules = NULL,
    predictiveRules = NULL,
    nonpredictiveRules = NULL,
    initialize = function(sizes = list(), inits = list(), data = list(), ...) {
      # It is not very easy to set debug onto the initialize function, so
      # here is a magic flag.
      if (isTRUE(.GlobalEnv$.debugModelInit)) browser()
      super$initialize(...)

      # TODO: is there a better way to populate declFunNameToIndex in Cpublic?
      declFunNameToIndex <- self$declFunNameToIndex_

      declFunNames <- names(declFunNameToIndex)
      if (isCompiled()) {
        # self$setup_decl_mgmt_from_names(declFunNames)
        # setting up the canonically indexed vector of node functions
        # now happens in the C++ constructor.
      } else {
        self$declFunList <- list()
        length(self$declFunList) <- length(declFunNames)
        names(self$declFunList) <- declFunNames
        for (declFunName in declFunNames) {
          self[[declFunName]] <- eval(as.name(self$CpublicDeclFuns[[declFunName]]))$new()
          self[[declFunName]]$setModel(self)
          self$declFunList[[declFunNameToIndex[[declFunName]]]] <- self[[declFunName]]
        }
      }

      # TODO: create a merge_and_set function that handles all three of the following.
      allSizes <- self$defaultSizes
      if (!missing(sizes)) {
        for (nm in names(sizes)) {
          allSizes[[nm]] <- sizes[[nm]]
        }
      }
      # TODO: should we handle 0-dim sizes elsewhere?
      allSizes <- allSizes[sapply(allSizes, length) > 0]
      if (length(allSizes)) resize_from_list(allSizes[sapply(allSizes, length) > 0])

      allInits <- self$defaultInits
      if (!missing(inits)) {
        for (nm in names(inits)) {
          allInits[[nm]] <- inits[[nm]]
        }
      }
      if (length(allInits)) set_from_list(allInits)

      if (missing(inits)) {
        allInits <- self$defaultInits
      } else if (length(inits)) set_from_list(inits)

      # TODO: do we want to handle data differently?
      # TODO: need to work through not setting as 'data' if values are NA;
      #   check back against how dataRules work in nimbleModel work.
      allData <- self$defaultData
      if (!missing(data)) {
        for (nm in names(data)) {
          allData[[nm]] <- data[[nm]]
        }
      }
      if (length(allData)) set_from_list(allData)

      dataVarIndices <- names(modelDef$constants) %in% modelDef$varNames & !names(modelDef$constants) %in% names(allData) # don't overwrite anything in 'allData'
      # TODO: revise messaging below using new nimbleModel messaging system.
      if (sum(names(modelDef$constants) %in% names(allData))) {
        messageIfVerbose("  [Note] Found the same variable(s) in both 'data' and 'constants'; using variable(s) from 'data'.\n")
      }
      if (sum(dataVarIndices)) {
        allData <- c(allData, modelDef$constants[dataVarIndices])
        messageIfVerbose("  [Note] Adding '", paste(names(modelDef$constants)[dataVarIndices], collapse = ", "), "' as data for building model.")
      }
      makeDataRules(allData)
      makePredictiveRules()
    },
    makeDataRules = function(data) {
      nms <- names(data)
      dataRules <- lapply(seq_along(nms), function(i) {
        varRulesClass$new(newDataRules(data[[i]], nms[i]), varName = nms[i])
      })
      self$dataRules <- dataRules[!sapply(dataRules, function(oneVarRules) is.null(oneVarRules$rules))]
      names(self$dataRules) <- sapply(self$dataRules, `[[`, "varName")
      nondataRules <- lapply(seq_along(nms), function(i) {
        varRulesClass$new(newDataRules(data[[i]], nms[i], nondata = TRUE), varName = nms[i])
      })
      self$nondataRules <- nondataRules[!sapply(nondataRules, function(oneVarRules) is.null(oneVarRules$rules))]
      names(self$nondataRules) <- sapply(self$nondataRules, `[[`, "varName")
    },

    # Start from ranges based on dataRules and walk upwards,
    # excluding any parents of such ranges.
    # For now, this doesn't use any notion of "touched", so there is duplicative
    # walking up the tree for nodes with multiple data descendants, but given
    # graph processing is declaration-based, this doesn't seem like an efficiency
    # concern, particularly since `candidateRules` will progressively shrink.
    makePredictiveRules = function() {
      # predictive rules
      candidateRules <- unlist(lapply(modelDef$calcRules, function(oneVarRules) {
        stoch <- sapply(oneVarRules$rules, function(rule) rule$declRule$decl$stoch)
        return(oneVarRules$rules[stoch])
      })) # `unlist` removes length-0 entries.
      candidateRules <- newVarRules(candidateRules)

      dataRanges <- unlist(lapply(dataRules, function(oneVarDataRules) {
        lapply(oneVarDataRules$rules, function(dataRule) {
          dataRule$rule$apply(dataRule$varName)
        })
      }))
      self$predictiveRules <- excludeFromPredictiveRules(modelDef, dataRanges, candidateRules)

      # nonpredictive rules
      candidateRules <- unlist(lapply(modelDef$calcRules, function(oneVarRules) {
        stoch <- sapply(oneVarRules$rules, function(rule) rule$declRule$decl$stoch)
        return(oneVarRules$rules[stoch])
      })) # `unlist` removes length-0 entries.
      candidateRules <- newVarRules(candidateRules)

      for (oneVarPredictiveRules in predictiveRules) {
        for (predictiveRule in oneVarPredictiveRules$rules) {
          predictiveRange <- predictiveRule$fullRange
          varName <- predictiveRule$varName
          tmp <- unlist(lapply(candidateRules[[varName]]$rules, exclude, predictiveRange))
          tmp <- tmp[!sapply(tmp, is.null)]
          if (length(tmp)) {
            candidateRules[[varName]] <- varRulesClass$new(tmp, varName)
          } else {
            candidateRules[[varName]] <- NULL
          }
        }
      }

      self$nonpredictiveRules <- candidateRules
    },
    getVarNames = function(includeLogProb = FALSE, nodeRanges) {
      if (missing(nodeRanges)) {
        if (includeLogProb) {
          return(modelDef$varNames)
        } else {
          return(names(modelDef$varInfo))
        }
      } else {
        if (!is.list(nodeRanges)) {
          nodeRanges <- list(nodeRanges)
        }
        return(unique(sapply(nodeRanges, `[[`, "varName")))
      }
    },

    # TODO: these various methods were in nimble::modelBaseClass but they don't
    # need to be part of model class since the declaration info is embedded in the input nodeRanges arg.
    # Think more about this.
    getDistribution = function(nodeRanges) {
      if (!is.list(nodeRanges)) {
        nodeRanges <- list(nodeRanges)
      }
      if (!all(sapply(nodeRanges, inherits, "nodeRangeClass"))) {
        stop("getDistribution: argument must be a `nodeRange` or list of `nodeRange`s")
      }
      RHSonly <- sapply(nodeRanges, function(x) is.null(x$decl))
      result <- rep(NA, length(RHSonly))
      result[!RHSonly] <- sapply(nodeRanges[!RHSonly], function(x) x$decl$distributionName)
      return(result)
    },
    getDimension = function(nodeRange, params = NULL, valueOnly = is.null(params) &&
                              !includeParams, includeParams = !is.null(params)) {
      if (!inherits(nodeRange, "nodeRangeClass")) {
        stop("getDimension: argument must be a `nodeRange`")
      }
      if (is.null(nodeRange$decl)) {
        return(NA)
      } # RHSonly
      return(nimbleModel:::getDimension(nodeRange$decl$distributionName, params, valueOnly, includeParams))
    },
    isStoch = function(nodeRanges) {
      if (!is.list(nodeRanges)) {
        nodeRanges <- list(nodeRanges)
      }
      if (!all(sapply(nodeRanges, inherits, "nodeRangeClass"))) {
        stop("isStoch: argument must be a `nodeRange` or list of `nodeRange`s")
      }
      RHSonly <- sapply(nodeRanges, function(x) is.null(x$decl))
      result <- rep(FALSE, length(RHSonly))
      result[!RHSonly] <- sapply(nodeRanges[!RHSonly], function(x) x$decl$stoch)
      return(result)
    },
    isDeterm = function(nodeRanges) {
      if (!is.list(nodeRanges)) {
        nodeRanges <- list(nodeRanges)
      }
      if (!all(sapply(nodeRanges, inherits, "nodeRangeClass"))) {
        stop("isDeterm: argument must be a `nodeRange` or list of `nodeRange`s")
      }
      RHSonly <- sapply(nodeRanges, function(x) is.null(x$decl))
      result <- rep(FALSE, length(RHSonly))
      result[!RHSonly] <- sapply(nodeRanges[!RHSonly], function(x) !x$decl$stoch)
      return(result)
    },
    isDiscrete = function(nodeRanges) {
      if (!is.list(nodeRanges)) {
        nodeRanges <- list(nodeRanges)
      }
      if (!all(sapply(nodeRanges, inherits, "nodeRangeClass"))) {
        stop("isDiscrete: argument must be a `nodeRange` or list of `nodeRange`s")
      }
      RHSonly <- sapply(nodeRanges, function(x) is.null(x$decl))
      result <- rep(NA, length(RHSonly))
      result[!RHSonly] <- sapply(nodeRanges[!RHSonly], function(x) nimbleModel:::isDiscrete(x$decl$distributionName))
      return(result)
    },
    isMultivariate = function(nodeRanges) {
      if (!is.list(nodeRanges)) {
        nodeRanges <- list(nodeRanges)
      }
      if (!all(sapply(nodeRanges, inherits, "nodeRangeClass"))) {
        stop("isMultivariate: argument must be a `nodeRange` or list of `nodeRange`s")
      }
      stoch <- isStoch(nodeRanges)
      result <- rep(NA, length(nodeRanges))
      result[stoch] <- sapply(nodeRanges[stoch], function(x) getValueDim(getDistributionInfo(x$decl$distributionName)) > 0)
      return(result)
    },
    isBinary = function(nodeRanges) {
      if (!is.list(nodeRanges)) {
        nodeRanges <- list(nodeRanges)
      }
      if (!all(sapply(nodeRanges, inherits, "nodeRangeClass"))) {
        stop("isBinary: argument must be a `nodeRange` or list of `nodeRange`s")
      }

      result <- rep(NA, length(nodeRanges))
      stoch <- isStoch(nodeRanges)
      dists <- getDistribution(nodeRanges[stoch])
      binary <- rep(FALSE, length(dists))
      binary[dists == "dbern"] <- TRUE
      binomInds <- which(dists == "dbin")
      if (length(binomInds)) {
        tmp <- sapply(binomInds, function(ind) getParamExpr(nodeRanges[stoch][[ind]], "size") == 1)
        binary[binomInds[tmp]] <- TRUE
      }
      result[stoch] <- binary
      return(result)
    },
    isTruncated = function(nodeRanges) {
      if (!is.list(nodeRanges)) {
        nodeRanges <- list(nodeRanges)
      }
      if (!all(sapply(nodeRanges, inherits, "nodeRangeClass"))) {
        stop("isTruncated: argument must be a `nodeRange` or list of `nodeRange`s")
      }
      RHSonly <- sapply(nodeRanges, function(x) is.null(x$decl))
      result <- rep(NA, length(RHSonly))
      result[!RHSonly] <- sapply(nodeRanges[!RHSonly], function(x) x$decl$truncated)
      return(result)
    },
    isData = function(nodeRanges, reduceToScalar = FALSE) {
      # Returns list or vector of boolean indicators of being data, at the node level.
      # Handles either character string input, as in nimble, or list of node/varRanges. 
      # As with nimble, if any element of a multivariate node is data, then the whole node is flagged as data.
      returnList <- FALSE
      if (inherits(nodeRanges, "varRangeClass"))
        nodeRanges <- list(nodeRanges)
      if (is.list(nodeRanges)) {
        returnList <- TRUE
        nodeRanges <- flatten(lapply(nodeRanges, \(x)
                                    if(inherits(x, 'varRangeClass')) getNodes(x, includeRHSonly = TRUE) else x))
      }
      if (is.character(nodeRanges)) 
        nodeRanges <- getNodes(nodeRanges, includeRHSonly = TRUE)
      if(!(is.list(nodeRanges) && all(sapply(nodeRanges, inherits, "nodeRangeClass"))))
          stop("isData: argument must be a character vector, a `nodeRange` or `varRange` or list of `nodeRange`s")

      allElements <- lapply(nodeRanges, \(v) v$toNodeChars())
      
      dataElements <- lapply(allElements, \(listItem) 
                             unlist(lapply(listItem, \(x) {
                               if (getVarName(x) %in% names(dataRules)) {
                                 unlist(lapply(dataRules[[getVarName(x)]]$apply(x),
                                               \(v) if(is.null(v)) v else x))  # Use `x` not `v` as any data elements mean whole node is data by old nimble handling.
                               } else NULL
                             })))
      
      isData <- lapply(seq_along(allElements), \(i) {
        result <- rep(FALSE, length(allElements[[i]]))
        names(result) <- allElements[[i]]
        result[allElements[[i]] %in% dataElements[[i]]] <- TRUE
        if (reduceToScalar) {
          numData <- sum(result)
          if (numData == length(result)) result <- TRUE
          if (numData == 0) result <- FALSE
        }
        return (result)
      })
      if (length(isData) == 1)
        isData <- isData[[1]]

      if(!returnList) {
        isData <- unlist(isData)
        if(reduceToScalar) {
          numData <- sum(isData)
          if (numData == length(isData)) isData <- TRUE
          if (numData == 0) isData <- FALSE
        }
      }
      return(isData)
    },
    
    # Returns the expr corresponding to 'param' in the distribution of `nodeRange`.
    getParamExpr = function(nodeRange, param) {
      if (!inherits(nodeRange, "nodeRangeClass")) {
        stop("getParamExpr: argument `nodeRange` must be a `nodeRange` object")
      }
      decl <- nodeRange$decl
      if (!decl$stoch) stop("getParamExpr: `nodeRange` must be stochastic")
      if (param %in% names(decl$valueExpr)) {
        expr <- decl$valueExpr[[param]]
      } else if (param %in% names(decl$altParamExprs)) {
        expr <- decl$altParamExprs[[param]]
      } else {
        stop("getParamExpr: `", param, "` is not present in the parameterization")
      }
      if (length(expr) > 1) {
        # Substitute original index values into the expression.
        indexVarRange <- decl$declRule$originalIndexingRule$apply(nodeRange)
        indexValues <- indexVarRange$indexRangeExprs
        names(indexValues) <- decl$context$indexVarNames
        expr <- eval(substitute(substitute(EXPR, indexValues), list(EXPR = expr)))
        return(evalNumeric(expr))
      } else {
        return(expr)
      }
    },

    # Returns the entire RHS valueExpr for `nodeRange`.
    getValueExpr = function(nodeRange) {
      if (!inherits(nodeRange, "nodeRangeClass")) {
        stop("getValueExpr: argument must be a `nodeRange`")
      }
      decl <- nodeRange$decl
      expr <- decl$valueExpr
      if (length(expr) > 1) {
        # First get canonical parameterization for stoch cases.
        if (decl$stoch) {
          expr <- expr[!names(expr) %in% c("lower_", "upper_") &
            !grepl("^\\.", names(expr))]
        }
        # Substitute original index values into the expression.
        indexVarRange <- decl$declRule$originalIndexingRule$apply(nodeRange)
        indexValues <- indexVarRange$indexRangeExprs
        names(indexValues) <- decl$context$indexVarNames
        expr <- eval(substitute(substitute(EXPR, indexValues), list(EXPR = expr)))
        return(evalNumeric(expr))
      } else {
        return(expr)
      }
    },
    getDependencies = function(nodes, self = TRUE, downstream = FALSE, immediateOnly = FALSE,
                               nodesAsChars = getNimbleModelOption('nodesAsChars'),
                               returnScalarComponents = FALSE
                               ) {
      nimbleModel::getDependencies(modelDef, nodes, self, downstream, immediateOnly,
                                   nodesAsChars, returnScalarComponents)
    },
    getParents = function(nodes, self = TRUE, upstream = FALSE, immediateOnly = FALSE,
                          nodesAsChars = getNimbleModelOption('nodesAsChars'),
                          returnScalarComponents = FALSE
                          ) {
      nimbleModel::getParents(modelDef, nodes, self, upstream, immediateOnly,
                              nodesAsChars, returnScalarComponents)
    },
    # TODO: not working because `nimbleModel::getNodes` needs the model not just modelDef.
    # Once we integrate modelClass with modelBase_nClass, we should be able to pass `self`.
    getNodes = function(nodes = NULL, determOnly = FALSE, stochOnly = FALSE,
                        includeData = TRUE, dataOnly = FALSE,
                        includeRHSonly = FALSE,
                        topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE,
                        includePredictive = TRUE, predictiveOnly = FALSE,
                        nodesAsChars = getNimbleModelOption('nodesAsChars'),
                        returnScalarComponents = FALSE,
                        .sort = FALSE) {
      nimbleModel::getNodes(
        self, nodes, determOnly, stochOnly, includeData, dataOnly,
        includeRHSonly,
        topOnly, latentOnly, endOnly,
        includePredictive, predictiveOnly, 
        nodesAsChars, returnScalarComponents, .sort
      )
    },
    getNodeNames = function(determOnly = FALSE, stochOnly = FALSE,
                        includeData = TRUE, dataOnly = FALSE, includeRHSonly = FALSE,
                        topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE,
                        includePredictive = TRUE, predictiveOnly = FALSE,
                        returnType = "names",
                        returnScalarComponents = FALSE) {
      nimbleModel::getNodeNames(
        self, determOnly, stochOnly, includeData, dataOnly,
        includeRHSonly, topOnly, latentOnly, endOnly,
        includePredictive, predictiveOnly, returnType, returnScalarComponents
      )
    },
    expandNodeNames = function(nodes, returnScalarComponents = FALSE,
                               returnType = "names", sort = FALSE, unique = TRUE) {
      nimbleModel::expandNodeNames(self, nodes, returnScalarComponents, "names", sort, unique)
    },
    calc_op = function(instr, fn, fn_cpp) {
      if (missing(instr)) {
        instr <- getVarNames()
      }
      instrList <- makeInstrList(self, instr)
      if (isCompiled()) {
        if (!instrList$isCompiled()) instrList <- makeCompiledInstrList(instrList)
        return(self[[fn_cpp]](instrList))
      }
      logProb <- 0
      for (i in 1:length(instrList)) {
        logProb <- logProb + declFunList[[instrList[[i]]$declID]][[fn]](instrList[[i]])
      }
      return(logProb)
    },
    calculate = function(instr) {
      logProb <- calc_op(instr, "calculate", "calculate_impl")
      return(logProb)
    },
    calculateDiff = function(instr) {
      logProb <- calc_op(instr, "calculateDiff", "calculateDiff_impl")
      return(logProb)
    },
    getLogProb = function(instr) {
      logProb <- calc_op(instr, "getLogProb", "getLogProb_impl")
      return(logProb)
    },
    simulate = function(instr, includeData = FALSE) {
      if (missing(instr)) {
        instr <- getVarNames()
      }
      instrList <- makeInstrList(self, instr, includeData = includeData)
      if(is.null(instrList)) return(invisible(NULL))
      if (isCompiled()) {
        if (!instrList$isCompiled()) instrList <- makeCompiledInstrList(instrList)
        self$simulate_impl(instrList)
      } else {
        for (i in 1:length(instrList)) {
          declFunList[[instrList[[i]]$declID]]$simulate(instrList[[i]])
        }
      }
      return(invisible(NULL))
    }
  ),
  Cpublic = list(
    # TODO: using 'RcppObject' was resulting in a symbolTBD error - probably nCompiler issue 186.
    declFunList = "numericScalar", # 'RcppObject',  # This won't actually be used in C++, but needs to be in Cpublic for accessibility.
    declFunNameToIndex = "RcppList", # Not sure what type this should be for use in C++.
    ping = nFunction(
      name = "ping",
      function() {
        return(TRUE)
        returnType(logical())
      },
      compileInfo = list(virtual = TRUE)
    ),
    makeCompiledInstrList = nFunction(
      name = "makeCompiledInstrList",
      function(input = "SEXP") {
        ans <- nList(instr_nClass)$new()
        cppLiteral("ans->set_all_values(input);")
        return(ans)
      },
      returnType = "nList(instr_nClass)"
    ),
    calculate_impl = nFunction(
      name = "calculate_impl",
      function(instrList) {
        cat("Uncompiled `calculate_impl` should never be called.\n")
        return(0)
      },
      returnType = "numericScalar",
      compileInfo = list(
        C_fun = function(instrList = "nList(instr_nClass)") {
          # NOTE: instrList input will be ordered.
          cppLiteral('Rprintf("modelBase_nClass calculate_impl (should not see this)\\n");')
          return(0)
        },
        virtual = TRUE
      )
    ),
    calculateDiff_impl = nFunction(
      name = "calculateDiff_impl",
      function(instrList) {
        cat("Uncompiled `calculateDiff_impl` should never be called.\n")
        return(0)
      },
      returnType = "numericScalar",
      compileInfo = list(
        C_fun = function(instrList = "nList(instr_nClass)") {
          # NOTE: instrList input will be ordered.
          cppLiteral('Rprintf("modelBase_nClass calculateDiff_impl (should not see this)\\n");')
          return(0)
        },
        virtual = TRUE
      )
    ),
    getLogProb_impl = nFunction(
      name = "getLogProb_impl",
      function(instrList) {
        cat("Uncompiled `getLogProb_impl` should never be called.\n")
        return(0)
      },
      returnType = "numericScalar",
      compileInfo = list(
        C_fun = function(instrList = "nList(instr_nClass)") {
          # NOTE: instrList input will be ordered.
          cppLiteral('Rprintf("modelBase_nClass getLogProb_impl (should not see this)\\n");')
          return(0)
        },
        virtual = TRUE
      )
    ),
    simulate_impl = nFunction(
      name = "simulate_impl",
      function(instrList) {
        cat("Uncompiled `simulate_impl` should never be called.\n")
        return(invisible(NULL))
      },
      returnType = "void",
      compileInfo = list(
        C_fun = function(instrList = "nList(instr_nClass)") {
          # NOTE: instrList input will be ordered.
          cppLiteral('Rprintf("modelBase_nClass simulate_impl (should not see this)\\n");')
        },
        virtual = TRUE
      )
    )
  ),
  # See comment above about needing to ensure a virtual destructor
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("modelBase_nC")),
  compileInfo = list(
    interface = "full",
    createFromR = FALSE,
    Hincludes = c('"declFunBase_nClass_c_.h"', '"instr_nClass_c_.h"'),
    needed_units = list("declFunBase_nClass", "instr_nClass", "nList(instr_nClass)"),
    exportName = "modelBase_nClass_new",
    packageNames = c(uncompiled = "modelBase_nClass_R", compiled = "modelBase_nClass")
  )
)

# Manually add
# # "#include <nCompiler/predef/modelClass_/modelClass_.h>" to that file,
# after the header content.


# nCompile(modelBase_nClass, control=list(generate_predefined=TRUE))
