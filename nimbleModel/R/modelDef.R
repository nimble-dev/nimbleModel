# A class representing a model definition; i.e., the model declarations
# and graph structure of a model.

# Some modelDefClass methods loop over modelDeclClass objects and create new/modified
# ones, hence they do not call out to methods of modelDeclClass.

# Contains sets of graphRules (upstream/parent and downstream/child)
# and sets of various kinds of nodeRules.
# {top,end,latent}Rules are just lists of pointers/shallow copies of calcRules.

modelDefClass <- R6Class(
  classname = "modelDefClass",
  portable = FALSE,
  public = list(
    modelCode = NULL,
    contexts = list(),
    constants = list(),
    declInfo = list(),
    declFunNameToIndex = list(),
    declFunIndexToName = NULL,
    downstreamRules = NULL,
    upstreamRules = NULL,
    calcRules = NULL,
    rhsOnlyRules = NULL,
    declRules = NULL,
    topRules = NULL,
    latentRules = NULL,
    endRules = NULL,
    varNames = NULL,
    varInfo = NULL,
    logProbVarInfo = NULL,
    initialize = function(code = NULL, constants = list(), dimensions = list(),
                          inits = list(), data = list(), userEnv = parent.frame()) {
      checkAndAssignConstants(constants, code)

      modelCode <<- codeProcessIfThenElse(code, constants, userEnv)
      modelCode <<- nimble:::nf_changeNimKeywords(modelCode) # Formerly in `assignBUGScode`.

      # TODO: add this later.
      # setModelValuesClassName()

      assignDimensions(dimensions, inits, data)
      initializeContexts() # Creates empty context.
      processModelCode() # Determines declarations and contexts.
      removeDataFromConstants() # Remove LHS variables from constants.
      addMissingIndexing() # Fill in missing indexing using `dimensions`.
      processBoundsAndTruncation()
      expandDistributions() # Handle parameterizations.
      checkMultivarExpr() # Check that multivariate params are not expressions.
      processLinks()
      reparameterizeDists()
      replaceAllConstants()
      liftExpressionArgs()
      addRemainingDotParams() # Add additional altParams as needed.
      replaceAllConstants() # Simplify expressions introduced in `addRemainingDotParams`.
      processDecls(userEnv) # Create declRules and set up symbolicParentNodes (and flags dynamic indexing).
      assignDeclIDs() # Set sequential declID values and declFun mapping.
      genAltParams() # Create altParam expressions and create `calculateCode` (without altParams).
      genBounds() # Create bound expressions (modifying `calculateCode`).

      makeRHSoriginalRules()
      makeVarInfo() # This requires `rhsOriginalRules` and `symbolicParentNodes`.

      # Change dynamic indices to full extent.
      # (e.g., `mu[k[i]]` to `mu[1:10]`, as needed for setting up graphRules.
      replaceDynamicIndexingInParents()

      makeGraphRules() # Create declaration-specific graphRules.
      makeGraphInfo() # Create calcRules and rhsOnlyRules.

      # TODO: add later
      # buildSymbolTable()

      makeVarNames()

      warnRHSonlyDynamicIndexing()

      invisible(NULL)
    },

    # Check constants and assign into class.
    checkAndAssignConstants = function(constants, code) {
      if (!is.list(constants) || (length(constants) && is.null(names(constants)))) {
        stop("`constants` must be a named list.")
      }
      if (length(names(constants))) {
        constantsInCode <- names(constants) %in% all.vars(code)
        if (!all(constantsInCode)) {
          for (constName in names(constants)[!constantsInCode]) {
            messageIfVerbose(
              "  [Note] '", constName,
              "' is provided in `constants` but not used in the model code and is being ignored."
            )
          }
        }
      }
      constants <<- constants
      invisible(NULL)
    },

    # Check dimensions and add into class field.
    assignDimensions = function(dimensions, initsList, dataList) {
      # First, add the provided dimensions.
      dL <- dimensions
      if (is.null(dL)) {
        dL <- list()
      }

      # Add dimensions of any *non-scalar* constants.
      # We'll try to be smart about this: check for duplicate names in constants and dimensions, and make sure they agree.
      for (constName in names(constants)) {
        # TODO: dimOrLength should be in nimbleInternalFunctions.
        constDim <- dimOrLength(constants[[constName]], scalarize = FALSE) # Don't scalarize as want to preserve dims as provided by user, e.g. for 1x1 matrices.
        if (length(constDim) == 1 && constDim == 1) {
          constDim <- integer(0)
        } # But for 1-length vectors treat as scalars as that is how handled in system.
        if (constName %in% names(dL)) {
          if (!identical(as.integer(dL[[constName]]), as.integer(constDim))) {
            stop(
              "inconsistent dimensions between `constants` and `dimensions` arguments: `",
              constName, "`."
            )
          }
        } else {
          dL[[constName]] <- constDim
        }
      }

      # Add dimensions of any *non-scalar* inits to dimensionsList.
      # We'll try to be smart about this: check for duplicate names in inits and dimensions, and make sure they agree.
      for (initName in names(initsList)) {
        initDim <- dimOrLength(initsList[[initName]], scalarize = FALSE) # Don't scalarize as want to preserve dims as provided by user, e.g. for 1x1 matrices.
        if (!(length(initDim) == 1 && initDim == 1)) { # E.e., non-scalar inits; 1-length vectors treated as scalars and not passed along as dimension info to avoid conflicts between scalars and one-length vectors/matrices/arrays in various places.
          if (initName %in% names(dL)) {
            if (!identical(as.integer(dL[[initName]]), as.integer(initDim))) {
              messageIfVerbose(
                "  [Warning] Inconsistent dimensions between inits and dimensions arguments: `",
                initName, "`; ignoring dimensions in inits."
              )
            }
          } else {
            dL[[initName]] <- as.numeric(initDim)
          }
        }
      }

      # Add dimensions of any *non-scalar* data to dimensionsList.
      # We'll try to be smart about this: check for duplicate names in data and dimensions, and make sure they agree.
      # Main use case here is when user provides RHS only variable as data.
      for (dataName in names(dataList)) {
        if (!is.null(dataName) && dataName != "") {
          dataDim <- dimOrLength(dataList[[dataName]], scalarize = FALSE) # Don't scalarize as want to preserve dims as provided by user, e.g. for 1x1 matrices.
          if (!(length(dataDim) == 1 && dataDim == 1)) { # I.e., non-scalar data; 1-length vectors treated as scalars and not passed along as dimension info to avoid conflicts between scalars and one-length vectors/matrices/arrays in various places.
            if (dataName %in% names(dL)) {
              if (!identical(as.integer(dL[[dataName]]), as.integer(dataDim))) {
                messageIfVerbose("  [Note] Inconsistent dimensions between data and dimensions arguments: ", dataName, "; ignoring dimensions in data.")
              }
            } else {
              dL[[dataName]] <- dataDim
            }
          }
        }
      }
      dimensionsList <<- dL
      invisible(NULL)
    },

    # Process raw code to determine declarations and contexts.
    processModelCode = function(code = NULL, contextID = 1, lineNumber = 0, envir) {
      recursiveCall <- lineNumber != 0
      if (is.null(code)) {
        code <- modelCode
        declInfo <<- list()
      }
      for (i in seq_along(code)) {
        if (code[[i]] == "{") {
          if (length(code[[i]]) == 1) {
            next
          }
        } # skip { lines
        lineNumber <- lineNumber + 1
        if (code[[i]][[1]] == "~" || code[[i]][[1]] == "<-") { # a declaration
          iAns <- length(declInfo) + 1
          if (code[[i]][[1]] == "~") {
            code[[i]] <- replaceDistributionAliases(code[[i]])
            checkUserDefinedDistribution(code[[i]], envir)
          }
          if (code[[i]][[1]] == "<-") {
            checkForDeterministicDorR(code[[i]])
          }

          declInfo[[iAns]] <<- modelDeclClass$new(
            code[[i]],
            contexts[[contextID]],
            lineNumber
          )
        }
        if (code[[i]][[1]] == "for") {
          # e.g. (for i in 1:N).  New context (for-loop info) needed.
          indexVarExpr <- code[[i]][[2]] # This is the `i`.
          if (length(contexts) > 0) {
            if (as.character(indexVarExpr) %in% contexts[[contextID]]$indexVarNames) {
              stop(
                "variable `",
                as.character(indexVarExpr),
                "` used multiple times as for loop index in nested loops.",
                "If your model has macros or if-then-else blocks,",
                "you can inspect the processed model code by running ",
                "`setNimbleModelOptions('stop_after_processing_model_code', TRUE)`",
                "before calling nimbleModel."
              )
            }
          }
          indexRangeExpr <- code[[i]][[3]] # This is the `1:N`.
          if (getNimbleModelOption("prioritizeColonLikeBUGS")) {
            indexRangeExpr <- reprioritizeColonOperator(indexRangeExpr)
          }

          nextContextID <- length(contexts) + 1
          forCode <- code[[i]][1:3] # This is the `(for i in 1:N)` without the code block.
          forCode[[3]] <- indexRangeExpr
          # Add the new context.
          singleContexts <- c(
            if (contextID == 1) NULL else contexts[[contextID]]$singleContexts,
            list(singleContextClass$new(
              indexVarExpr = indexVarExpr,
              indexRangeExpr = indexRangeExpr,
              forCode = forCode
            ))
          )
          contexts[[nextContextID]] <<- modelContextClass$new(singleContexts = singleContexts)
          if (length(code[[i]][[4]]) == 1) {
            stop("cannot evaluate `", safeDeparse(code[[i]]), "`.")
          }
          recurseCode <- if (code[[i]][[4]][[1]] == "{") {
            code[[i]][[4]]
          } else {
            substitute(
              {
                ONELINE
              },
              list(ONELINE = code[[i]][[4]])
            )
          }
          # Recursive call to process the contents of the for loop
          lineNumber <-
            processModelCode(
              recurseCode,
              nextContextID,
              lineNumber = lineNumber,
              envir = envir
            )
        }
        if (code[[i]][[1]] == "{") {
          # Recursive call to a block contained in a `{}`,
          # perhaps as a result of `processCodeIfThenElse`.
          lineNumber <-
            processModelCode(
              code[[i]],
              contextID,
              lineNumber = lineNumber,
              envir = envir
            )
        }
        if (!safeDeparse(code[[i]][[1]]) %in% c("~", "<-", "for", "{")) {
          stop(
            "`",
            safeDeparse(code[[i]][[1]]),
            " not allowed in model code in `",
            safeDeparse(code[[i]]), "`."
          )
        }
      }
      invisible(lineNumber)
    },

    # Remove items from `constants` that appear as LHS variables (i.e., `data`).
    # CHECK: are there any cases where this deals with something other than 'data'?
    removeDataFromConstants = function() {
      constantsNames <- names(constants)
      if (length(constantsNames)) {
        vars <- sapply(declInfo, function(x) x$targetVarName)
        newDataVars <- constantsNames[constantsNames %in% vars]
        if (length(newDataVars)) {
          messageIfVerbose(
            "  [Note] Using `", paste0(newDataVars, collapse = "`, `"),
            "` (given within `constants`) as data."
          )
          for (varName in newDataVars) {
            constants[[varName]] <<- NULL
          }
        }
      }
      invisible(NULL)
    },

    # Overwrites `declInfo`, using `dimensionsList`, to fill in in any missing indexing.
    addMissingIndexing = function() {
      for (i in seq_along(declInfo)) {
        decl <- declInfo[[i]]
        newCode <- addMissingIndexingRecurse(decl$code, dimensionsList)
        declInfo[[i]] <<- modelDeclClass$new(newCode, decl$context, decl$sourceLineNumber)
      }
      invisible(NULL)
    },

    # For non-truncated declarations, extract range info from distribution;
    # for truncated declarations, pulls bounds out of `T()` syntax.
    processBoundsAndTruncation = function() {
      for (i in seq_along(declInfo)) {
        decl <- declInfo[[i]]
        if (!decl$stoch) next

        callName <- decl$distributionName
        if (!(callName %in% c("T", "I"))) {
          truncated <- FALSE
          boundExprs <- getDistributionInfo(callName)$range
        } else {
          truncated <- TRUE
          if (callName == "I") {
            messageIfVerbose("  [Note] Interpreting `I(,)` as truncation (equivalent to `T(,)`) in `", safeDeparse(decl$code), "`; this is only valid when ", safeDeparse(decl$targetExpr), " has no unobserved (stochastic) parents.")
          }

          newCode <- decl$code
          newCode[[3]] <- decl$valueExpr[[2]] # insert the core density function call

          distName <- as.character(newCode[[3]][[1]])
          if (!getAllDistributionsInfo("pqAvail")[distName]) {
            stop(
              "cannot implement truncation for `",
              distName, "`; 'p' and 'q' functions not available."
            )
          }

          distRange <- getDistributionInfo(distName)$range
          boundExprs <- distRange

          if (length(decl$valueExpr) >= 3 && decl$valueExpr[[3]] != "") {
            boundExprs$lower <- decl$valueExpr[[3]]
          }
          if (length(decl$valueExpr) >= 4 && decl$valueExpr[[4]] != "") {
            boundExprs$upper <- decl$valueExpr[[4]]
          }
          if (length(decl$valueExpr) != 4) {
            messageIfVerbose(
              "   [Note] Lower and upper bounds not supplied for `T()`; proceeding with bounds: (",
              paste(boundExprs, collapse = ","), ")."
            )
          }

          decl$code <- newCode
        }
        declInfo[[i]] <<- modelDeclClass$new(
          decl$code, decl$context,
          decl$sourceLineNumber,
          truncated, boundExprs
        )
      }
      invisible(NULL)
    },

    # Overwrite `declInfo` for stochastic nodes:
    # calls `match.call()` on RHS (using `distributions$matchCallEnv`)
    expandDistributions = function() {
      for (i in seq_along(declInfo)) {
        decl <- declInfo[[i]]
        if (!decl$stoch) next

        newCode <- decl$code
        newCode[[3]] <- evalInDistsMatchCallEnv(decl$valueExpr)
        declInfo[[i]] <<- modelDeclClass$new(
          newCode, decl$context,
          decl$sourceLineNumber,
          decl$truncated, decl$boundExprs
        )
      }
      invisible(NULL)
    },

    # Check that multivariate params are not expressions.
    checkMultivarExpr = function() {
      if (getNimbleModelOption("disallowMultivariateArgumentExpressions")) {
        for (i in seq_along(declInfo)) {
          declInfo[[i]]$checkMultivarExpr()
        }
      }
      invisible(NULL)
    },

    # Overwrite declInfo (*and adds*) for nodes with link functions (using `linkInverses`).
    processLinks = function() {
      newDeclInfo <- list()
      for (i in seq_along(declInfo)) {
        decl <- declInfo[[i]]
        nextNewDeclInfoIndex <- length(newDeclInfo) + 1
        if (is.null(decl$transExpr)) {
          newDeclInfo[[nextNewDeclInfoIndex]] <- decl
          next
        }
        linkText <- safeDeparse(decl$transExpr, warn = TRUE)
        if (!(linkText %in% names(linkInverses))) stop("unknown link function: `", linkText, "`.")

        if (decl$stoch) { # stochastic declaration
          code <- decl$code
          code[[2]] <- parse(text = paste0(linkText, "_", decl$targetNodeName), keep.source = FALSE)[[1]]

          newRHS <- linkInverses[[linkText]]
          newRHS[[2]] <- code[[2]]
          newCode <- substitute(A <- B, list(A = decl$targetNodeExpr, B = newRHS))

          newDeclInfo[[nextNewDeclInfoIndex]] <- modelDeclClass$new(
            code, decl$context,
            decl$sourceLineNumber,
            decl$truncated, decl$boundExprs
          )

          newDeclInfo[[nextNewDeclInfoIndex + 1]] <- modelDeclClass$new(
            newCode, decl$context,
            decl$sourceLineNumber,
            decl$truncated, decl$boundExprs
          )
        } else { # deterministic declaration
          newRHS <- linkInverses[[linkText]]
          newRHS[[2]] <- decl$code[[3]]
          newLHS <- decl$targetNodeExpr
          newCode <- substitute(A <- B, list(A = newLHS, B = newRHS))
          newDeclInfo[[nextNewDeclInfoIndex]] <- modelDeclClass$new(
            newCode, decl$context,
            decl$sourceLineNumber,
            decl$truncated, decl$boundExprs
          )
        }
      } # close loop over declInfo
      declInfo <<- newDeclInfo
      invisible(NULL)
    },
    reparameterizeDists = function() {
      for (i in seq_along(declInfo)) {
        decl <- declInfo[[i]]
        if (!decl$stoch) next # skip deterministic nodes
        code <- decl$code
        valueExpr <- decl$valueExpr # grab the RHS (distribution)
        distName <- decl$distributionName
        # CHECK: shouldn't this be trapped earlier (e.g., in expandDistributions?).
        if (!(distName %in% getAllDistributionsInfo("namesVector"))) {
          stop("unknown distribution name: `", distName, "`.")
        }
        distRule <- getDistributionInfo(distName)
        numArgs <- length(distRule$reqdArgs)
        newValueExpr <- quote(dist()) # set up a parse tree for the new value expression
        newValueExpr[[1]] <- as.name(distName) # add in the distribution name
        if (!numArgs) { # for dflat and dhalfflat, or a user-defined distribution might have 0 arguments
          nonReqdArgExprs <- NULL
          boundExprs <- decl$boundExprs
        } else {
          newValueExpr[1 + (1:numArgs)] <- rep(NA, numArgs) # fill in the new parse tree with required arguments
          names(newValueExpr)[1 + (1:numArgs)] <- distRule$reqdArgs # add names for the arguments

          params <- if (length(valueExpr) > 1) as.list(valueExpr[-1]) else structure(list(), names = character()) # extract the original distribution parameters

          if (identical(sort(names(params)), sort(distRule$reqdArgs))) {
            matchedAlt <- 0
          } else {
            matchedAlt <- NULL
            count <- 0
            while (is.null(matchedAlt) && count < distRule$numAlts) {
              count <- count + 1
              if (identical(sort(unique(distRule$alts[[count]])), sort(unique(names(params))))) {
                matchedAlt <- count
              }
            }
            if (is.null(matchedAlt)) {
              stop(
                "invalid parameters for distribution `",
                safeDeparse(valueExpr), "`. (No available re-parameterization found.)"
              )
            }
          }
          nonReqdArgs <- names(params)[!(names(params) %in% distRule$reqdArgs)]
          for (iArg in seq_len(numArgs)) { # loop over the required arguments
            reqdArgName <- distRule$reqdArgs[iArg]
            # If it was supplied, copy the supplied expression "as is".
            if (reqdArgName %in% names(params)) {
              newValueExpr[[iArg + 1]] <- params[[reqdArgName]]
              next
            }
            if (!matchedAlt) {
              stop(
                "problem in processing distribution parameterizations: looking for alternative parameterization, but supplied args are same as required args in `",
                safeDeparse(valueExpr), "`."
              )
            }
            if (!reqdArgName %in% names(distRule$exprs[[matchedAlt]])) {
              stop(
                "could not find `",
                reqdArgName, "` in alternative parameterization number ", matchedAlt, " for `", safeDeparse(valueExpr), "`."
              )
            }
            transformedParameterPT <- distRule$exprs[[matchedAlt]][[reqdArgName]]
            # handles pathological-case model variable names, e.g., `y ~ dnorm(0, tau = sd)`.
            namesToSubstitute <- intersect(c(nonReqdArgs, distRule$reqdArgs), all.vars(transformedParameterPT))
            for (nm in namesToSubstitute) {
              # Loop thru possible non-canonical parameters in the expression for the canonical parameter.
              if (is.null(params[[nm]])) stop("processing error in parameter transformation.")
              transformedParameterPT <- parseTreeSubstitute(pt = transformedParameterPT, pattern = as.name(nm), replacement = params[[nm]])
            }
            newValueExpr[[iArg + 1]] <- transformedParameterPT
          }

          # Evaluate boundExprs in context of model.
          boundExprs <- decl$boundExprs
          reqdParams <- as.list(newValueExpr[-1])
          for (iBound in 1:2) {
            if (!is.numeric(boundExprs[[iBound]])) {
              # Only expecting `boundExprs` to be functions of `reqdArgs`.
              if (length(intersect(nonReqdArgs, all.vars(boundExprs[[iBound]])))) {
                stop(
                  "expecting expressions for distribution range for `",
                  distName, "` to be functions only of required arguments, namely the parameters used in the 'Rdist' element."
                )
              }
              namesToSubstitute <- intersect(c(distRule$reqdArgs), all.vars(boundExprs[[iBound]]))
              for (nm in namesToSubstitute) {
                if (is.null(params[[nm]])) stop("processing error in parameter transformation.")
                boundExprs[[iBound]] <- parseTreeSubstitute(pt = boundExprs[[iBound]], pattern = as.name(nm), replacement = params[[nm]])
              }
            }
          }

          # Hold onto the expressions for non-required args.
          nonReqdArgExprs <- params[nonReqdArgs] # Grab the non-required args from the original params list.
          # Append '.' to the front of all the old (reparameterized away) param names.
          names(nonReqdArgExprs) <- if (length(nonReqdArgExprs) > 0) paste0(".", names(nonReqdArgExprs)) else character(0)
        }
        names(boundExprs)[names(boundExprs) %in% c("lower", "upper")] <-
          paste0(names(boundExprs)[names(boundExprs) %in% c("lower", "upper")], "_")
        newValueExpr <- as.call(c(as.list(newValueExpr), boundExprs, nonReqdArgExprs))
        newCode <- decl$code
        newCode[[3]] <- newValueExpr

        # Note at this point `boundExprs` set back to NULL as all info in `lower` , `upper` in `valueExpr`.
        declInfo[[i]] <<- modelDeclClass$new(
          newCode, decl$context,
          decl$sourceLineNumber, decl$truncated, NULL
        )
      } # close loop over declInfo
      invisible(NULL)
    },

    # Overwrite declInfo (both LHS and RHS) with constants replaced; only replaces scalar constants.
    replaceAllConstants = function() {
      constantsEnv <- list2env(constants, parent = getDefaultNamespace())
      for (i in seq_along(declInfo)) {
        newCode <- replaceConstantsRecurse(declInfo[[i]]$code, constantsEnv)$code
        declInfo[[i]] <<- modelDeclClass$new(
          newCode, declInfo[[i]]$context,
          declInfo[[i]]$sourceLineNumber,
          declInfo[[i]]$truncated, declInfo[[i]]$boundExprs
        )
      }
      invisible(NULL)
    },

    # Overwrite declInfo (*and adds*), lifting any expressions in distribution arguments to new declarations.
    liftExpressionArgs = function() {
      newDeclInfo <- list()
      for (i in seq_along(declInfo)) {
        decl <- declInfo[[i]]
        valueExpr <- decl$valueExpr
        newValueExpr <- valueExpr # `newValueExpr` is initially a copy of the old one.

        nextNewDeclInfoIndex <- length(newDeclInfo) + 1

        if (decl$stoch) {
          params <- as.list(valueExpr[-1]) # Extract the original distribution parameters.
          paramNames <- names(valueExpr)[-1]
          types <- nimble:::distributions[[decl$distributionName]]$types
          # `types` may be NULL if all are scalar.

          for (iParam in seq_along(params)) {
            # Skips '.param' names, 'lower', and 'upper'; we do NOT lift these.
            if (grepl("^\\.", names(params)[iParam]) || names(params)[iParam] %in% c("lower_", "upper_")) next
            paramExpr <- params[[iParam]]
            paramName <- paramNames[iParam]
            if (!isExprLiftable(paramExpr, types[[paramName]])) next # If this param isn't an expression, skip.
            requireNewAndUniqueDecl <- any(decl$context$indexVarNames %in% all.vars(paramExpr))
            uniquePiece <- if (requireNewAndUniqueDecl) paste0("_L", decl$sourceLineNumber) else ""
            # Pass through `Rname2CppName` twice (creating new variable name) so that long names truncated if adding 'lifted_' puts them over nchar limit.
            newNodeNameExpr <- as.name(paste0(Rname2CppName(paste0(
              "lifted_",
              Rname2CppName(paramExpr, colonsOK = TRUE)
            ), colonsOK = TRUE), uniquePiece))
            if (safeDeparse(paramExpr[[1]], warn = TRUE) %in% liftedCallsDoNotAddIndexing) { # Skip adding indexing to mixed-size calls.
              newNodeNameExprIndexed <- newNodeNameExpr
            } else {
              newNodeNameExprIndexed <- addNecessaryIndexingToNewNode(newNodeNameExpr, paramExpr, decl$context$indexVarExprs) # Add indexing if necessary.
            }

            newValueExpr[[iParam + 1]] <- newNodeNameExprIndexed

            newNodeCode <- substitute(LHS <- RHS, list(LHS = newNodeNameExprIndexed, RHS = paramExpr)) # Create code line for declaration of new node.
            # If `requireNewAndUniqueDecl` is `TRUE`, the _L# is appended to the `newNodeNameExpr` and it should be impossible for this to be TRUE:
            identicalNewDecl <- checkForDuplicateNodeDeclaration(newNodeCode, newNodeNameExprIndexed, newDeclInfo)

            if (!identicalNewDecl) {
              # Keep new declaration in the same context, regardless of presence/absence of indexing.
              newDeclInfo[[nextNewDeclInfoIndex]] <- modelDeclClass$new(
                newNodeCode, decl$context,
                decl$sourceLineNumber, FALSE, NULL
              )

              nextNewDeclInfoIndex <- nextNewDeclInfoIndex + 1 # Update for lifting other nodes, and re-adding decl at the end.
            }
          } # closes loop over params
        }
        newCode <- decl$code
        newCode[[3]] <- newValueExpr
        newDeclInfo[[nextNewDeclInfoIndex]] <- modelDeclClass$new(
          newCode, decl$context,
          decl$sourceLineNumber,
          decl$truncated, decl$boundExprs
        ) # Regardless of anything, add decl itself in.
      } # closes loop over declInfo
      declInfo <<- newDeclInfo
      invisible(NULL)
    },
    assignDeclIDs = function() {
      for (i in seq_along(declInfo)) {
        declInfo[[i]]$declRule$ID <- as.character(i)
      }
      declFunNameToIndex <<- as.list(1:length(declInfo))
      names(declFunNameToIndex) <<- paste("declFun", 1:length(declInfo), sep = "_")
    },

    # Add additional altParams not already addressed in getting canonical params.
    addRemainingDotParams = function() {
      for (iDecl in seq_along(declInfo)) {
        decl <- declInfo[[iDecl]]
        if (!decl$stoch) next
        valueExpr <- decl$valueExpr # Grab the RHS (distribution).
        newValueExpr <- valueExpr
        defaultParamExprs <- getDistributionInfo(as.character(newValueExpr[[1]]))$altParams
        if (!length(defaultParamExprs)) next # Skip if there are no altParams defined in distributions.

        defaultParamNames <- names(defaultParamExprs)
        defaultDotParamNames <- paste0(".", defaultParamNames)
        for (iParam in seq_along(defaultDotParamNames)) {
          dotParamName <- defaultDotParamNames[iParam]
          if (!(dotParamName %in% names(newValueExpr))) {
            defaultParamExpr <- defaultParamExprs[[iParam]]
            subParamExpr <- eval(substitute(substitute(EXPR, as.list(valueExpr)[-1]), list(EXPR = defaultParamExpr)))
            newValueExpr[[dotParamName]] <- subParamExpr
          }
        }
        newCode <- decl$code
        newCode[[3]] <- newValueExpr
        declInfo[[iDecl]] <<- modelDeclClass$new(
          newCode, decl$context,
          decl$sourceLineNumber,
          decl$truncated, decl$boundExprs
        )
      }
      invisible(NULL)
    },

    # Create declaration rule and determines symbolic parent nodes (RHS pieces) for each declaration.
    processDecls = function(envir) {
      nimFunNames <- getAllDistributionsInfo("namesExprList")
      for (i in seq_along(declInfo)) {
        declInfo[[i]]$processDecl(nimFunNames, constants, envir)
      }
      invisible(NULL)
    },

    # Create altParam expressions and `canonicalCode` (without altParams, used in `calculate`).
    genAltParams = function() {
      for (i in seq_along(declInfo)) {
        declInfo[[i]]$genAltParams()
      }
      invisible(NULL)
    },

    # Create bound expressions and remove bounds from `calculateCode`.
    genBounds = function() {
      for (i in seq_along(declInfo)) {
        declInfo[[i]]$genBounds()
      }
      invisible(NULL)
    },

    # Create an object storing variable-level information, including variable extents (mins, maxs).
    makeVarInfo = function() {
      # First set up `varInfo`s for all LHS variables and collect `anyStoch`.
      # That allows determination of when logProb information needs to be collected.
      for (iDI in seq_along(declInfo)) {
        decl <- declInfo[[iDI]]
        # TODO: what is this case and how handle with new nimbleModel?
        # if(decl$numUnrolledNodes == 0) next
        # LHS:
        lhsVar <- decl$targetVarName
        if (!(lhsVar %in% names(varInfo))) {
          nDim <- if (length(decl$targetNodeExpr) == 1) 0 else length(decl$targetNodeExpr) - 2
          varInfo[[lhsVar]] <<- varInfoClass$new(
            varName = lhsVar,
            mins = rep(Inf, nDim),
            maxs = rep(0, nDim),
            nDim = nDim,
            anyStoch = FALSE
          )
        } else { # Multiple LHS variable declarations with mismatched dimensions.
          nDim <- if (length(decl$targetNodeExpr) == 1) 0 else length(decl$targetNodeExpr) - 2
          if (nDim == 0) {
            stop("There are multiple definitions for variable `", lhsVar, "`.")
          }
          if (nDim != varInfo[[lhsVar]]$nDim) {
            stop("Inconsistent dimensions in declarations for variable `", lhsVar, "`.")
          }
        }
        varInfo[[lhsVar]]$anyStoch <<- varInfo[[lhsVar]]$anyStoch | decl$stoch
      }

      anyStoch <- unlist(lapply(varInfo, `[[`, "anyStoch"))
      logProbVarInfo <<- lapply(varInfo[anyStoch], function(x) {
        varInfoClass$new(
          varName = makeLogProbName(x$varName),
          mins = rep(Inf, x$nDim),
          maxs = rep(0, x$nDim),
          nDim = x$nDim,
          anyStoch = FALSE
        )
      })
      names(logProbVarInfo) <<- lapply(logProbVarInfo, `[[`, "varName")

      for (iDI in seq_along(declInfo)) {
        decl <- declInfo[[iDI]]
        # TODO: what is this case and how handle with new nimbleModel?
        # if(decl$numUnrolledNodes == 0) next
        # LHS:
        lhsVar <- decl$targetVarName
        anyStoch <- varInfo[[lhsVar]]$anyStoch
        if (anyStoch) lhsLogProbVar <- makeLogProbName(lhsVar)
        if (varInfo[[lhsVar]]$nDim > 0) {
          if (!is.null(decl$declRule$fullRange)) {  # NULL can occur with backwards indexing.
              newMinMax <- decl$declRule$fullRange$getMinMax()
              # Force overwrite of placeholder max based on LHS info.
              varInfo[[lhsVar]]$maxs[varInfo[[lhsVar]]$maxs == .Machine$integer.max] <<- 0
              if(FALSE) { # This is not sophisticated enough - see issue #26.
                # Check for overlap in all dimensions, indicating duplicate declaration.
                if (sum(varInfo[[lhsVar]]$maxs)) { # On repeated declaration for a variable.
                  overlap <- !(varInfo[[lhsVar]]$maxs < newMinMax[, 1] |
                                 varInfo[[lhsVar]]$mins > newMinMax[, 2])
                  if (all(overlap)) {
                    stop("Indexing for declarations for variable `", lhsVar, "` overlaps.")
                  }
                }
              }
              varInfo[[lhsVar]]$mins <<- pmin(varInfo[[lhsVar]]$mins, newMinMax[, 1])
              varInfo[[lhsVar]]$maxs <<- pmax(varInfo[[lhsVar]]$maxs, newMinMax[, 2])
              if (anyStoch) {
                logProbVarInfo[[lhsLogProbVar]]$mins <<- pmin(logProbVarInfo[[lhsLogProbVar]]$mins, newMinMax[, 1])
                logProbVarInfo[[lhsLogProbVar]]$maxs <<- pmax(logProbVarInfo[[lhsLogProbVar]]$maxs, newMinMax[, 2])
              }
          }
        }
      }
      for (iDI in seq_along(declInfo)) { # Do RHS after all LHS so that check for overlap only concerns LHS
        decl <- declInfo[[iDI]]
        for (iRHR in seq_along(decl$rhsOriginalRules)) {
          rhsRule <- decl$rhsOriginalRules[[iRHR]]
          rhsVar <- rhsRule$varName
          if (!(rhsVar %in% names(varInfo))) {
            tmp <- stripIndexWrapping(decl$symbolicParentNodes[[iRHR]])
            nDim <- if (length(tmp) == 1) 0 else length(tmp) - 2
            varInfo[[rhsVar]] <<- varInfoClass$new(
              varName = rhsVar,
              mins = rep(Inf, nDim),
              maxs = rep(0, nDim),
              nDim = nDim,
              anyStoch = FALSE
            )
          }
          if (varInfo[[rhsVar]]$nDim) {
            newMinMax <- rhsRule$fullRange$getMinMax()
            varInfo[[rhsVar]]$mins <<- pmin(varInfo[[rhsVar]]$mins, newMinMax[, 1])
            varInfo[[rhsVar]]$maxs <<- pmax(varInfo[[rhsVar]]$maxs, newMinMax[, 2])
          }
        }
      }


      # Now use `dimensionsList`, to check / update varInfo.
      for (i in seq_along(dimensionsList)) {
        dimVarName <- names(dimensionsList)[i]
        if (!(dimVarName %in% names(varInfo))) next
        if (length(dimensionsList[[dimVarName]]) != varInfo[[dimVarName]]$nDim) {
          stop("inconsistent dimensions for variable `", dimVarName, "`.")
        }
        if (any(dimensionsList[[dimVarName]] < varInfo[[dimVarName]]$maxs &
          varInfo[[dimVarName]]$maxs < .Machine$integer.max)) { # Had changed to `< Inf` but that causes problems with dyn idx case.
          stop("dimensions specified are smaller than model specification for variable `", dimVarName, "`.")
        }
        varInfo[[dimVarName]]$maxs <<- dimensionsList[[dimVarName]]
      }

      # Check for maxs < mins; this would generally be from a model syntax error,
      # e.g., for(i in 1:4) y[k] ~ dnorm(0,1);
      # in some cases these would be caught by the check for mins or maxs zero or less,
      # but this error message is more informative.
      invalidRange <- sapply(varInfo, function(x) {
        length(x$mins) && length(x$maxs) &&
          any(x$mins > x$maxs)
      })
      if (any(invalidRange)) {
        problemVars <- which(invalidRange)
        stop(
          "indexing error found for model variable(s): `",
          paste0(names(varInfo)[problemVars], collapse = "`, `"),
          "`. Please check that variables used for indexing are properly defined in the relevant for loop(s). Also note that backwards indexing (e.g., `for(i in 5:1)`) is generally not supported."
        )
      }

      # Check for mins or maxs zero or less (these trigger various errors including R crashes).
      invalidMins <- sapply(varInfo, function(x) length(x$mins) && min(x$mins) < 1)
      invalidMaxs <- sapply(varInfo, function(x) length(x$maxs) && min(x$maxs) < 1)
      if (any(invalidMins) || any(invalidMaxs)) {
        problemVars <- c(which(invalidMins), which(invalidMaxs))
        stop(
          "index value of zero or less found for model variable(s): `",
          paste0(names(varInfo)[problemVars], collapse = "`, `"), "`."
        )
      }

      # Flag variables as being dynamically indexed.
      if (getNimbleModelOption("allowDynamicIndexing")) {
        nimFunNames <- getAllDistributionsInfo("namesExprList")
        for (i in seq_along(declInfo)) {
          for (p in seq_along(declInfo[[i]]$symbolicParentNodes)) {
            parentExpr <- stripIndexWrapping(declInfo[[i]]$symbolicParentNodes[[p]])
            dynamicIndices <- detectDynamicIndices(parentExpr)
            if (sum(dynamicIndices) && !any(sapply(declInfo, function(x) identical(x$targetExpr, parentExpr)))) {
              varInfo[[getVarName(parentExpr)]]$anyDynamicallyIndexed <<- TRUE
            }
          }
        }
      }
    },
    replaceDynamicIndexingInParents = function() {
      if (getNimbleModelOption("allowDynamicIndexing")) {
        for (i in seq_along(declInfo)) {
          declInfo[[i]]$replaceDynamicIndexingInParents(varInfo)
        }
      }
      invisible(NULL)
    },
    makeRHSoriginalRules = function() {
      for (i in seq_along(declInfo)) {
        declInfo[[i]]$makeRHSoriginalRules(constants)
      }
      invisible(NULL)
    },
    makeGraphRules = function() {
      for (i in seq_along(declInfo)) {
        declInfo[[i]]$makeGraphRules(constants)
      }
      invisible(NULL)
    },

    # Create calcRules and full sets of declRules and graphRules based on all declarations.
    makeGraphInfo = function() {
      declRules <<- lapply(declInfo, function(x) x$declRule)
      varNames <<- unique(lapply(declRules, function(rule) rule$varName))

      rhsOriginalRules <- unlist(lapply(declInfo, function(x) x$rhsOriginalRules))
      rhsOnlyRules <<- newVarRules(makeRHSonlyRules(rhsOriginalRules, declRules, constants))

      allDownstreamRules <- unlist(lapply(declInfo, function(x) x$downstreamRules))
      fromVarNames <- sapply(allDownstreamRules, function(rule) rule$fromVarName)
      downstreamRules <<- newVarRules(allDownstreamRules, fromVarNames)

      allUpstreamRules <- unlist(lapply(declInfo, function(x) x$upstreamRules))
      fromVarNames <- sapply(allUpstreamRules, function(rule) rule$fromVarName)
      upstreamRules <<- newVarRules(allUpstreamRules, fromVarNames)

      # Check for cycles. Need to use `initialCalcRules` rather than `declRules`
      # as `declRules` don't have `sortID`.

      initialCalcRules <- lapply(declRules, function(rule) {
        calcRuleClass$new(rule, NULL, NULL, rule$context, constants)
      })
      sapply(seq_along(initialCalcRules), function(i) initialCalcRules[[i]]$ID <- as.character(i))
      names(initialCalcRules) <- sapply(initialCalcRules, function(rule) rule$ID)

      setRelationships(initialCalcRules, downstreamRules)
      sorted <- setSortIDs(initialCalcRules)
      # At this point, we have a potential cyclic case if `sorted` is `FALSE`.

      # Do fracturing, but in potential cyclic case, do not fracture already-fractured nodes
      # to avoid very slow one-by-one carving off calcRules in state-space cases.

      # Start from scratch with clean set of `initialCalcRules`
      # (empty `sortID`, `parents`, `children` slots).
      initialCalcRules <- lapply(declRules, function(rule) {
        calcRuleClass$new(rule, NULL, NULL, rule$context, constants)
      })
      sapply(seq_along(initialCalcRules), function(i) initialCalcRules[[i]]$ID <- as.character(i))
      names(initialCalcRules) <- sapply(initialCalcRules, function(rule) rule$ID)

      allCalcRules <- makeCalcRules(initialCalcRules, rhsOriginalRules, downstreamRules,
        recurseFracturing = sorted, constants
      )
      sorted <- setSortIDs(allCalcRules)

      if (!sorted) { # SSM case
        # Handle standard SSM case of lag +1 or -1, with one or more calcRules in the cycle.

        # This inserts vectors of sortIDs for the calcRules in the cycle.
        allCalcRules <- processCyclicRules(allCalcRules, self)
        # Now assign remaining sortIDs (i.e., to various parent calcRules that formerly had `Inf` as `sortID`).
        sorted <- setSortIDs(allCalcRules)

        if (!sorted) { # Complicated SSM-type cases or true cycles.
          # Fully fracture to try to handle complicated SSM cases.
          messageIfVerbose("  [Note] Detected state-space type structure or cycle in model graph. Attempting to determine graph structure for non-cyclic cases. This may take some time. You may wish to alert the NIMBLE development team of your use case so that handling of such cases can be improved.")

          # Start from scratch with clean set of `initialCalcRules` (because elements of
          # `allCalcRules` are the same as elements of `initialCalcRules`),
          # meaning some `sortID` values have been modified.
          initialCalcRules <- lapply(declRules, function(rule) {
            calcRuleClass$new(rule, NULL, NULL, rule$context, constants)
          })
          sapply(seq_along(initialCalcRules), function(i) initialCalcRules[[i]]$ID <- as.character(i))
          names(initialCalcRules) <- sapply(initialCalcRules, function(rule) rule$ID)
          allCalcRules <- makeCalcRules(initialCalcRules, rhsOriginalRules, downstreamRules,
            recurseFracturing = TRUE
          )
          sorted <- setSortIDs(allCalcRules)
          if (!sorted) {
            stop("cycle found in model graph. NIMBLE does not allow cyclic models.")
          }
        }
      }

      # Set `top` and `end` flags in each calcRule.
      setEndRules(allCalcRules)
      setTopRules(allCalcRules)

      # Set up nested lists indexed by varName
      topRules <<- newVarRules(allCalcRules, type = "top")
      endRules <<- newVarRules(allCalcRules, type = "end")
      latentRules <<- newVarRules(allCalcRules, type = "latent")
      calcRules <<- newVarRules(allCalcRules)
      declRules <<- newVarRules(declRules)

      invisible(NULL)
    },
    makeVarNames = function() {
      varNames <<- c(names(varInfo), names(logProbVarInfo))
    },
    initializeContexts = function() {
      contextClassObject <- modelContextClass$new()
      contexts[[1]] <<- contextClassObject
      invisible(NULL)
    },
    warnRHSonlyDynamicIndexing = function() {
      # rhsOnlyRules are nested
      if (getNimbleModelOption("allowDynamicIndexing")) {
        for (i in seq_along(rhsOnlyRules)) {
          ind <- sapply(rhsOnlyRules[[i]]$rules, function(rule) rule$usedInIndex)
          if (sum(ind) && !rhsOnlyRules[[i]]$varName %in% names(declRules)) {
            varRangeChars <- sapply(rhsOnlyRules[[i]]$rules[ind], function(rule) {
              rule$fullRange$toChar()
            })
            messageIfVerbose(
              "  [Note] Detected use of non-constant indices: `", paste0(varRangeChars, collapse = "`, `"),
              "`.\n         For computational efficiency we recommend specifying these in `constants`."
            )
          }
        }
      }
      invisible(NULL)
    }
  )
)


# Core graph and node querying functions in the model API.
# These are standalone functions for now, but may become
# part of model class. That said, more naturally part of modelDef class.

# TODO: move these functions into a new stand-alone code file for user-facing functions?

# Note: `getDependencies` and `getParents` cannot handle `stochOnly` or `determOnly`
# because a given varRange result for getParents could be partially stochastic and
# partially deterministic. Instead a user would pass the result through `getNodes()`.
# Similarly, filtering by RHSonly will be done in `getNodes()`.

# Note: data-related flags not handled as that relates to flags on a model
# and not part of modelDef.

# TODO: these should presumably take the model not modelDef as the first arg.
# Once we integrate modelClass with modelBase_nClass, we should be able to
# pass `self` from the getDeps and getParents methods to these functions.

getDependencies <- function(modelDef, nodes,
                            self = TRUE,
                            downstream = FALSE, immediateOnly = FALSE) {
  traverseGraph(modelDef$downstreamRules, modelDef$declRules,
    nodes = nodes,
    down = TRUE, self = self,
    follow = downstream, immediateOnly = immediateOnly
  )
}

getParents <- function(modelDef, nodes,
                       self = FALSE,
                       upstream = FALSE, immediateOnly = FALSE) {
  traverseGraph(modelDef$upstreamRules, modelDef$declRules,
    nodes = nodes,
    down = FALSE, self = self,
    follow = upstream, immediateOnly = immediateOnly
  )
}


# Evaluates `if` statements in model code to generate actual model code
# without any `if` statements. Condition of if statement can use variables
# from the user's environment or from constants.
codeProcessIfThenElse <- function(code, constants, envir) {
  if (is.list(constants)) {
    constants <- list2env(constants, parent = envir)
  }

  codeLength <- length(code)
  if (is.name(code)) {
    stop("incomplete declaration found: '", safeDeparse(code), "'.")
  }

  if (code[[1]] == "{") {
    if (codeLength > 1) {
      for (i in 2:codeLength) {
        code[[i]] <- codeProcessIfThenElse(code[[i]], constants, envir)
      }
    }
    return(code)
  }

  if (code[[1]] == "for") {
    code[[4]] <- codeProcessIfThenElse(code[[4]], constants, envir)
    return(code)
  }

  if (code[[1]] == "if") {
    evaluatedCondition <- try(eval(code[[2]], constants), silent = TRUE)
    if (inherits(evaluatedCondition, "try-error")) {
      stop(
        "cannot evaluate condition of `if` statement: `",
        safeDeparse(code[[2]]),
        "`.\nCondition must be able to be evaluated based on values in `constants` or environment from which model is created."
      )
    }
    if (evaluatedCondition) {
      return(codeProcessIfThenElse(code[[3]], constants, envir))
    } else {
      if (length(code) == 4) {
        return(codeProcessIfThenElse(code[[4]], constants, envir))
      } else {
        return(quote({}))
      }
    }
  } else {
    return(code)
  }
}

addMissingIndexingRecurse <- function(code, dimensionsList) {
  if (!is.call(code)) {
    return(code)
  } # simple names or numbers
  if (code[[1]] != "[") {
    for (i in seq_along(code)) {
      code[[i]] <- addMissingIndexingRecurse(code[[i]], dimensionsList)
    }
    return(code)
  }

  # Code must be an indexing call, e.g. `x[.....]`.
  if (code[[1]] != "[") {
    stop("expecting a bracket, `[`, in `", safeDeparse(code), "`.")
  }

  # Handle cases like `covMat[1:5,1:5] <- eigen(constMat[1:5,])$vectors[1:5,1:5]%*%t(eigen(constMat[1:5,1:5])$vectors[,])`.
  if (length(code[[2]]) > 1 && code[[2]][[1]] == "$") {
    code[[2]][[2]] <- addMissingIndexingRecurse(code[[2]][[2]], dimensionsList)
    return(code)
  }

  # Handle cases like `(x[1:2]%*%y[1:2, i])[1,1]`.
  if (length(code[[2]]) > 1 && code[[2]][[1]] == "(") {
    code[[2]][[2]] <- addMissingIndexingRecurse(code[[2]][[2]], dimensionsList)
    # Handle missing indices within the indexing of an expression, e.g.,
    # the `k[ , 1]` in `(x[1:2,1:2]%*%y[1:2,1:2])[k[ , 1], ]`.
    len <- length(code)
    if (len > 2) {
      for (idx in 3:len) {
        if (is.call(code[[idx]])) {
          code[[idx]] <- addMissingIndexingRecurse(code[[idx]], dimensionsList)
        }
      }
    }
    return(code)
  }

  # We allow `myfun()[,1]`, similarly to `(x[1:2,1:2]%*%y[1:2,1:2])[,1]`.
  # Handle missing indices within the indexing of an expression as above;
  # handle the args of `myfun` in `myfun()[,1]`.
  if (is.call(code[[2]])) {
    len <- length(code[[2]])
    if (len > 1) {
      for (idx in 2:len) {
        code[[2]][[idx]] <- addMissingIndexingRecurse(code[[2]][[idx]], dimensionsList)
      }
    }
    len <- length(code)
    # Handle the indexing of `myfun()` in `myfun()[,1]`.
    if (len > 2) {
      for (idx in 3:len) {
        if (is.call(code[[idx]])) {
          code[[idx]] <- addMissingIndexingRecurse(code[[idx]], dimensionsList)
        }
      }
    }
    return(code)
  }

  # Dimension information was NOT provided for this variable.
  # Check to make sure all indices are present.
  if (!any(code[[2]] == names(dimensionsList))) {
    if (any(unlist(lapply(as.list(code), is.blank)))) {
      stop(
        "The model definition included the expression `", safeDeparse(code), "`, which contains missing indices.\n",
        "There are three options to resolve this:\n",
        "(1) Explicitly provide the missing indices in the model definition (e.g., `",
        safeDeparse(example_fillInMissingIndices(code)), "`).\n",
        "(2) Provide the dimensions of variable `", code[[2]], "` via the `dimensions` argument to `nimbleModel()`, e.g.,\n",
        "    `nimbleModel(code, dimensions = list(", code[[2]], " = ", safeDeparse(example_getMissingDimensions(code)), "`)).\n",
        "(3) Provide initial values for the variable `", code[[2]], "` via the `inits` argument to `nimbleModel()`."
      )
    }
    # and to recurse on all elements
    for (i in seq_along(code)) {
      code[[i]] <- addMissingIndexingRecurse(code[[i]], dimensionsList)
    }
    return(code)
  }

  # Dimension information WAS provided for this variable.
  if (any(code[[2]] == names(dimensionsList))) {
    dimensions <- dimensionsList[[as.character(code[[2]])]]
    # First, just check that the dimensionality of the node is consistent.
    if (length(code) != length(dimensions) + 2) {
      stop("inconsistent dimensionality provided for `", code[[2]], "`.")
    }
    # Then, fill in any missing indices, and recurse on all other elements.
    for (i in seq_along(code)) {
      if (is.blank(code[[i]])) {
        code[[i]] <- substitute(1:TOP, list(TOP = as.numeric(dimensions[i - 2])))
      } else {
        code[[i]] <- addMissingIndexingRecurse(code[[i]], dimensionsList)
      }
    }
    return(code)
  }
  stop("unable to process `", safeDeparse(code), "`.")
}

# Replace constants that involve no indexing with actual values of constants.
# E.g., `dnorm(x[N], sd)` , where `N` is a constant, gets `N` replaced.
# but `dnorm(x[blockID[i]], sd)`, where `i` is a for-loop index, does not get replaced at this step.
replaceConstantsRecurse <- function(code, constantsEnv, do.eval = TRUE) {
  cLength <- length(code)
  if (cLength == 1) {
    if (is.name(code)) {
      if (any(code == names(constantsEnv))) {
        if (do.eval) {
          origCode <- code
          code <- as.numeric(eval(code, constantsEnv))
          if (length(code) != 1) {
            messageIfVerbose("   [Warning] Code `", safeDeparse(origCode), "` was given as known but evaluates to a non-scalar. This is probably not what you want.")
          }
        }
        return(list(
          code = code,
          replaceable = TRUE
        ))
      }
      return(list(
        code = code,
        replaceable = FALSE
      ))
    }
    if (is.numeric(code) || is.logical(code)) {
      return(list(
        code = code,
        replaceable = TRUE
      ))
    }
  }
  if (is.call(code)) {
    if (code[[1]] == "[") {
      replacements <- lapply(
        code[-c(1, 2)],
        function(x) replaceConstantsRecurse(x, constantsEnv)
      )
      for (i in 1:length(replacements)) {
        code[[i + 2]] <- replacements[[i]]$code
      }
      replaceables <- unlist(lapply(replacements, function(x) x$replaceable))
      allReplaceable <- all(replaceables) & do.eval
      repVar <- replaceConstantsRecurse(code[[2]], constantsEnv, FALSE)
      code[[2]] <- repVar$code
      if (allReplaceable & repVar$replaceable) {
        testcode <- as.numeric(eval(code, constantsEnv))
        if (length(testcode) == 1) code <- testcode
      }
      return(list(
        code = code,
        replaceable = allReplaceable & repVar$replaceable
      ))
    }
    # A call that is not '['.
    if (cLength > 1) {
      if (as.character(code[[1]]) %in% c("<-", "~")) {
        replacements <- c(
          list(replaceConstantsRecurse(code[[2]], constantsEnv, FALSE)),
          lapply(code[-c(1, 2)], function(x) replaceConstantsRecurse(x, constantsEnv))
        )
        replacements[[1]]$replaceable <- FALSE
      } else {
        replacements <- lapply(code[-1], function(x) replaceConstantsRecurse(x, constantsEnv))
      }
      for (i in 1:length(replacements)) {
        code[[i + 1]] <- replacements[[i]]$code
      }
      replaceables <- unlist(lapply(replacements, function(x) x$replaceable))
      allReplaceable <- all(replaceables)
    } else {
      allReplaceable <- TRUE
    }
    if (allReplaceable) {
      if (!any(code[[1]] == getAllDistributionsInfo("namesVector"))) {
        callChar <- as.character(code[[1]])
        if (exists(callChar, constantsEnv)) {
          if (!is.vectorized(code)) {
            if (is.null(neverReplaceable[[callChar]])) {
              if (isTRUE(callChar %in% nimblePreevaluationFunctionNames)) {
                if (inherits(get(callChar, constantsEnv), "function")) {
                  testcode <- as.numeric(eval(code, constantsEnv))
                  if (length(testcode) == 1) code <- testcode
                }
              }
            }
          }
        }
      }
    }
    return(list(code = code, replaceable = allReplaceable))
  }
  stop("unable to process `", safeDeparse(code), "`.")
}

neverReplaceable <- list(
  # Only the names matter, any non-null value will do.
  chol = TRUE,
  inverse = TRUE,
  CAR_calcNumIslands = TRUE,
  CAR_calcC = TRUE,
  CAR_calcM = TRUE,
  CAR_calcEVs2 = TRUE,
  CAR_calcEVs3 = TRUE
)

liftedCallsDoNotAddIndexing <- c(
  "CAR_calcNumIslands"
)

liftedCallsGetIndexingFromArgumentNumbers <- list(
  CAR_calcC = c(1),
  CAR_calcM = c(1),
  CAR_calcEVs2 = c(2),
  CAR_calcEVs3 = c(3)
)

example_fillInMissingIndices <- function(code) {
  as.call(lapply(as.list(code), function(el) {
    if (is.blank(el)) quote(1:10) else el
  }))
}

example_getMissingDimensions <- function(code) {
  cCall <- quote(c())
  for (i in seq_along(code)[-c(1, 2)]) {
    cCall[[i - 1]] <- parse(text = paste0("dim", i - 2, "_max"))[[1]]
  }
  return(cCall)
}

# Determines whether a parameter expression should be lifted to a new declaration.
isExprLiftable <- function(paramExpr, type = NULL) {
  if (is.name(paramExpr)) {
    return(FALSE)
  }
  if (is.numeric(paramExpr)) {
    return(FALSE)
  }
  if (is.logical(paramExpr)) {
    stop("not expecting a logical/boolean value; please use a numeric value in place of `", paramExpr, "`.")
  }
  if (is.call(paramExpr)) {
    callText <- getCallText(paramExpr)
    if (callText %in% names(neverReplaceable)) {
      return(TRUE)
    } # Special calls that are not lifted.
    if (length(paramExpr) == 1) {
      return(FALSE)
    } # Don't lift function calls with no arguments.
    if (callText == "[") {
      return(FALSE)
    } # Don't lift simply indexed expressions: , `x[...]`.
    nDim <- type[["nDim"]]
    if (is.numeric(nDim)) {
      if (nDim > 0) {
        return(FALSE)
      }
    } # Beyond above cases, don't lift non-scalar arguments.
    if (is.vectorized(paramExpr)) {
      return(FALSE)
    } # Don't lift any expression with vectorized indexing, `funName(x[1:10])`.
    return(TRUE)
  }
  stop("cannot process this parameter expression: `", safeDeparse(paramExpr), "`.")
}

addNecessaryIndexingToNewNode <- function(newNodeNameExpr, paramExpr, indexVarExprs) {
  if (is.call(paramExpr) && safeDeparse(paramExpr[[1]], warn = TRUE) %in% names(liftedCallsGetIndexingFromArgumentNumbers)) {
    return(addNecessaryIndexingFromArgumentNumbers(newNodeNameExpr, paramExpr, indexVarExprs))
  }
  usedIndexVarsList <- indexVarExprs[indexVarExprs %in% all.vars(paramExpr)] # This extracts any index variables that appear in `paramExpr`.
  vectorizedIndexExprsList <- extractAnyVectorizedIndexExprs(paramExpr) # Creates a list of any vectorized (:) indexing expressions appearing in `paramExpr`.
  neededIndexExprsList <- c(usedIndexVarsList, vectorizedIndexExprsList)
  if (length(neededIndexExprsList) == 0) {
    return(newNodeNameExpr)
  } # No index variables, or vectorized indexing, return the (un-indexed) name expression.
  newNodeNameExprIndexed <- substitute(NAME[], list(NAME = newNodeNameExpr))
  newNodeNameExprIndexed[3:(2 + length(neededIndexExprsList))] <- neededIndexExprsList
  return(newNodeNameExprIndexed)
}

addNecessaryIndexingFromArgumentNumbers <- function(newNodeNameExpr, paramExpr, indexVarExprs) {
  paramExprCallName <- as.character(paramExpr[[1]])
  argNumbers <- liftedCallsGetIndexingFromArgumentNumbers[[paramExprCallName]]
  argList <- as.list(paramExpr[argNumbers + 1]) # +1 to skip past the function name (first element).
  neededIndexExprsList <- lapply(argList, function(x) x[[3]])
  newNodeNameExprIndexed <- substitute(NAME[], list(NAME = newNodeNameExpr))
  newNodeNameExprIndexed[3:(2 + length(neededIndexExprsList))] <- neededIndexExprsList
  return(newNodeNameExprIndexed)
}

extractAnyVectorizedIndexExprs <- function(expr) {
  if (!(":" %in% all.names(expr))) {
    return(list())
  }
  if (!is.call(expr)) {
    return(list())
  }
  if (expr[[1]] == ":") {
    return(expr)
  }
  ret <- unlist(lapply(expr[-1], function(i) extractAnyVectorizedIndexExprs(i)))
  if (is.null(ret)) {
    return(list())
  } else {
    return(ret)
  }
}

checkForDuplicateNodeDeclaration <- function(newNodeCode, newNodeNameExprIndexed, newDeclInfo) {
  for (i in seq_along(newDeclInfo)) {
    if (identical(newNodeNameExprIndexed, newDeclInfo[[i]]$targetExpr)) {
      # We've found a node declaration with exactly the same LHS, which is a mangling of the RHS during lifting.
      if (!identical(newNodeCode, newDeclInfo[[i]]$code)) stop("error in processing `", safeDeparse(newNodeCode), "`.")
      return(TRUE) # Indicate that we found a matching node declaration.
    }
  }
  return(FALSE) # A duplicate node entry was *not* found.
}


# Checks if distribution is defined and if not, attempts to register it.
checkUserDefinedDistribution <- function(code, userEnv) {
  dist <- as.character(code[[3]][[1]])
  if (dist %in% c("T", "I")) {
    dist <- as.character(code[[3]][[2]][[1]])
  }
  if (!dist %in% distributions$namesVector) {
    if (!exists("distributions", nimbleUserNamespace, inherits = FALSE) ||
      !dist %in% nimbleUserNamespace$distributions$namesVector) {
      messageIfVerbose("  [Note] Registering `", dist, "` as a distribution based on its use in model code. If you make changes to the nimbleFunctions for the distribution, you must call `deregisterDistributions` before using the distribution in model code for those changes to take effect.")
      registerDistributions(dist, userEnv)
    }
  }
}


replaceDistributionAliases <- function(code) {
  if (length(code) < 3) {
    stop("invalid model declaration: `", safeDeparse(code), "`.")
  }
  if (!is.call(code[[3]])) {
    stop("invalid model declaration: `", safeDeparse(code), "` must call a density function.")
  }
  dist <- as.character(code[[3]][[1]])
  trunc <- FALSE
  if (dist %in% c("T", "I")) {
    dist <- as.character(code[[3]][[2]][[1]])
    trunc <- TRUE
  }
  if (dist %in% names(distributionAliases)) {
    dist <- as.name(distributionAliases[dist])
    if (trunc) code[[3]][[2]][[1]] <- dist else code[[3]][[1]] <- dist
  }
  return(code)
}

checkForDeterministicDorR <- function(code) {
  if (is.call(code[[3]])) {
    drFuns <- c(distribution_dFuns, distribution_rFuns)
    if (exists("distributions", nimbleUserNamespace, inherits = FALSE)) {
      dFunsUser <- get("namesVector", nimbleUserNamespace$distributions)
      drFuns <- c(drFuns, dFunsUser, paste0("r", stripPrefix(dFunsUser)))
    }
    if (as.character(code[[3]][[1]]) %in% drFuns) {
      messageIfVerbose("  [Warning] Model includes deterministic assignment using '<-' of the result of a density ('d') or simulation ('r') calculation. This is likely not what you intended in: `", safeDeparse(code), "`.")
    }
  }
  return(NULL)
}


# A small class for information deduced about a variable in a model.
varInfoClass <- R6Class(
  "varInfoClass",
  portable = FALSE,
  public = list(
    varName = "ANY",
    mins = "ANY",
    maxs = "ANY",
    nDim = "ANY",
    anyStoch = "ANY",
    anyDynamicallyIndexed = "ANY",
    initialize = function(varName, mins, maxs, nDim, anyStoch, anyDynamicallyIndexed = FALSE) {
      varName <<- varName
      mins <<- mins
      maxs <<- maxs
      nDim <<- nDim
      anyStoch <<- anyStoch
      anyDynamicallyIndexed <<- anyDynamicallyIndexed
    }
  )
)
