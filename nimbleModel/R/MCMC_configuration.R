## TODO: conjugacyRelationshipsObject$checkConjugacy
## getNodeSize (see toNodeChars code for ideas)

## Placeholders:
sampler_binary <- sampler_prior_samples <- sampler_posterior_predictive <- sampler_binary <- sampler_categorical <- sampler_RW <- sampler_RW_block <- sampler_RW_llFunction <- sampler_slice <- sampler_ess <- sampler_AF_slice <- sampler_crossLevel <- sampler_RW_llFunction_block <- sampler_RW_dirichlet <- sampler_RW_wishart <- sampler_RW_lkj_corr_cholesky <- sampler_RW_block_lkj_corr_cholesky <- sampler_CAR_normal <- sampler_CAR_proper <- function() {}

## NOTE: samplers will be assigned to entire nodeRanges by default.
## Samplers that block across elements of multiple nodes will be assigned by passing in one or more varRanges.


samplerConfClass <- R6Class(
  classname = "samplerConfClass",
  portable = FALSE,
  public = list(
    name = NULL,
    samplerFunction = NULL,
    baseClassName = NULL,
    target = NULL,
    control = NULL,
    ## NOTE: for conjugate samplers, the dependence info in `control` will be for deps of the first node in the target nodeRange
    initialize = function(name, samplerFunction, target, control, model) {
      baseClassName <<- environment(environment(samplerFunction)$contains)$className
      ## if(is.null(baseClassName) || (baseClassName != 'sampler_BASE')) warning('MCMC sampler nimbleFunctions should inherit from (using "contains" argument) base class sampler_BASE.')  ## Placeholder: inactivated for now
      name <<- name
      samplerFunction <<- samplerFunction
      target <<- target
      control <<- control
      if (name == "crossLevel") {
        control <<- c(
          control,
          list(dependent_nodes = getNodes(model, getDependencies(model$modelDef, target, self = FALSE), stochOnly = TRUE))
        )
      } ## special case for printing dependents of crossLevel sampler (only)
    },
    buildSampler = function(model, mvSaved) {
      samplerFunction(model = model, mvSaved = mvSaved, target = target, control = control)
    },
    toStr = function(displayControlDefaults = FALSE, displayNonScalars = FALSE, displayConjugateDependencies = FALSE) {
      tempList <- list()
      tempList[[paste0(name, " sampler")]] <- paste0(target, collapse = ", ")
      infoList <- c(tempList, control)
      mcmc_listContentsToStr(infoList, displayControlDefaults, displayNonScalars, displayConjugateDependencies)
    },
    show = function() {
      cat(toStr())
    }
  )
)

mcmcConfClass <- R6Class(
  classname = "mcmcConfClass",
  portable = FALSE,
  public = list(
    model = NULL,
    samplerConfs = NULL,
    samplerExecutionOrder = NULL,
    controlDefaults = NULL,
    initialize = function(model, nodes, control = list(), useConjugacy = TRUE, ...) {
      model <<- model
      samplerConfs <<- list()
      samplerExecutionOrder <<- numeric()
      controlDefaults <<- list(...)
      for (i in seq_along(control)) controlDefaults[[names(control)[i]]] <<- control[[i]]

      if (missing(nodes)) {
        nodes <- NULL
      }

      ## Splitting into top and latent nodes results in homogeneous nodeRanges in terms of children (and parents,
      ## though that is not relevant here) of a given nodeRange.
      nodeRanges <- c(
        getNodes(model, nodes = nodes, stochOnly = TRUE, includeData = FALSE, topOnly = TRUE),
        getNodes(model, nodes = nodes, stochOnly = TRUE, includeData = FALSE, latentOnly = TRUE)
      )

      ## FIX: could pre-allocate samplers rather than having `addOneSampler` grow the list of confs.
      for (i in seq_along(nodeRanges)) {
        addDefaultSampler(
          nodeRange = nodeRanges[[i]],
          useConjugacy = useConjugacy
        )
      }
    },
    addDefaultSampler = function(nodeRange, control = list(), useConjugacy = TRUE, ...) {
      ## TODO: this doesn't deal with CRP or predictive nodes.

      ## CHECK: this assumes all elements in nodeRange can be assigned the same sampler.
      ## Should be fine in general for non-conjugate as those are based on prior.
      ## For conjugate, this assumes we have distinguished top from latent nodes, as the
      ## process of doing so should create nodeRanges with elements with identical child structure.

      controlDefaultsArg <- list(...)
      for (i in seq_along(control)) controlDefaultsArg[[names(control)[i]]] <- control[[i]]

      if (useConjugacy) {
        conjugacyResult <- nimbleModel:::conjugacyRelationshipsObject$checkConjugacy(model, nodeRange)
        if (length(conjugacyResult)) {
          addConjugateSampler(
            conjugacyResult = conjugacyResult,
            dynamicallyIndexed = model$modelDef$varInfo[[nodeRange$varName]]$anyDynamicallyIndexed
          )
          ## CHECK: should we use getVarName(nodeRange) (could nodeRange be character?)
          return()
        }
      }

      dist <- model$getDistribution(nodeRange)

      if (model$isMultivariate(nodeRange)) {
        if (dist == "dmulti") stop("  [Error] Support for sampling multinomial distributions was removed from nimble due to incorrect sampling results.\n  Please contact nimble developers at the nimble-users google group or at nimble.stats@gmail.com to let them know this happened.  \n Thank you.", call. = FALSE)
        if (dist == "ddirch") {
          addSampler(target = nodeRange, type = "RW_dirichlet", control = controlDefaultsArg)
          return()
        }
        if (dist == "dwish") {
          addSampler(target = nodeRange, type = "RW_wishart", control = controlDefaultsArg)
          return()
        }
        if (dist == "dinvwish") {
          addSampler(target = nodeRange, type = "RW_wishart", control = controlDefaultsArg)
          return()
        }
        if (dist == "dlkj_corr_cholesky") {
          nodeSize <- nodeRange$getNodeSize()
          if (nodeSize >= 9) {
            addSampler(target = nodeRange, type = "RW_block_lkj_corr_cholesky", control = controlDefaultsArg)
          } else {
            if (nodeSize == 4) {
              addSampler(target = nodeRange, type = "RW_lkj_corr_cholesky", control = controlDefaultsArg) ## only a scalar free param in 2x2 case
            } else {
              warning("Not assigning sampler to dlkj_corr_cholesky node for 1x1 case.")
            }
          }
          return()
        }
        if (dist == "dcar_normal") {
          addSampler(target = nodeRange, type = "CAR_normal", control = controlDefaultsArg)
          return()
        }
        if (dist == "dcar_proper") {
          addSampler(target = nodeRange, type = "CAR_proper", control = controlDefaultsArg)
          return()
        }
        addSampler(target = nodeRange, type = "RW_block", control = controlDefaultsArg)
      }

      ## if node is discrete 0/1 (binary), assign 'binary' sampler
      if (model$isBinary(nodeRange)) {
        addSampler(target = nodeRange, type = "binary", control = controlDefaultsArg)
        invisible(NULL)
      }

      ## for categorical nodes, assign a 'categorical' sampler
      if (dist == "dcat") {
        addSampler(target = nodeRange, type = "categorical", control = controlDefaultsArg)
        invisible(NULL)
      }

      ## if node distribution is discrete, assign 'slice' sampler
      if (model$isDiscrete(nodeRange)) {
        addSampler(target = nodeRange, type = "slice", control = controlDefaultsArg)
        invisible(NULL)
      }

      addSampler(target = nodeRange, type = "RW", control = controlDefaultsArg)
    },
    addConjugateSampler = function(conjugacyResult, dynamicallyIndexed = FALSE) {
      prior <- conjugacyResult$prior
      ## TODO: how will dependents be specified?
      dependentCounts <- sapply(conjugacyResult$control, length)
      names(dependentCounts) <- gsub("^dep_", "", names(dependentCounts))
      ## We have separate sampler functions for the same conjugacy
      ## when there are both dynamically and non-dynamically indexed
      ## nodes with that conjugacy, as the dynamically-indexed
      ## sampler has a check for zero contributions for each dependent
      conjSamplerName <- createDynamicConjugateSamplerName(prior = prior, dependentCounts = dependentCounts, dynamicallyIndexed = dynamicallyIndexed)
      ## Placeholder: do not generate sampler functions for now.
      ## if(!dynamicConjugateSamplerExists(conjSamplerName)) {
      ##     conjSamplerDef <- conjugacyRelationshipsObject$generateDynamicConjugateSamplerDefinition(prior = prior, dependentCounts = dependentCounts, doDependentScreen = dynamicallyIndexed)
      ##     dynamicConjugateSamplerAdd(conjSamplerName, conjSamplerDef)
      ## }
      conjSamplerFunction <- function() {} # dynamicConjugateSamplerGet(conjSamplerName) # Placeholder
      nameToPrint <- gsub("^sampler_", "", conjSamplerName)
      addSampler(target = conjugacyResult$target, type = conjSamplerFunction, control = conjugacyResult$control, name = nameToPrint)
    },
    addSampler = function(target, type = "RW", control = list(), targetAsScalars = FALSE, name, ...) {
      ## `target` should be one or more strings, or a single varRange/nodeRange
      ## or a list of varRanges or nodeRanges.

      ## nodeRanges are assigned separate, identical samplers to each element in the node.
      ## (This mimics current `targetByNode = TRUE`.) If one wants the alternative,
      ## one can pass in a varRange rather than a nodeRange.
      ## varRanges are assigned a single sampler, unless `targetAsScalars = TRUE`.
      ## character strings are converted to varRanges and handled as above.
      ## CHECK: might consider other approaches.
      ## NOTE: to assign a single block sampler to multiple nodes in a nodeRange, would need to pass in a varRange or char.

      nameProvided <- !missing(name)

      if (is.character(target)) {
        target <- lapply(target, function(x) varRangeClass$new(x))
        if (length(target) == 1) target <- target[[1]]
      } else if (!(inherits(target, "varRangeClass") || all(sapply(target, function(x) inherits(x, "varRangeClass"))))) {
        stop("`target` must be one more character strings, `varRange`s or `nodeRange`s")
      }
      if (is.character(type)) {
        if (type == "conjugate") {
          if (is.list(target) || !inherits(target, "nodeRangeClass")) {
            stop("Can only assign conjugate sampler to a single nodeRange")
          }
          conjugacyResult <- nimbleModel:::conjugacyRelationshipsObject$checkConjugacy(model, target)
          if (!is.null(conjugacyResult)) {
            return(addConjugateSampler(
              conjugacyResult = conjugacyResult,
              dynamicallyIndexed = model$modelDef$varInfo[[target$varName]]$anyDynamicallyIndexed
            ))
          }
          stop("Cannot assign conjugate sampler to non-conjugate node: `", target, "`")
        }

        if (targetAsScalars) {
          if (is.list(target)) {
            stop("Cannot provide multiple inputs when `targetAsScalars = TRUE`")
          }

          ## For efficiency, first see if can be treated as a single nodeRange.
          targetNodes <- getNodes(model, target)
          ## Otherwise, expand (inefficiently).
          if (length(targetNodes) > 1 || model$isMultivariate(targetNodes[[1]])) {
            target <- lapply(target$toVarChars(expandScalars = TRUE), function(x) varRangeClass$new(x))
          } else {
            target <- targetNodes[[1]]
          }
        }

        thisSamplerName <- if (nameProvided) name else gsub("^sampler_", "", type) ## removes 'sampler_' from beginning of name, if present
        if (thisSamplerName == "RW_block") {
          messageIfVerbose('  [Note] Assigning an RW_block sampler to nodes with very different scales can result in low MCMC efficiency.  If all nodes assigned to RW_block are not on a similar scale, we recommend providing an informed value for the \"propCov\" control list argument, or using the AFSS sampler instead.')
        }
        if (exists(type, inherits = TRUE)) { #  && is.nfGenerator(eval(as.name(type)))) {   ## try to find sampler function 'type'
          samplerFunction <- function() {} # eval(as.name(type))  # Placeholder
        } else {
          sampler_type <- paste0("sampler_", type) ## next, try to find sampler function 'sampler_type'
          if (exists(sampler_type)) { #  && is.nfGenerator(eval(as.name(sampler_type)))) {   ## try to find sampler function 'sampler_type'
            samplerFunction <- function() {} # eval(as.name(sampler_type))
          } else {
            stop(paste0("cannot find sampler type '", type, "'"))
          }
        }
      } else if (is.function(type)) {
        if (nameProvided) {
          thisSamplerName <- name
        } else {
          typeArg <- substitute(type)
          if (is.name(typeArg)) {
            thisSamplerName <- gsub("^sampler_", "", deparse(typeArg))
          } else {
            thisSamplerName <- "custom_function"
          }
        }
        samplerFunction <- type
      } else {
        stop("sampler type must be character name or function")
      }
      if (!is.character(thisSamplerName)) stop("sampler name should be a character string")
      if (!is.function(samplerFunction)) stop("sampler type does not specify a function")

      controlArgs <- c(control, list(...))
      thisControlList <- mcmc_generateControlListArgument(control = controlArgs, controlDefaults = controlDefaults) ## should name arguments

      if (targetAsScalars && !inherits(target, "nodeRangeClass")) {
        tmp <- sapply(target, function(x) addOneSampler(thisSamplerName, samplerFunction, x, thisControlList))
      } else {
        addOneSampler(thisSamplerName, samplerFunction, target, thisControlList)
      }
      invisible(samplerConfs)
    },
    addOneSampler = function(thisSamplerName, samplerFunction, targetOne, thisControlList) {
      newSamplerInd <- length(samplerConfs) + 1
      samplerConfs[[newSamplerInd]] <<- samplerConfClass$new(name = thisSamplerName, samplerFunction = samplerFunction, target = targetOne, control = thisControlList, model = model)
      samplerExecutionOrder <<- c(samplerExecutionOrder, newSamplerInd)
    }
  )
)


mcmc_generateControlListArgument <- function(control, controlDefaults) {
  if (missing(control)) control <- list()
  if (missing(controlDefaults)) controlDefaults <- list()
  thisControlList <- controlDefaults ## start with all the defaults
  thisControlList[names(control)] <- control ## add in any controls provided as an argument
  return(thisControlList)
}
