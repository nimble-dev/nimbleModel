# Determine nodes of interest, potentially of particular types.
# Incorporates functionality formerly in `getNodeNames` and `expandNodeNames`
getNodes <- function(model, nodes,
                     determOnly = FALSE, stochOnly = FALSE,
                     includeData = TRUE, dataOnly = FALSE,
                     includeRHSonly = FALSE,
                     topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE,
                     includePredictive = TRUE, predictiveOnly = FALSE,
                     nodesAsChars = getNimbleModelOption('nodesAsChars'),
                     returnScalarComponents = FALSE,
                     .sort = FALSE
                     ) {
  if(!missing(nodes) && is.null(nodes)) return(nodes)
  # A single nodeRange can have elements that don't share a sortID when converted to calcRange representation,
  # so we can't sort nodeRanges.
  if (.sort && !nodesAsChars)
    warning("`.sort=TRUE` is provided only for back compatibility and requires the use of character representations of nodes")
  # Similarly for scalar components.
  if (returnScalarComponents && !nodesAsChars) {
    warning("`returnScalarComponents=TRUE` requires the use of character representations of nodes")
    nodesAsChars = TRUE
  }
    
  # `nodes` may contain one or more varRanges or varNames.
  if (topOnly + latentOnly + endOnly > 1) {
    stop("only one of `topOnly`, `latentOnly`, `endOnly` can be `TRUE`.")
  }

  if (missing(nodes)) {
    nodes <- names(model$modelDef$declRules)
    if (includeRHSonly && !stochOnly && !determOnly) {
      nodes <- unique(c(nodes, names(model$modelDef$rhsOnlyRules)))
    }
  } else {
    if (inherits(nodes, "varRangeClass")) {
      nodes <- list(nodes)
    }
    if (!all(sapply(nodes, function(node) is.character(node) || inherits(node, "varRangeClass")))) {
      stop("`nodes` must be variable names or `varRange`s.")
    }
  }

  # Filter out; result is varRanges so do this before applying later rules, which produce nodeRanges.
  if (dataOnly) {
    nodes <- flatten(lapply(nodes, function(node) applyRules(model$dataRules, node)))
  }
  if (!includeData) {
    dataNodes <- sapply(nodes, getVarName) %in% names(model$dataRules)
    nodes <- c(nodes[!dataNodes], flatten(lapply(
      nodes[dataNodes],
      function(node) applyRules(model$nondataRules, node)
    )))
  }

  if (predictiveOnly) {
    nodes <- flatten(lapply(nodes, function(node) applyRules(model$predictiveRules, node)))
  }
  if (!includePredictive) {
    nodes <- flatten(lapply(nodes, function(node) applyRules(model$nonpredictiveRules, node)))
  }

  if (!topOnly && !latentOnly && !endOnly) {
    result <- lapply(nodes, function(node) applyRules(model$modelDef$declRules, node))
  }    
    
  if (topOnly) result <- lapply(nodes, function(node) applyRules(model$modelDef$topRules, node))
  if (latentOnly) result <- lapply(nodes, function(node) applyRules(model$modelDef$latentRules, node))
  if (endOnly) result <- lapply(nodes, function(node) applyRules(model$modelDef$endRules, node))

  result <- flatten(result) # Flatten the result so don't have nested list.

  if (length(result)) {
    if (stochOnly) {
      result <- result[sapply(result, function(nodeRange) nodeRange$decl$stoch)]
    }
    if (determOnly) {
      result <- result[!sapply(result, function(nodeRange) nodeRange$decl$stoch)]
    }
  }

  if (includeRHSonly && !stochOnly && !determOnly) { # RHSonly are considered neither determ not stoch.
    rhsResult <- lapply(nodes, function(node) applyRules(model$modelDef$rhsOnlyRules, node))
    if (!.sort) 
      result <- c(result, flatten(rhsResult))  
  }

  if (.sort) {
    # Ordering is only relevant at calcRange stage and a single nodeRange can contain
    # elements with various sortIDs, so we convert to nodeChars first and then get their
    # sortID by creating a temporary calcRange for each.
    nodeChars <- unlist(sapply(result, \(x) x$toNodeChars()))
    
    calcRanges <- unlist(lapply(nodeChars, function(node) {
      lapply(model$modelDef$calcRules[[getVarName(node)]]$rules, function(rule) {
        rule$makeCalcRange(rule$apply(node))
      })
    }))
    if (length(nodeChars) != length(calcRanges))
      stop("unexpected mismatch between node character representation and calcRanges in `getNodes` sorting")
    ord <- order(sapply(calcRanges, \(x) x$sortID))
    result <- nodeChars[ord]
    if (includeRHSonly)
      result <- c(sapply(flatten(rhsResult), \(x) x$toNodeChars()), result)
    if (returnScalarComponents)
      result <- lapply(result, \(x) varRangeClass$new(x)$toVarChars(expandScalars = TRUE))
    result <- unlist(result)
  } else {
    if (nodesAsChars) {
      if (returnScalarComponents) {
        result <- lapply(result, \(x) x$toVarRange()$toVarChars(expandScalars = TRUE))
      } else result <- lapply(result, \(x) x$toNodeChars())
      result <- unlist(result)
    }
  }   
  if (!length(result)) {
    return(NULL)
  }
  return(result)
}

# Provided for backward compatibility.
#' @export
getNodeNames <- function(model, determOnly = FALSE, stochOnly = FALSE,
                         includeData = TRUE, dataOnly = FALSE, includeRHSonly = FALSE,
                         topOnly = FALSE, latentOnly = FALSE, endOnly = FALSE,
                         includePredictive = TRUE, predictiveOnly = FALSE,
                         returnType = "names",
                         returnScalarComponents = FALSE) {
  if (returnType != "names")
    stop("In nimble2, one can only request 'names' as the `returnType`")
  return(getNodes(model, determOnly=determOnly, stochOnly=stochOnly,
                  includeData=includeData, dataOnly=dataOnly,
                  includeRHSonly=includeRHSonly, topOnly=topOnly, latentOnly=latentOnly, endOnly=endOnly,
                  includePredictive=includePredictive, predictiveOnly=predictiveOnly,
                  nodesAsChars = TRUE,
                  returnScalarComponents=returnScalarComponents, .sort = TRUE))
}

# Provided for backward compatibility. 
# Need a test case where unique=FALSE retains duplicates.
# This should not do any exclusions of nodes based on types.
#' @export
expandNodeNames <- function(model, nodes, returnScalarComponents = FALSE,
                            returnType = "names", sort = FALSE, unique = TRUE) {
  if (returnType != "names")
    stop("In nimble2, one can only request 'names' as the `returnType`")
  result <- getNodes(model, nodes, includeRHSonly = TRUE, nodesAsChars = TRUE,
                     returnScalarComponents = returnScalarComponents, .sort = sort)
  if (unique) result <- unique(result)
  return(result)
}

`[.R6` <- function(x, ...) {
  x$`[`(...)
}

`[<-.R6` <- function(x, ...) {
  x$`[<-`(...)
}

taggedClass <- R6Class(
  classname = "taggedClass",
  portable = TRUE,  # Needed for `[` operator.
  public = list(
    tagged = logical(),
    num = NULL,
    initialize = function(declRule, init = FALSE) {
      if(length(declRule$indexSlotToSet)) {
        # TODO: change to `extractIndexRange()` when pick up `master`.
        self$num <- declRule$fullRange$extractIndexRange(seq_along(declRule$fullRange$indexSlotToRange))$numElements
      } else self$num <- 1
      self$tagged <- rep(init, self$num)
    },
    tag = function(IDs = NULL) {
      if(is.null(IDs)) {
        self$tagged <- rep(as.logical(TRUE), self$num)
      } else self$tagged[IDs] <- TRUE
      invisible(NULL)
    },
    untag = function(IDs = NULL) {
      if(is.null(IDs)) {
        self$tagged <<- rep(as.logical(FALSE), self$num)
      } else self$tagged[IDs] <<- FALSE
      invisible(NULL)
    },
    `[` = function(idx) {
      return(self$tagged[idx])
    },
    `[<-` = function(idx, value) {  # Provided for completeness but doesn't protect against invalid inputs.
      if(!is.logical(value)) stop("non-logical value used in replacement in `taggedClass`")
      self$tagged[idx] <- value
      return(self)
    }
  )
)

# This may not optimally aggregate in cases without contiguity - e.g., 2:4 + 6:8 will become a matrix, even though
# it may be more efficient to leave it as two nodeRanges.
#' @export
aggregate_nodes <- function(nodeSet) {
  if(!length(nodeSet)) return(nodeSet)
  if(is.character(nodeSet) || !is.list(nodeSet)) 
    stop("`nodeSet` must be a list of nodeRanges")
  declIDs <- sapply(nodeSet, \(x) x$decl$declRule$ID)
  nodeIDs <- lapply(nodeSet, \(x) x$getIDs())
  IDsByDecl <- lapply(split(nodeIDs,declIDs),\(x) unique(nimbleModel:::flatten(x)))
  nms <- names(IDsByDecl)
  newNodeSet <- lapply(seq_along(IDsByDecl), \(i) {
    whichDecl <- match(nms[i], declIDs)
    decl <- nodeSet[[whichDecl]]$decl
    return(decl$declRule$originalIndexingRule$apply_reverse(  
      decl$declRule$getOriginalIndexing(IDsByDecl[[i]]), decl))
  })
  return(newNodeSet)    
}
                      
#' @export
intersect_nodes <- function(nodeSet1, nodeSet2) {
  if(!length(nodeSet1) || !length(nodeSet2)) return(list())
  if(is.character(nodeSet1) || is.character(nodeSet2) ||
       !is.list(nodeSet1) || !is.list(nodeSet2))
    stop("`nodeSet1` and `nodeSet2` must be lists of nodeRanges")
  declIDs1 <- sapply(nodeSet1, \(x) x$decl$declRule$ID)
  declIDs2 <- sapply(nodeSet2, \(x) x$decl$declRule$ID)
  nodeIDs1 <- lapply(nodeSet1, \(x) x$getIDs())
  nodeIDs2 <- lapply(nodeSet2, \(x) x$getIDs())
  newNodeSet1 <- list(); length(newNodeSet1) <- length(nodeSet1)
  for(i in seq_along(nodeSet1)) {
    keepNodeIDs <- unique(unlist(lapply(seq_along(nodeIDs2), \(j) {
      if(declIDs1[i] == declIDs2[j]) intersect(nodeIDs1[[i]], nodeIDs2[[j]]) else NULL
    })))
    if(length(keepNodeIDs)) {
      newNodeSet1[[i]] <- nodeSet1[[i]]$decl$declRule$originalIndexingRule$apply_reverse(  
        nodeSet1[[i]]$decl$declRule$getOriginalIndexing(keepNodeIDs), nodeSet1[[i]]$decl)
    } 
  }
  return(newNodeSet1[!sapply(newNodeSet1, is.null)])
}

#' @export
setdiff_nodes <- function(nodeSet1, nodeSet2) {
  if(!length(nodeSet1) || !length(nodeSet2)) return(nodeSet1)
  if(is.character(nodeSet1) || is.character(nodeSet2) ||
       !is.list(nodeSet1) || !is.list(nodeSet2))
    stop("`nodeSet1` and `nodeSet2` must be lists of nodeRanges")
  declIDs1 <- sapply(nodeSet1, \(x) x$decl$declRule$ID)
  declIDs2 <- sapply(nodeSet2, \(x) x$decl$declRule$ID)
  nodeIDs1 <- lapply(nodeSet1, \(x) x$getIDs())
  nodeIDs2 <- lapply(nodeSet2, \(x) x$getIDs())
  excludeNodeIDs <- lapply(split(nodeIDs2,declIDs2),\(x) unique(nimbleModel:::flatten(x))) 
  newNodeSet1 <- list(); length(newNodeSet1) <- length(nodeSet1)
  for(i in seq_along(nodeSet1)) {
    keepNodeIDs <- setdiff(nodeIDs1[[i]], excludeNodeIDs[[declIDs1[i]]])
    if(length(keepNodeIDs)) {
      newNodeSet1[[i]] <- nodeSet1[[i]]$decl$declRule$originalIndexingRule$apply_reverse(  
        nodeSet1[[i]]$decl$declRule$getOriginalIndexing(keepNodeIDs), nodeSet1[[i]]$decl)
    } 
  }
  return(newNodeSet1[!sapply(newNodeSet1, is.null)])
}

#' @export
getConditionallyIndependentSets <- function(model, nodes, givenNodes, 
                                            explore = c("both", "down", "up"),
                                            unknownAsGiven = TRUE, returnScalarComponents = FALSE,
                                            endAsGiven = FALSE,
                                            nodesAsChars = getNimbleModelOption('nodesAsChars')) {
  nodesProvided <- !missing(nodes)
  givenProvided <- !missing(givenNodes)
  if (returnScalarComponents && !nodesAsChars) {
    warning("`returnScalarComponents=TRUE` requires the use of character representations of nodes")
    nodesAsChars = TRUE
  }

  explore <- match.arg(explore)
  startUp <- startDown <- TRUE
  if (explore == "down") 
    startUp <- FALSE
  if (explore == "up") 
    startDown <- FALSE

  if (!givenProvided) {
    givenNodes <- c(model$getNodes(topOnly = TRUE, stochOnly = TRUE, nodesAsChars = FALSE),
                    model$getNodes(dataOnly = TRUE, stochOnly = TRUE, nodesAsChars = FALSE))
    if (endAsGiven) 
      givenNodes <- c(givenNodes, model$getNodes(endOnly = TRUE, includeData = FALSE, stochOnly = TRUE, nodesAsChars = FALSE))
  } else {
    if (is.character(givenNodes)) {
      givenNodesChar <- givenNodes    
      givenNodes <- model$getNodes(givenNodes, stochOnly = TRUE, nodesAsChars = FALSE)
      if(length(givenNodes) != length(model$getNodes(givenNodesChar, nodesAsChars = FALSE)))
        stop("At the moment, `getConditionallyIndependentSets` does not allow deterministic nodes in `givenNodes`. This is work in progress.")
    }
    if(!all(sapply(givenNodes, \(x) model$isStoch(x))))
      stop("At the moment, `getConditionallyIndependentSets` does not allow deterministic nodes in `givenNodes`. This is work in progress.")
  }
  
  if (!nodesProvided) {
    nodes <- model$getNodes(latentOnly = TRUE, stochOnly = TRUE, 
                            includeData = FALSE, nodesAsChars = FALSE)
    pars <- model$getParents(givenNodes, upstream = TRUE, nodesAsChars = FALSE)
    if(is.null(pars)) {
      nodes <- NULL
    } else {
      allGivenParents <- model$getNodes(pars, nodesAsChars = FALSE)
      nodes <- intersect_nodes(nodes, allGivenParents)
    }
  } else {
    if (is.character(nodes)) 
      nodes <- model$getNodes(nodes, stochOnly = TRUE, nodesAsChars = FALSE)
  }

  if (nodesProvided && !givenProvided) {
    givenNodes <- setdiff_nodes(givenNodes, nodes)
  }
  if (givenProvided) {
    nodes <- setdiff_nodes(nodes, givenNodes)
  }

  allNodes <- model$getNodes(stochOnly = TRUE, nodesAsChars = FALSE)
  unknownNodes <- setdiff_nodes(allNodes, c(givenNodes, nodes))
  if(unknownAsGiven) {
    givenNodes <- c(givenNodes, unknownNodes)
  } # else nodes <- c(nodes, unknownNodes)
  
  stochDecl <- sapply(model$modelDef$declInfo, \(declInfo) declInfo$stoch)
  touched <- lapply(model$modelDef$declInfo[stochDecl], \(declInfo) taggedClass$new(declInfo$declRule))
  names(touched) <- sapply(model$modelDef$declInfo[stochDecl], \(declInfo) declInfo$declRule$ID)
  given <- lapply(model$modelDef$declInfo[stochDecl], \(declInfo) taggedClass$new(declInfo$declRule))
  names(given) <- names(touched)
  tmp <- sapply(givenNodes, \(node) given[[node$decl$declRule$ID]]$tag(node$getIDs()))
  
  sets <- list()
  length(sets) <- 10
  currentIdx <- 1
  numSets <- 1
  while(currentIdx <= length(nodes)) {
    focalNodeRange <- nodes[[currentIdx]]
    focalNodeIDs <- focalNodeRange$getIDs()
    this_touched <- touched[[focalNodeRange$decl$declRule$ID]]$tagged[focalNodeIDs]
    while(!all(this_touched)) {
      focalNodeID <- focalNodeIDs[!this_touched][1]
      currentNode <- focalNodeRange$decl$declRule$originalIndexingRule$apply_reverse(  # indexing range to nodeRange
        focalNodeRange$decl$declRule$getOriginalIndexing(focalNodeID), focalNodeRange$decl)  # ID to indexing range
      sets[[numSets]] <- getOneConditionallyIndependentSet(model, currentNode, focalNodeID, given, touched,
                                                           startUp = startUp, startDown = startDown)
      numSets <- numSets + 1
      if(numSets > length(sets))
        length(sets) <- 2*length(sets)
      this_touched <- touched[[focalNodeRange$decl$declRule$ID]]$tagged[focalNodeIDs]
    }
    currentIdx <- currentIdx + 1
  }
  sets <- sets[seq_len(numSets - 1)]
  
  if (nodesAsChars) {
    if (returnScalarComponents) {
      sets <- lapply(sets, \(set) nimbleModel:::flatten(lapply(set, \(x) x$toVarRange()$toVarChars(expandScalars = TRUE))))
    } else sets <- lapply(sets, \(set) flatten(lapply(set, \(x) x$toNodeChars())))
  }
  return(sets)

}

getOneConditionallyIndependentSet <- function(model, currentNode, currentID, given, touched, startUp, startDown) {
  ans <- list(currentNode)
  touched[[currentNode$decl$declRule$ID]]$tag(currentID)
  if(startUp)
    ans <- exploreUp(ans, model, currentNode, given, touched)
  if(startDown)
    ans <- exploreDown(ans, model, currentNode, given, touched)
  return(aggregate_nodes(ans))
}

exploreDown <- function(ans, model, currentNodes, given, touched) {
  deps <- model$getDependencies(currentNodes, self = FALSE)
  if(!is.null(deps)) {
    children <- model$getNodes(deps, stochOnly = TRUE, nodesAsChars = FALSE)
    for(child in children) {
      childIDs <- child$getIDs()
      this_touched <- touched[[child$decl$declRule$ID]][childIDs]
      this_given <- given[[child$decl$declRule$ID]][childIDs]
      touched[[child$decl$declRule$ID]]$tag(childIDs)
      chosen <- !this_touched & !this_given
      if(any(chosen)) {
        if(all(chosen)) {
          newLatentNodes <- child
        } else {
          newLatentIDs <- childIDs[chosen]
          newLatentNodes <- child$decl$declRule$originalIndexingRule$apply_reverse(child$decl$declRule$getOriginalIndexing(newLatentIDs), child$decl)
        }
        ans[[length(ans)+1]] <- newLatentNodes
      } else newLatentNodes <- NULL
      chosen <- !this_touched & this_given
      if(all(chosen)) {
        upNodesFromGiven <- child
      } else {
        upIDsFromGiven <- childIDs[chosen]
        if(length(upIDsFromGiven)) 
          upNodesFromGiven <- child$decl$declRule$originalIndexingRule$apply_reverse(child$decl$declRule$getOriginalIndexing(upIDsFromGiven), child$decl) else upNodesFromGiven <- NULL
      }
      upNodes <- c(newLatentNodes, upNodesFromGiven)
      if(length(upNodes))
        ans <- exploreUp(ans, model, upNodes, given, touched)
      if(length(newLatentNodes))
        ans <- exploreDown(ans, model, newLatentNodes, given, touched)
    }
  }
  return(ans)
}

exploreUp <- function(ans, model, currentNodes, given, touched) {
  pars <- model$getParents(currentNodes, self = FALSE, nodesAsChars = FALSE)
  if(!is.null(pars)) {
    parents <- model$getNodes(pars, stochOnly = TRUE, nodesAsChars = FALSE)
    for(parent in parents) {
      parentIDs <- parent$getIDs()
      this_touched <- touched[[parent$decl$declRule$ID]][parentIDs]
      this_given <- given[[parent$decl$declRule$ID]][parentIDs]
      touched[[parent$decl$declRule$ID]]$tag(parentIDs)
      chosen <- !this_touched & !this_given
      if(any(chosen)) {
        if(all(chosen)) {
          newLatentNodes <- parent
        } else {
          newLatentIDs <- parentIDs[chosen]
          newLatentNodes <- parent$decl$declRule$originalIndexingRule$apply_reverse(parent$decl$declRule$getOriginalIndexing(newLatentIDs), parent$decl)
        }
        ans[[length(ans)+1]] <- newLatentNodes
        ans <- exploreUp(ans, model, newLatentNodes, given, touched)
        ans <- exploreDown(ans, model, newLatentNodes, given, touched)
      }
    }
  }
  return(ans)
}

#' @export
setupMargNodes <- function(model, paramNodes, randomEffectsNodes, calcNodes,
                           calcNodesOther,
                           split = TRUE,
                           check = TRUE,
                           allowDiscreteLatent = FALSE) {

  paramProvided     <- !missing(paramNodes)
  reProvided        <- !missing(randomEffectsNodes)
  calcProvided      <- !missing(calcNodes)
  calcOtherProvided <- !missing(calcNodesOther)

  if(paramProvided) paramNodes         <- model$getNodes(paramNodes, nodesAsChars = FALSE)
  if(reProvided)    randomEffectsNodes <- model$getNodes(randomEffectsNodes, nodesAsChars = FALSE)
  # CHECK: do we need sorting? If so, probably do at end only when needed.
  if(calcProvided)  calcNodes          <- model$getNodes(calcNodes, nodesAsChars = FALSE) # , sort = TRUE)
  if(calcOtherProvided) calcNodesOther <- model$getNodes(calcNodesOther, nodesAsChars = FALSE) # , sort = TRUE)

  if(reProvided) {
    if(check && !allowDiscreteLatent)
      if(any(model$isDiscrete(randomEffectsNodes)))
        messageIfVerbose("  [Warning] Some elements of `randomEffectsNodes` follow discrete distributions. That is likely to cause problems.")
  }

    # We considered a feature to allow params to be nodes without priors. This is a placeholder in case
  # we ever pursue that again.
  # allowNonPriors <- FALSE
  # We may need to use determ and stochastic dependencies of parameters multiple times below
  # Define these to avoid repeated computation
  # A note for future: determ nodes between parameters and calcNodes are needed inside buildOneAGHQuad
  # and buildOneAGHQuad1D. In the future, these could be all done here to be more efficient
  paramDetermDeps <- character(0)
  paramStochDeps  <- character(0)
  paramDetermDepsCalculated <- FALSE
  paramStochDepsCalculated  <- FALSE

  # 1. Default parameters are stochastic top-level nodes. (We previously
  #    considered an argument allowNonPriors, defaulting to FALSE. If TRUE, the
  #    default params would be all top-level stochastic nodes with no RHSonly
  #    nodes as parents and RHSonly nodes (handling of constants TBD, since
  #    non-scalars would be converted to data) that have stochastic dependencies
  #    (And then top-level stochastic nodes with RHSonly nodes as parents are
  #    essentially latent/data nodes, some of which would need to be added to
  #    randomEffectsNodes below.) However this got too complicated. It is
  #    simpler and clearer to require "priors" for parameters, even though prior
  #    probs may not be used.
  paramsHandled <- TRUE
  if(!paramProvided) {
    if(!reProvided) {
      if(!calcProvided) {
        paramNodes <- model$getNodes(topOnly = TRUE, stochOnly = TRUE, includePredictive = FALSE, nodesAsChars = FALSE)
      } else {
        # calcNodes were provided, but RE nodes were not, so delay creating default params
        paramsHandled <- FALSE
      }
    } else {
      nodesToFindParentsFrom <- randomEffectsNodes
      paramNodes <- model$getNodes(model$getParents(randomEffectsNodes, self=FALSE, nodesAsChars = FALSE), stochOnly=TRUE, nodesAsChars = FALSE)
    }
    if(paramsHandled) {
      if(calcProvided) paramNodes <- setdiff_nodes(paramNodes, calcNodes)
      if(calcOtherProvided) paramNodes <- setdiff_nodes(paramNodes, calcNodesOther)
    }
  }

  # 2. Default random effects are latent nodes that are downstream stochastic dependencies of params.
  #    In step 3, default random effects are also limited to those that are upstream parents of calcNodes
  if((!reProvided) || check) {
    latentNodes <- model$getNodes(latentOnly = TRUE, stochOnly = TRUE,
                                  includeData = FALSE, includePredictive = FALSE, nodesAsChars = FALSE)
    if(!allowDiscreteLatent && length(latentNodes)) {
      latentDiscrete <- model$isDiscrete(latentNodes)
      if(any(latentDiscrete)) {
        if((!reProvided) && check) {
          messageIfVerbose("  [Note] In trying to determine default `randomEffectsNodes`, there are some nodes\n",
                  "         that follow discrete distributions. These will be omitted.")
        }
        latentNodes <- latentNodes[!latentDiscrete]
      }
    }
    if(paramsHandled) {
      paramDownstream <- model$getNodes(model$getDependencies(paramNodes, self = FALSE, downstream = TRUE, nodesAsChars = FALSE),
                                        stochOnly = TRUE, includePredictive = FALSE, nodesAsChars = FALSE)
      reNodesDefault <- intersect_nodes(latentNodes, paramDownstream)
    } else {
      reNodesDefault <- latentNodes
    }
    # Next, if calcNodes were not provided, we create a temporary
    # dataNodesDefault for purposes of updating reNodesDefault if needed. The
    # idea is that reNodesDefault should be trimmed to include only nodes
    # upstream of "data" nodes, where "data" means nodes in the role of data for
    # purposes of marginalization.
    # The tempDataNodesDefault is either dependencies of RE nodes if provided, or
    # actual data nodes in the model if RE nodes not provided.
    # If calcNodes were provided, then they are used directly to trim reNodesDefault.
    if(!calcProvided) {
      if(reProvided) {
        tempDataNodesDefault <- model$getNodes(model$getDependencies(randomEffectsNodes, self = FALSE, nodesAsChars = FALSE),
                                                stochOnly = TRUE, includePredictive = FALSE, nodesAsChars = FALSE)
      } else tempDataNodesDefault <- model$getNodes(dataOnly = TRUE, nodesAsChars = FALSE)
      if(paramsHandled)
        tempDataNodesDefault <- setdiff_nodes(tempDataNodesDefault, paramNodes)
      tempDataNodesDefaultParents <- model$getNodes(model$getParents(tempDataNodesDefault, upstream = TRUE, self = FALSE, nodesAsChars = FALSE),
                                                    stochOnly = TRUE, nodesAsChars = FALSE)
      reNodesDefault <- intersect_nodes(reNodesDefault, tempDataNodesDefaultParents)
    } else {
      # Update reNodesDefault to exclude nodes that lack downstream connection to a calcNode
      if(paramsHandled) { # This means reProvided OR paramsProvided. Including parents allows checking
        # of potentially missing REs.
        reNodesDefault <- intersect_nodes(reNodesDefault, 
                                     model$getNodes(model$getParents(calcNodes, upstream=TRUE, self=TRUE, nodesAsChars = FALSE), stochOnly = TRUE, nodesAsChars = FALSE))
      } else { # This means !paramsHandled and hence !reProvided AND !paramsProvided
        reNodesDefault <- intersect_nodes(reNodesDefault, calcNodes)
      }
    }
  }
  # If only calcNodes were provided, we have now created reNodesDefault from calcNodes,
  # and are now ready to create default paramNodes
  if(!paramsHandled) {
    paramNodes <- model$getNodes(model$getParents(reNodesDefault, self=FALSE, nodesAsChars = FALSE), stochOnly=TRUE, nodesAsChars = FALSE)
    if(calcOtherProvided) paramNodes <- setdiff_nodes(paramNodes, calcNodesOther)
  }

  # 3. Optionally check random effects if they were provided (not default)
  if(reProvided && check) {
    # First check is for random effects that should have been included but weren't
    reCheck <- setdiff_nodes(reNodesDefault, randomEffectsNodes)
    if(length(reCheck)) {
      errorNodes <- head(reCheck, n = 4)
      if(length(reCheck) > 4) moreText <- ", .." else moreText <- ""
      messageIfVerbose("  [Warning] There are some random effects (latent states) in the model that look like\n",
                       "            they should be included for the provided (or default) `paramNodes`,\n",
                       "            but are not included in `randomEffectsNodes`:\n",
                       "            ", paste0(sapply(errorNodes, \(node) node$toChar()), collapse = "; "), moreText, ".\n",
                       "            To silence this warning, one can usually include `check = FALSE`\n",
                       "            (potentially in the control list) for the algorithm or as\n",
                       "            an argument to `setupMargNodes`.")
    }
    # Second check is for random effects that were included but look unnecessary
    reCheck <- setdiff_nodes(randomEffectsNodes, reNodesDefault)
    if(length(reCheck)) {
      # Top nodes should never trigger warning.
      # Descendants of top nodes that are in randomEffectsNodes should not trigger warning
      topNodes <- model$getNodes(topOnly=TRUE, nodesAsChars = FALSE)
      reCheckTopNodes <- intersect_nodes(reCheck, topNodes)
      if(length(reCheckTopNodes)) {
        # Simple downstream=TRUE here is not a perfect check of connection among all nodes
        # but it will avoid false alarms
        reCheck <- setdiff_nodes(reCheck, model$getNodes(model$getDependencies(reCheckTopNodes, downstream=TRUE, nodesAsChars = FALSE), stochOnly=TRUE, nodesAsChars = FALSE))
      }
      if(length(reCheck)) {
        errorNodes <- head(reCheck, n = 4)
        if(length(reCheck) > 4) moreText <- ", .." else moreText <- ""
        extraMsg <- if(isTRUE(nimble::getNimbleOption('includeUnneededLatents'))) "" else "            They will be omitted, but one can force inclusion with\n            `nimbleOptions(includeUnneededLatents=TRUE)`.\n"
        messageIfVerbose("  [Warning] There are some `randomEffectsNodes` provided that look like\n",
                         "            they are not needed for the provided (or default) `paramNodes`:\n",
                         "            ", paste0(sapply(errorNodes, \(node) node$toChar()), collapse = "; "), moreText, ".\n", extraMsg,
                         "            To silence this warning, one can usually include `check = FALSE`\n",
                         "            (potentially in the control list) for the algorithm or as\n",
                         "            an argument to `setupMargNodes`.")
        if(!isTRUE(nimble::getNimbleOption('includeUnneededLatents')))
            randomEffectsNodes <- setdiff_nodes(randomEffectsNodes, reCheck)
      }
    }
  }
  # Set final choice of randomEffectsNodes
  if(!reProvided) {
    randomEffectsNodes <- reNodesDefault
  }

  # Set actual default calcNodes. This time it has self=TRUE (default)
  if((!calcProvided) || check) {
      calcNodesDefault <- model$getNodes(model$getDependencies(randomEffectsNodes, nodesAsChars = FALSE),
                                         includePredictive = FALSE, nodesAsChars = FALSE)
  }
  # 5. Optionally check calcNodes if they were provided (not default)
  if(calcProvided && check) {
    # First check is for calcNodes that look necessary but were omitted
    calcCheck <- setdiff_nodes(calcNodesDefault, calcNodes)
    if(length(calcCheck)) {
      errorNodes <- head(calcCheck, n = 4)
      if(length(calcCheck) > 4) moreText <- ", .." else moreText <- ""
      messageIfVerbose("  [Warning] There are some model nodes that look like they should be\n",
                       "            included in the `calcNodes` because\n",
                       "            they are dependencies of some `randomEffectsNodes`: \n",
                       "            ", paste0(sapply(errorNodes, \(node) node$toChar()), collapse = "; "), moreText, ".\n",
                       "            To silence this warning, one can usually include `check = FALSE`\n",
                       "            (potentially in the control list) for the algorithm or as\n",
                       "            an argument to `setupMargNodes`.")
    }
    # Second check is for calcNodes that look unnecessary
    # If some determ nodes between paramNodes and randomEffectsNodes are provided in calcNodes
    # then that's ok and we should not throw a warning message.
    calcCheck <- setdiff_nodes(calcNodes, calcNodesDefault)
    errorNodes <- calcCheck[model$isStoch(calcCheck)]
    # N.B. I commented out this checking of deterministic nodes for now.
    #      Iterating through individual nodes for getDependencies can be slow
    #      and I'd like to think more about how to do this. -Perry
    ## determCalcCheck <- setdiff(calcCheck, errorNodes)
    ## lengthDetermCalcCheck <- length(determCalcCheck)
    ## # Check other determ nodes
    ## if(lengthDetermCalcCheck){
    ##   paramDetermDeps <- model$getDependencies(paramNodes, determOnly = TRUE, includePredictive = FALSE)
    ##   paramDetermDepsCalculated <- TRUE
    ##   for(i in 1:lengthDetermCalcCheck){
    ##     if(!(determCalcCheck[i] %in% paramDetermDeps) ||
    ##        !(any(model$getDependencies(determCalcCheck[i], self = FALSE) %in% calcNodesDefault))){
    ##       errorNodes <- c(errorNodes, determCalcCheck[i])
    ##     }
    ##   }
    ## }
    if(length(errorNodes)){
      outErrorNodes <- head(errorNodes, n = 4)
      if(length(errorNodes) > 4) moreText <- ", .." else moreText <- ""
      messageIfVerbose("  [Warning] There are some `calcNodes` provided that look like\n",
                       "            they are not needed for the provided (or default) `randomEffectsNodes`:\n",
                       "            ", paste0(sapply(outErrorNodes, \(node) node$toChar()), collapse = "; "), moreText, ".\n",
                       "            To silence this warning, one can usually include `check = FALSE`\n",
                       "            (potentially in the control list) for the algorithm or as\n",
                       "            an argument to `setupMargNodes`.")
    }
  }
  # Finish step 4
  if(!calcProvided){
    calcNodes <- calcNodesDefault
  }
  if(!paramProvided) {
    possibleNewParamNodes <- model$getNodes(model$getParents(calcNodes, self=FALSE, nodesAsChars = FALSE), stochOnly=TRUE, includeData=FALSE, nodesAsChars = FALSE)
    # includeData=FALSE as data nodes cannot be parameters
    paramNodes <- aggregate_nodes(c(paramNodes, possibleNewParamNodes))
  }

  # 6. Default calcNodesOther: nodes needed for full model likelihood but
  #    that are not involved in the marginalization done by Laplace.
  #    Default is a bit complicated: All dependencies from paramNodes to
  #    stochastic nodes that are not part of calcNodes. Note that calcNodes
  #    does not necessarily contain deterministic nodes between paramNodes and
  #    randomEffectsNodes. We don't want to include those in calcNodesOther.
  #    (A deterministic that is needed for both calcNodes and calcNodesOther should be included.)
  #    So we have to first do a setdiff on stochastic nodes and then fill in the
  #    deterministics that are needed.
  if(!calcOtherProvided || check) {
    paramStochDeps <- model$getNodes(
      model$getDependencies(paramNodes, self = FALSE, nodesAsChars = FALSE),
      stochOnly = TRUE, # Should this be dataOnly=TRUE?
      includePredictive = FALSE, nodesAsChars = FALSE)
    calcNodesOtherDefault <- setdiff_nodes(paramStochDeps, calcNodes)
  }
  if(calcOtherProvided) {
    if((length(calcNodesOther) > 0) && !any(model$isStoch(calcNodesOther))) {
      messageIfVerbose("  [Warning] There are no stochastic nodes in the `calcNodesOther` provided for Laplace or AGHQ approximation.")
    }
  }
  if(!calcOtherProvided){
    calcNodesOther <- calcNodesOtherDefault
  }
  if(calcOtherProvided && check) {
    calcOtherCheck <- setdiff_nodes(calcNodesOtherDefault, calcNodesOther)
    if(length(calcOtherCheck)) {
      # We only check missing stochastic nodes; determ nodes will be added below
      missingStochNodesInds <- which(model$isStoch(calcOtherCheck))
      lengthMissingStochNodes <- length(missingStochNodesInds)
      if(lengthMissingStochNodes){
        missingStochNodes <- calcOtherCheck[missingStochNodesInds]
        errorNodes <- head(missingStochNodes, n = 4)
        if(length(missingStochNodes) > 4) moreText <- ", .." else moreText <- ""
        messageIfVerbose("  [Warning] There are some model nodes (stochastic) that look like they should be\n",
                         "            included in the `calcNodesOther` for parts of the likelihood calculation\n",
                         "            outside of Laplace or AGHQ approximation: \n",
                         "            ", paste0(sapply(outErrorNodes, \(node) node$toChar()), collapse = "; "), moreText, ".\n",
                         "            To silence this warning, include `check = FALSE` in the control list\n",
                         "            to `buildLaplace` or as an argument to `setupMargNodes`.")
      }
    }
    # Check redundant stochastic nodes
    calcOtherCheck <- setdiff_nodes(calcNodesOther, calcNodesOtherDefault)
    stochCalcOtherCheck <- calcOtherCheck[model$isStoch(calcOtherCheck)]
    errorNodes <- stochCalcOtherCheck
    # Check redundant determ nodes
    # N.B. I commented-out this deterministic node checking for reasons similar to above. -Perry
    ## determCalcOtherCheck <- setdiff(calcOtherCheck, stochCalcOtherCheck)
    ## lengthDetermCalcOtherCheck <- length(determCalcOtherCheck)
    ## errorNodes <- character(0)
    ## if(lengthDetermCalcOtherCheck){
    ##   if(!paramDetermDepsCalculated) {
    ##     paramDetermDeps <- model$getDependencies(paramNodes, determOnly = TRUE, includePredictive = FALSE)
    ##     paramDetermDepsCalculated <- TRUE
    ##   }
    ##   for(i in 1:lengthDetermCalcOtherCheck){
    ##     if(!(determCalcOtherCheck[i] %in% paramDetermDeps) ||
    ##        !(any(model$getDependencies(determCalcOtherCheck[i], self = FALSE) %in% calcNodesOtherDefault))){
    ##       errorNodes <- c(errorNodes, determCalcOtherCheck[i])
    ##     }
    ##   }
    ## }
    ## errorNodes <- c(stochCalcOtherCheck, errorNodes)
    if(length(errorNodes)){
      outErrorNodes <- head(errorNodes, n = 4)
      if(length(errorNodes) > 4) moreText <- ", .." else moreText <- ""
      messageIfVerbose("  [Warning] There are some nodes provided in `calcNodesOther` that look like\n",
                       "            they are not needed for parts of the likelihood calculation\n",
                       "            outside of Laplace or AGHQ approximation: \n",
                       "            ", paste0(sapply(outErrorNodes, \(node) node$toChar()), collapse = "; "), moreText, ".\n",
                       "            To silence this warning, include `check = FALSE` in the control list\n",
                       "            to `buildLaplace` or as an argument to `setupMargNodes`.")
    }
  }
  # Check and add necessary (upstream) deterministic nodes into calcNodesOther
  # This ensures that deterministic nodes between paramNodes and calcNodesOther are used.
  if(length(calcNodesOther)) {
    if(!paramDetermDepsCalculated) {
      # We need to process each individual node.
      paramDetermDeps <- model$getNodes(model$getDependencies(paramNodes, nodesAsChars = FALSE), determOnly = TRUE,
                                        includePredictive = FALSE, nodesAsChars = TRUE)
      paramDetermDepsCalculated <- TRUE
    }
    numParamDetermDeps <- length(paramDetermDeps)
    if(numParamDetermDeps) {
      keep_paramDetermDeps <- logical(numParamDetermDeps)
      for(i in seq_along(paramDetermDeps)) {
        nextDeps <- model$getNodes(model$getDependencies(paramDetermDeps[i], nodesAsChars = FALSE), nodesAsChars = FALSE)
        keep_paramDetermDeps[i] <- length(intersect_nodes(nextDeps, calcNodesOther)) > 0
      }
      paramDetermDeps <- paramDetermDeps[keep_paramDetermDeps]
    }
    calcNodesOther <- aggregate_nodes(c(calcNodesOther, model$getNodes(paramDetermDeps, nodesAsChars = FALSE)))  # c(paramDetermDeps, calcNodesOther)
  }

  # 7. Do the splitting into sets (if given) or conditionally independent sets (if TRUE)
  givenNodes <- NULL
  reSets <- list()
  if(length(randomEffectsNodes)) {
    if(isFALSE(split)) {
      reSets <- list(randomEffectsNodes)
    } else {
      if(isTRUE(split)) {
        # givenNodes should only be stochastic
        givenNodes <- setdiff_nodes(c(paramNodes, calcNodes),
                              c(randomEffectsNodes,
                                model$getNodes(model$getDependencies(randomEffectsNodes, nodesAsChars = FALSE), determOnly=TRUE, nodesAsChars = FALSE)))
        reSets <- getConditionallyIndependentSets(model,
          nodes = randomEffectsNodes, givenNodes = givenNodes,
          unknownAsGiven = TRUE)
      }
      else if(is.numeric(split)){   # TODO: check this makes sense with inputs being nodeRanges.
        reSets <- split(randomEffectsNodes, split)
      }
      else stop("setupMargNodes: Invalid value for `split`")
    }
  }
  list(paramNodes = paramNodes,
       randomEffectsNodes = randomEffectsNodes,
       calcNodes = calcNodes,
       calcNodesOther = calcNodesOther,
       givenNodes = givenNodes,
       randomEffectsSets = reSets
       )
}


    
splitLatents <- function(model, paramNodes, latentNodes, calcNodes, calcNodesOther, control = list()) {
    stochNodes <- model$getNodes(stochOnly = TRUE, includeData = FALSE, nodesAsChars = FALSE)
    discreteStochNodes <- model$isDiscrete(stochNodes)
    if (any(discreteStochNodes))
        stop("splitLatents: found discrete non-data stochastic nodes in processing nodes for quadrature-based posterior approximation: ",
             paste0(sapply(stochNodes[discreteStochNodes], \(node) node$toChar()), collapse = "; "), ". Discrete non-data stochastic nodes cannot be handled by the posterior approximation algorithm.")
    split <- extractControlElement(control, "split", TRUE)
    check <- extractControlElement(control, "check", TRUE)
    margNodes <- setupMargNodes(model = model, paramNodes = paramNodes, randomEffectsNodes = latentNodes,
        calcNodes = calcNodes, calcNodesOther = calcNodesOther, split = split, check = check)
    if (missing(paramNodes) && missing(latentNodes)) {
        if (!missing(calcNodes) || !missing(calcNodesOther))
            messageIfVerbose("   [Note] Ignoring provide `calcNodes` and `calcNodesOther` because `paramNodes` and `latentNodes` not provided and are being determined automatically.")
        paramNodes <- margNodes$paramNodes
        latentNodes <- margNodes$randomEffectsNodes
        deps <- model$getNodes(model$getDependencies(latentNodes, self = FALSE, nodesAsChars = FALSE), includeData = FALSE, nodesAsChars = FALSE)
        ## By default, we treat "siblings" of latent nodes as latents.
        ## This attempts to have fixed effects in latents,
        ## along with random effects.
        newLatents <- model$getNodes(model$getParents(deps, nodesAsChars = FALSE), stochOnly = TRUE, includeData = FALSE, nodesAsChars = FALSE)
        paramNodes <- setdiff_nodes(paramNodes, newLatents)
        latentNodes <- aggregate_nodes(c(latentNodes, newLatents))
        margNodes <- setupMargNodes(model = model, paramNodes = paramNodes,
            randomEffectsNodes = latentNodes, split = split, check = check)
    }

    return(margNodes)
}

