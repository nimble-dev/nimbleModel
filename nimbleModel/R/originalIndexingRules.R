# An originalIndexingRuleClass object represents the relationship
# between the loop indexing and the indexing of a LHS variable,
# such as giving the values of `i` when provided a varRange for `y` in
# `for(i in 5:n) y[i-2] <- 1`, such that `y[7:9]` would give `i=9:11`.


originalIndexingRuleClass <- R6Class(
  classname = "originalIndexingRuleClass",
  portable = FALSE,
  public = list(
    graphRule = NULL,
    graphRuleRev = NULL,
    varName = character(),
    initialize = function(LHS,
                          context,
                          constants = list()) {
      varName <<- getVarName(LHS)
      if (length(context$indexVarNames)) {
        # Exclude indices not used in lifted expression, e.g., `i` in `y[i,j] ~ dnorm(mu[i], var = sigma2[j])`
        indexVarNames <- context$indexVarNames
        indexVarNames <- indexVarNames[indexVarNames %in% all.vars(LHS)]
        indexing <- if (length(indexVarNames)) {
          paste0("[", paste(indexVarNames, collapse = ","), "]")
        } else {
          ""
        }
        dummyLHS <- parse(text = paste0(varName, indexing))[[1]]
        # Unused singleContexts will be removed in graphRuleClass$new().
      } else {
        dummyLHS <- as.name(varName)
      }
      graphRule <<- graphRuleClass$new(
        dummyLHS,
        LHS,
        context,
        constants
      )
      graphRuleRev <<- graphRuleClass$new(
        LHS,
        dummyLHS,
        context,
        constants
      )
    },

    # Produces a varRange, though it's not really a range for a variable
    # but rather a range for the indices.
    # (2023-06-10, commit 30ede6)
    # Do not remove duplicates because in generation of `calcRange`s there
    # can be cases where we need duplicated values in order to have correct
    # number of logProbs.
    # (2026-07-09) actually we need to remove duplicates for y[i,n1[i]:n2[i]] case
    # as that has one 'node' per scalar element.
    # It appears that the 2023-06-10 thinking was incorrect as it seems to have
    # been based on wanting `calculate()` to return as many elements as were
    # computed in any given deterministic calculation, e.g., y[1:2] <- foo()
    # returning 2 elements, but we actually don't return deterministic results anyway.
    # See test in line 1302 of test-nodeRules.R in commit 30ede6.
    apply = function(fromVarRange) {
      graphRule$apply(fromVarRange, removeDuplicates = TRUE)
    },
    
    apply_reverse = function(indexingRange) {
      graphRuleRev$apply(indexingRange)
    }
  )
)

# Probably move to declRuleClass
nodeIDRuleClass <- R6Class(
  classname = "nodeIDRuleClass",
  portable = FALSE,
  public = list(
    declRule = NULL,
    numLoops = NULL,
    indexingRules = NULL,
    initialize = function(declRule) {
      declRule <<- declRule
      indexingRules <<- declRule$originalIndexingRule$graphRule$indexRules
      ## TODO: what else should I cache? numLoops, indexing lengths?
      if(length(indexingRules) == length(declRule$originalIndexingRule$graphRule$indexSets$toIndexSlotToSet) &&
           !identical(declRule$originalIndexingRule$graphRule$indexSets$toIndexSlotToSet, seq_along(indexingRules)))
        stop("originalIndexingRules are not in canonical order")
      numLoops <<- length(declRule$originalIndexingRule$graphRule$indexSets$toIndexSlotToSet)
    },
    apply = function(indexingRange) {
      if(!inherits(indexingRange, 'varRangeClass'))
        stop("`indexingRange` must be a varRange")
      # Loop indexing is separable so can determine IDs by arithmetic.
      if(length(indexingRules) == numLoops) {
        # TODO: will indexingRules always be in canonical order?
        if(length(indexingRange$indexRanges) == length(indexingRules)) {   # indexingRange is separable as well.
          if(numLoops > 1) {
            indices <- list(length = numLoops)
            # Put in order of loop index slots.
            indexingRangeRanges <- indexingRange$indexRanges[indexingRange$indexSlotToRange]
            individualLoopIDs <- lapply(seq_len(numLoops), \(i) getOneLoopIDs(indexingRangeRanges[[i]], indexingRules[[i]]))
            indices[[numLoops]] <- individualLoopIDs[[numLoops]]
            offset <- 1
            lens <- sapply(indexingRules, \(x) x$getNumElements())
            for(loop_idx in (numLoops-1):1) {
              offset <- offset * lens[loop_idx+1]
              indices[[loop_idx]] <- (individualLoopIDs[[loop_idx]]-1)*offset
            }
            nodeIDs <- rowSums(do.call(expand.grid, rev(indices))) # Use `rev` to avoid need to sort.
          } else {
            nodeIDs <- getOneLoopIDs(indexingRange$indexRanges[[1]], indexingRules[[1]])
          }
        } else { # indexing range is non-separable: expand and then do arithmetic.
          inputIndices <- indexingRange$extractIndexRange(seq_len(numLoops))$getValuesAsMatrix()
          indicesLast <- getOneLoopIDs(newIndexRange(inputIndices[ , numLoops], sort = FALSE), indexingRules[[numLoops]])
          indices <- matrix(0, nrow = length(indicesLast), ncol = numLoops)
          indices[ , numLoops] <- indicesLast
          offset <- 1
          lens <- sapply(indexingRules, \(x) x$getNumElements())
          for(loop_idx in (numLoops-1):1) {
            offset <- offset * lens[loop_idx+1]
            indices[ , loop_idx] <- (getOneLoopIDs(newIndexRange(inputIndices[ , loop_idx], sort = FALSE), indexingRules[[loop_idx]])-1)*offset
          }
          nodeIDs <- rowSums(indices)
        }
      } else {  # Loop indexing is nonseparable. Fall back to expand and match, even if there are multiple (i.e., crossed) rules.
        fullIndices <- declRule$originalIndexingRule$apply(declRule$varName)$extractIndexRange(seq_len(numLoops))$getValuesAsMatrix()
        inputIndices <- indexingRange$extractIndexRange(seq_len(numLoops))$getValuesAsMatrix()
        nodeIDs <- match(
          do.call(interaction, as.data.frame(inputIndices)),
          do.call(interaction, as.data.frame(fullIndices))
        )
      }        
      return(nodeIDs)
    },
    apply_reverse = function(nodeIDs) {
      if(length(indexingRules) == numLoops) {  # Loop indexing is separable.
        # Can we shortcircuit if nodeIDs is all possible ones?
        # Or if only one nodeID
        if(numLoops == 1) {
          indices <- getOneLoopIndices(nodeIDs, indexingRules[[1]])
          indexRange <- newIndexRange(indices)
          if (inherits(indexRange, "indexRangeMatrixClass")) {
            indexRange <- indexRange$toSequence()  # TODO: does this handle sorting? Should we see if we can do it more quickly?
          }
          return(varRangeClass$new(list(indexRange), varName = declRule$originalIndexingRule$varName))
        } else {
          lens <- sapply(indexingRules, \(x) x$getNumElements())
          cumLens <- rev(cumprod(rev(lens)))
          indices <- matrix(0, nrow = length(nodeIDs), ncol = numLoops)
          for(loop_idx in 1:(numLoops-1)) {
            indices[ , loop_idx] <- ((nodeIDs - 1) %/% cumLens[loop_idx+1]) + 1
            nodeIDs <- nodeIDs - (indices[,loop_idx]-1) * cumLens[loop_idx+1]
          }
          indices[ , 3] <- nodeIDs

          # Check for crossed ranges before converting back to actual indexing (to deal with non-sequential cases).
          uniqValues <- lapply(seq_len(numLoops), \(i) {
            uniq <- sort(unique(indices[,i]))
            if(all(diff(uniq) == 1)) {
              rg <- range(uniq)
              uniq <- rg[1]:rg[2]
            }
            return(uniq)
          })
          lens <- sapply(uniqValues, length)
          if(nrow(indices) == prod(lens)) {   # Check for and simplify crossed ranges.
            gr <- do.call(expand.grid, lapply(rev(uniqValues), as.numeric)) # TODO: ideally avoid need for `as.numeric`.
            colnames(gr) <- NULL
            if(identical(c(indices[ , numLoops:1]), as.numeric(unlist(gr)))) {  
              uniqIndices <- lapply(seq_along(uniqValues), \(i) getOneLoopIndices(uniqValues[[i]], indexingRules[[i]]))
              indexRanges <- lapply(seq_len(numLoops), \(i) {
                if(length(uniqIndices[[i]]) > 1 && all(diff(uniqIndices[[i]]) == 1)) {
                  rg <- range(uniqIndices[[i]])
                  return(newIndexRange(substitute(MIN:MAX, list(MIN=rg[1], MAX=rg[2]))))
                } else return(newIndexRange(uniqIndices[[i]]))
              })
              return(varRangeClass$new(indexRanges, varName = declRule$varName))
            }
          } else {
            for(i in seq_len(numLoops))
              indices[,i] <- getOneLoopIndices(indices[,i], indexingRules[[i]])
          }
        }
      } else { # Loop indexing is nonseparable. Fall back to expand and select, even if there are multiple (i.e., crossed) rules.
        fullIndices <- declRule$originalIndexingRule$apply(declRule$varName)$extractIndexRange(seq_len(numLoops))$getValuesAsMatrix()
        indices <- fullIndices[nodeIDs, ]
      }
      return(varRangeClass$new(list(newIndexRange(indices)), varName = declRule$originalIndexingRule$varName))
    },
    # Convert from actual loop indexing to 1-based indexing (accounting for offset or non-sequential indexing).
    # TODO: we may want to create more methods for the indexingRules so as not to access their internals here.
    getOneLoopIDs = function(indexRange, indexingRule) {
      if(inherits(indexingRule, 'indexRuleBlockClass')) {
        init <- indexingRule$setupResults$fromMin
        # We could use `switch` but then can't use `inherits` and would need to pick off [[1]] element from class().
        if(inherits(indexRange, 'indexRangeMatrixClass')) {
          return(c(indexRange$values)-init+1)
        }
        if(inherits(indexRange, 'indexRangeSequenceClass')) {
          return((indexRange$start-init+1):(indexRange$end-init+1))
        }
        if(inherits(indexRange, 'indexRangeScalarClass')) {
          return(indexRange$value-init+1)
        }
        stop("invalid type of indexRange provided for creating nodeIDs")
      }
      if(inherits(indexingRule, 'indexRuleArbitraryClass')) {
        return(match(indexRange$getValuesAsMatrix(), unlist(indexingRule$setupResults$iRow2toIndices)))
      }
      stop("invalid type of indexRule provided for creating nodeIDs")
    },
    # Convert from 1-based indexing to the actual loop indexing (accounting for offset or non-sequential indexing).
    getOneLoopIndices = function(relativeNodeIDs, indexingRule) {
      if(inherits(indexingRule, 'indexRuleBlockClass')) {
        if(indexingRule$setupResults$fromMin != 1)
          return(relativeNodeIDs + (indexingRule$setupResults$fromMin - 1)) else return(relativeNodeIDs)
      }
      if(inherits(indexingRule, 'indexRuleArbitraryClass')) {
        return(unlist(indexingRule$setupResults$iRow2toIndices[relativeNodeIDs]))  
      }
      stop("invalid type of indexRule provided for creating indices from nodeIDs")
    }
  )
)
