# The dataRuleClass represents the indexing of data nodes (or non-data nodes).

# Perhaps should be called `isDataRuleClass`.

# TODO: this sets `rule` field as NULL if the rule
# is "empty" (representing no indices).
# May need to revisit this behavior.

# `sequenceThreshold` determines cutoff below which we
# create block rules in 1-d case rather than arbitrary rules.
# For more than 1-d, default to arbitrary for simplicity
# rather than dealing with an algorithm to try to create
# minimal number of blocks.

newDataRules <- function(x, varName, nondata = FALSE, sequenceThreshold = 0.1) {
  NAs <- is.na(x)
  allNAs <- all(NAs)
  anyNAs <- any(NAs)

  if (allNAs && !nondata) {
    return(NULL)
  }
  if (!anyNAs && nondata) {
    return(NULL)
  }

  if (length(x) == 1) {
    expr <- parse(text = varName)[[1]]
    rules <- list(graphRuleClass$new(expr, expr, context = modelContextClass$new()))
  } else {
    if (!anyNAs || allNAs) { # No fracturing
      rulePieces <- makeRulePieces(NAs, varName, all = TRUE)
    } else {
      if (nondata) {
        rulePieces <- makeRulePieces(NAs, varName,
          all = FALSE,
          sequenceThreshold = sequenceThreshold
        )
      } else {
        rulePieces <- makeRulePieces(!NAs, varName,
          all = FALSE,
          sequenceThreshold = sequenceThreshold
        )
      }
    }
    # We'll use graphRules, though only have a single "side".
    rules <- lapply(rulePieces, function(singleRulePieces) {
      graphRuleClass$new(singleRulePieces$expr, singleRulePieces$expr,
        context = modelContextClass$new(singleRulePieces$singleContexts),
        constants = singleRulePieces$constants
      )
    })
  }
  return(lapply(rules, function(rule) dataRuleClass$new(rule, varName, nondata)))
}

dataRuleClass <- R6Class(
  classname = "dataRuleClass",
  portable = FALSE,
  public = list(
    rule = NULL,
    varName = NULL,
    nondata = NULL,
    initialize = function(rule, varName, nondata) {
      varName <<- varName
      nondata <<- nondata # inverse case, used for `includeData = FALSE`
      rule <<- rule
    },
    apply = function(varRange = NULL) {
      if (is.null(varRange)) {
        varRange <- varName
      }
      name <- getVarName(varRange)
      if (name != varName) { # variable names don't match
        return(NULL)
      }
      if (is.character(varRange) && name != varRange) {
        varRange <- varRangeClass$new(varRange)
      } # e.g., 'y[1:3]'
      rule$apply(varRange)
    }
  )
)


makeRulePieces <- function(elements, varName, all, sequenceThreshold = 0.1) {
  d <- dimOrLength(elements)
  expr <- quote(y[idx])
  expr[[2]] <- as.name(varName)

  if (all) { # Full 'rectangular' extent.
    idxNames <- paste0("idx", seq_along(d))
    singleContexts <- lapply(seq_along(d), function(i) {
      singleContextClass$new(
        indexVarExpr = as.name(idxNames[i]),
        indexRangeExpr = substitute(1:L, list(L = d[i]))
      )
    })
    expr[3:(2 + length(d))] <- lapply(idxNames, as.name)

    constants <- list()
  } else {
    # TODO: could extend this to handle matrices where entire rows/columns
    # are homogeneous, creating block rules.
    if (length(d) == 1 && mean(!elements) < sequenceThreshold) {
      splits <- which(!elements)
      starts <- c(1, splits + 1)
      ends <- c(splits - 1, length(elements))
      invalid <- starts > ends
      starts <- starts[!invalid]
      ends <- ends[!invalid]
      singleContext <- singleContextClass$new(
        indexVarExpr = as.name("idx"),
        indexRangeExpr = quote(L:U)
      )
      return(lapply(
        seq_along(starts),
        function(i) {
          list(
            expr = expr, singleContexts = list(singleContext),
            constants = list(L = starts[i], U = ends[i])
          )
        }
      ))
    } else {
      singleContexts <- list(
        singleContextClass$new(
          indexVarExpr = as.name("idx"),
          indexRangeExpr = substitute(1:L, list(L = sum(elements)))
        )
      )

      newcode <- paste0("k", seq_along(d), "[idx]")
      expr[3:(2 + length(d))] <- parse(text = newcode)

      inds <- which(elements, arr.ind = TRUE)
      if (!is.array(inds)) {
        if (length(d) == 1) {
          inds <- matrix(inds, ncol = 1)
        } else {
          inds <- matrix(inds, nrow = 1)
        }
      } # Shouldn't ever be needed.
      constants <- lapply(seq_len(ncol(inds)), function(i) {
        inds[, i]
      })
      names(constants) <- paste0("k", seq_along(d))
    }
  }
  return(list(list(expr = expr, singleContexts = singleContexts, constants = constants)))
}


excludeFromPredictiveRules <- function(modelDef, currentRanges, candidateRules) {
  if (!length(candidateRules)) {
    return(NULL)
  }
  for (range in currentRanges) {
    varName <- range$varName
    tmp <- unlist(lapply(candidateRules[[varName]]$rules, exclude, range))
    tmp <- tmp[!sapply(tmp, is.null)]
    if (length(tmp)) {
      candidateRules[[varName]] <- varRulesClass$new(tmp, varName)
    } else {
      candidateRules[[varName]] <- NULL
    }
    parents <- getParents(modelDef, range, nodesAsChars = FALSE)
    candidateRules <- excludeFromPredictiveRules(modelDef, parents, candidateRules)
  }
  return(candidateRules)
}
