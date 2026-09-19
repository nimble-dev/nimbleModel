# An loopIndexingRuleClass object represents the relationship
# between the loop indexing and the indexing of a LHS variable,
# such as giving the values of `i` when provided a varRange for `y` in
# `for(i in 5:n) y[i-2] <- 1`, such that `y[7:9]` would give `i=9:11`.


loopIndexingRuleClass <- R6Class(
  classname = "loopIndexingRuleClass",
  portable = FALSE,
  public = list(
    graphRule = NULL,
    indexSlotToSet = NULL,
    externalRule = NULL,
    internalRule = NULL,
    decl = NULL,
    varName = character(),
    initialize = function(LHS,
                          context,
                          constants = list(),
                          decl = NULL) {
      varName <<- getVarName(LHS)
      decl <<- decl  
      if (length(context$indexVarNames)) {
        # Exclude indices not used in lifted expression, e.g., `i` in `y[i,j] ~ dnorm(mu[i], var = sigma2[j])`
        indexVarNames <- context$indexVarNames
        indexVarNames <- indexVarNames[indexVarNames %in% all.vars(LHS)]
        indexing <- if (length(indexVarNames)) {
          paste0("[", paste(indexVarNames, collapse = ","), "]")
        } else {
          ""
        }
        dummyLHS <- parse(text = paste0(".loop", indexing))[[1]]
        # Unused singleContexts will be removed in graphRuleClass$new().
      } else {
        dummyLHS <- as.name(".loop")
      }

      graphRule <<- graphRuleClass$new(
        dummyLHS,
        LHS,
        context,
        constants
      )

      # For use in `invert`; we want to produce nodeRanges, not varRanges.
      fullRule <- graphRuleClass$new(
        LHS,
        dummyLHS,
        context,
        constants
      )

      indexSlotToSet <<- fullRule$indexSets$toIndexSlotToSet
      if (length(fullRule$indexRules)) { # if any indexing
        isConstant <- sapply(fullRule$indexRules, function(x) inherits(x, "indexRuleConstantClass"))

        # Treat constants as internal rules that don't relate to indexing over nodes.
        # Need to relate constant rule types to indexing; constant rules are in order of constant indices.
        constantIndices <- fullRule$indexSets$toIndexSlotToSet == 0

        externalRule <<- fullRule$clone()
        externalRule$indexRules[isConstant] <<- NULL
        externalRule$indexSets$toIndexSlotToSet <<-
          externalRule$indexSets$toIndexSlotToSet[!constantIndices]

        internalRule <<- fullRule$clone()
        internalRule$indexRules[!isConstant] <<- NULL
        if (length(internalRule$indexRules)) {
          # Transform indexSets as if LHS is just from the constant rules and RHS is
          # all constants. E.g., `y[1] <- y[1,1]` if one of two slots is constant.
          toExpr <- parse(text = paste0("y[", paste(rep(1, sum(constantIndices)), collapse = ","), "]"))[[1]]
          fromExpr <- parse(text = paste0("y[", paste(rep(1, length(constantIndices)), collapse = ","), "]"))[[1]]
          internalRule$indexSets <<- makeSeparableIndexSets(toExpr, fromExpr, modelContextClass$new())
        }
      }
    },

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
    # TODO: or we could name this loopIndexingToNodes or some such.
    invert = function(indexingRange) {
      if (length(externalRule$indexRules)) {
        externalRange <- externalRule$apply(indexingRange)
        if (is.null(externalRange)) {
          return(NULL)
        }
      } else {
        externalRange <- varRangeClass$new(list())
      }
      if (length(internalRule$indexRules)) {
        # This needs to be instantiated anew to avoid having multiple references to the internalRange indexRanges.
        internalRange <- internalRule$apply(externalRule$getFromRange()) 
      } else {
        internalRange <- varRangeClass$new(list())
      }
      # Note that the varName is determined from self$varName (indexingRange has .loop as varName).
      return(nodeRangeClass$new(varName, externalRange, internalRange, indexSlotToSet, decl))
    }
  )
)

loopIndexingRangeClass <- R6Class(
  "loopIndexingRangeClass",
  portable = FALSE,
  inherit = varRangeClass,
  public = list(
        initialize = function(indexInfo,
                          rangeToIndexSlot = NULL,
                          varName = ".loop",
                          fromStochRule = NULL) {
          super$initialize(indexInfo, rangeToIndexSlot, varName, fromStochRule)
        },
        toChar = function() {
          dots <- sapply(indexRangeExprs, identical, quote(...))
          if(all(dots))
            return("nonseparable loop indexing")
          text <- paste0("index ", seq_along(indexRangeExprs[!dots]), ": ")
          text <- paste0(text, indexRangeExprs[!dots], collapse = ", ")
          if(any(dots)) text <- paste0(text, ", plus nonseparable loop indexing")
          return(text)
        },
        print = function() {
          cat("looping with ", toChar(), ".\n", sep = "")
        }
  )
)

        
