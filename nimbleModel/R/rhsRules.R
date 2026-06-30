# A rhsRuleClass object represents the indexing information for a variable
# in a right-hand side expression.
# This may come from the original declaration, or from excluding from that declaration
# elements that appear on the left-hand side of another expression.

# Constant vectors are treated as sequences because we need indexing for them
# when determining RHSonly.
# E.g., if we have `z[i,1:2] <- y[i,1:2]; w <- y[2,2]` treating `1:2` as constant
# causes `exclude` to lose track of `y[2,1]`

rhsRuleClass <- R6Class(
  classname = "rhsRuleClass",
  portable = FALSE,
  inherit = nodeRuleClass,
  public = list(
    usedInIndex = FALSE, # Allows tracking of use as dynamic index.

    initialize = function(expr, ID = NULL, context = modelContextClass$new(), constants = list(), usedInIndex = FALSE) {
      usedInIndex <<- usedInIndex
      # Process any dynamic indexing.
      if (getNimbleModelOption("allowDynamicIndexing") && isUsedInIndex(expr)) {
        usedInIndex <<- TRUE
        expr <- stripIndexWrapping(expr)
      }

      # Indices for dynamically-indexed RHS vars cannot be determined
      # so set to some very large value (can't use `1:Inf`).
      if (getNimbleModelOption("allowDynamicIndexing")) {
        if (length(expr) >= 3) {
          expr[3:length(expr)] <- lapply(
            expr[3:length(expr)],
            function(e) {
              if (isDynamicIndex(e)) {
                e <- quote(1:2)
                e[[3]] <- .Machine$integer.max
              }
              return(e)
            }
          )
        }
      }

      # Replace sequence indexing with maximal indexing in cases
      # in which could have duplicate declarations of RHS because
      # one or more singleContexts are not used in indexing the RHS
      # but are used in for loop expression, e.g.,
      # `for(i in 1:4) for(t in 1:seasons[i]) y[i,t] <- alpha[t]`.
      usedIndexVarsBool <- names(context$singleContexts) %in% all.vars(expr)
      if (any(usedIndexVarsBool) && !all(usedIndexVarsBool)) {
        usedIndexVars <- names(context$singleContexts)[usedIndexVarsBool]
        depIndexVarExprs <- sapply(
          usedIndexVars,
          function(var) {
            !all(all.vars(context$singleContexts[[var]]$indexRangeExpr) %in%
              names(constants))
          }
        )
        usedIndexVars <- usedIndexVars[depIndexVarExprs]
        if (length(usedIndexVars)) { # Only if index expr uses other indices.
          allReplacements <- lapply(usedIndexVars, as.name)
          names(allReplacements) <- paste0("t", seq_along(allReplacements))
          unrolledIndicesEnv <-
            expandContextAndReplacements(
              allReplacements = allReplacements,
              allReplacementNameExpr = lapply(names(allReplacements), as.name),
              context = context,
              constants = constants
            )
          rgs <- lapply(usedIndexVars, function(x) { # Use full extent of indexing across all iterations.
            range(unrolledIndicesEnv[[x]])
          })
          newSingleContexts <- context$singleContexts
          # Assign full extent back into singleContext indexRangeExpr, removing
          # dependence on other indices.
          newSingleContexts[usedIndexVars] <- lapply(
            seq_along(usedIndexVars),
            function(i) {
              singleContextClass$new(
                indexVarExpr = as.name(usedIndexVars[i]),
                indexRangeExpr = substitute(L:M, list(L = rgs[[i]][1], M = rgs[[i]][2]))
              )
            }
          )
          context <- modelContextClass$new(newSingleContexts)
        }
      }

      # Transform constants into sequences.
      if (length(expr) > 1 && expr[[1]] == "[") {
        scalarConstants <- sapply(
          3:length(expr),
          function(i) length(expr[[i]]) == 1 && !length(all.vars(expr[[i]]))
        )
        blockConstants <- sapply(
          3:length(expr),
          function(i) length(expr[[i]]) > 1 && expr[[i]][[1]] == ":" && !length(all.vars(expr[[i]]))
        )

        scalarOrBlockConstants <- scalarConstants | blockConstants
        if (any(scalarOrBlockConstants)) {
          newSingleContexts <- context$singleContexts
          cnt <- length(newSingleContexts)
          for (idx in which(scalarOrBlockConstants)) {
            cnt <- cnt + 1
            newSingleContexts[[cnt]] <- singleContextClass$new(
              indexVarExpr = parse(text = paste0(".block", cnt))[[1]],
              indexRangeExpr = if (scalarConstants[idx]) {
                substitute(A:A, list(A = expr[[idx + 2]]))
              } else {
                expr[[idx + 2]]
              }
            )
            expr[[idx + 2]] <- newSingleContexts[[cnt]]$indexVarExpr
          }
          context <- modelContextClass$new(newSingleContexts)
        }
      }
      super$initialize(expr, ID, context = context, constants = constants, isRHS = TRUE)
    }
  )
)
