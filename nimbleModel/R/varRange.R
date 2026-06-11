# A varRangeClass object represents a set of index values for a variable,
# covering all the index slots (i.e., index positions).

# It does not represent the *values* of the variable.  Instead it symbolically
# represents some subset of the variable to be manipulated and passed.

# It can represent
# -  entire variables: x
# -  variable blocks: x[1:6, 2:5]
# -  arbitrary single-index subsets in each index:
#         x[c(2, 4, 6), c(3, 5, 7)]
# -  combinations of index blocks and single-index subsets:
#         x[c(2, 4, 6), 3:7]
# -  arbitrary indices given as rows of a matrix:
#         x[ matrix(c(1, 2, 10, 12, 5, 2), ncol = 2) ]
# -  combinations of matrices, index blocks, and single-index subsets
#         x[ matrix(...), c(2, 4, 6), 3:5 ]
# Internally, it manages multiple representations of the indices that are
# useful in different ways.

# TODO: 'varName' -> name?

varRangeClass <- R6Class(
  "varRangeClass",
  portable = FALSE,
  public = list(
    varName = character(),
    fromStochRule = logical(), # used when returned as result of graphRule (need for graph traversal)
    indexRangeExprs = list(), # e.g., `1:10`, `c(2,4,6)`
    indexRanges = list(),

    # These next two should be integer type (arbitrary but good to do for ease of testing).
    # They contain the information indicating how index slots relate to indexRanges.
    # Example: x[3, 1, 11], x[3, 2, 11], x[10, 1, 8], x[10, 2, 8]
    # An indexRangeMatrix handles 1st/3rd index slots, while a sequence covers second.
    rangeToIndexSlot = list(), # e.g., list(c(1,3), 2) for a matrix covering 1st/3rd index slots
    indexSlotToRange = integer(), # e.g., c(1,2,1)

    initialize = function(indexInfo,
                          rangeToIndexSlot = NULL,
                          varName = NULL,
                          fromStochRule = NULL) {
      fromStochRule <<- fromStochRule
      if (inherits(indexInfo, "indexRangeClass")) {
        stop("`varRange` must be initialized from a list of `indexRange`s, not a single `indexRange`.")
      }
      if (is.character(indexInfo)) {
        indexInfo <- parse(text = indexInfo, keep.source = FALSE)[[1]]
      }

      # Input is an expression.
      if (is.call(indexInfo) || is.name(indexInfo)) {
        if (length(indexInfo) == 1) {
          # The expression is just a name.
          nameFromExpr <- as.character(indexInfo)
        } else {
          # The expression must have some indexing so it must start with `[`.
          if (!identical(indexInfo[[1]], as.name("["))) {
            stop("input is not a valid variable or `varRange`.")
          }
          nameFromExpr <- as.character(indexInfo[[2]]) # safeDeparse(indexInfo[[2]], warn = TRUE)
          indexRangeExprs <<- as.list(indexInfo[-c(1, 2)])
          indexRanges <<- lapply(indexRangeExprs, newIndexRange)

          # Truncate indexRangeExprs for matrices for nicer printing.
          if (length(indexRanges) == 1 && inherits(indexRanges[[1]], "indexRangeMatrixClass")) {
            indexRangeExprs <<- indexRanges[[1]]$toExpr()
          } else if (any(unlist(lapply(indexRanges, function(x) inherits(x, "indexRangeMatrixClass"))))) {
            indexRangeExprs <<- lapply(indexRanges, function(x) x$toExpr())
          }
          rangeToIndexSlot <<- as.list(seq_along(indexRanges))
        }
        if (is.null(varName)) {
          varName <<- nameFromExpr
        } else {
          if (!identical(varName, nameFromExpr)) {
            messageIfVerbose("  [Warning] Variable name `", varName, "` does not match variable name `", nameFromExpr, "` in input expression. Using name from expression.")
            varName <<- nameFromExpr
          } else {
            varName <<- varName
          }
        }
      } else {
        # Input is a list that should be of `indexRange`s.
        if (is.list(indexInfo)) {
          if (length(indexInfo)) {
            if (!all(sapply(indexInfo, function(x) inherits(x, "indexRangeClass")))) {
              stop("`indexInfo` should be a list of `indexRange`s.")
            }
            setIndexRanges(indexInfo, rangeToIndexSlot)
            indexRangeExprs <<- lapply(indexRanges, function(x) x$toExpr())
          }
          varName <<- varName
        } else {
          stop("unexpected input.")
        }
      }
      if (length(self$rangeToIndexSlot) == 1) {
        indexSlotToRange <<- rep(1, length(self$rangeToIndexSlot[[1]])) # This saves a bit of time in the simple case.
      } else if (length(self$rangeToIndexSlot)) {
        rangeID <- rep(seq_along(self$rangeToIndexSlot), times = sapply(self$rangeToIndexSlot, length))
        indexSlotToRange <<- rangeID[order(unlist(self$rangeToIndexSlot))]
      }
    },
    setIndexRanges = function(indexRanges, rangeToIndexSlot = NULL) {
      # Helper method for `initialize` to set `indexRanges` and `rangeToIndexSlot`
      # based on list input, each element as returned by `indexRange` and possibly
      # an `rangeToIndexSlot` list relating each `indexRange` to one or more index slots.
      indexRanges <<- indexRanges
      if (!is.null(rangeToIndexSlot)) {
        rangeToIndexSlot <<- lapply(rangeToIndexSlot, as.integer)
      } else {
        # Assign elements of `rangeToIndexSlot` sequentially based on number of columns.
        nextID <- 1
        rangeToIndexSlot <<-
          lapply(
            indexRanges,
            function(x) {
              numCols <- x$numColumns
              if (is.null(numCols)) {
                stop("unexpected lack of `numCols`")
              } # empty indexRange case
              ans <- nextID - 1 + (1:numCols)
              nextID <<- nextID + numCols
              as.integer(ans)
            }
          )
      }
      return(NULL)
    },

    # Extract one or more columns of a varRange.
    # If multiple columns, result is expanded as a matrix of indices.
    extractIndexRange = function(indices, returnUsedRanges = FALSE) {
      usedIndices <- unlist(lapply(rangeToIndexSlot, function(x) x[x %in% indices]))
      usedIndicesBool <- lapply(rangeToIndexSlot, function(x) x %in% indices)
      usedRanges <- which(unlist(lapply(usedIndicesBool, any)))

      if (!length(usedRanges)) {
        return(NULL)
      } else {
        indexRangesList <- lapply(usedRanges, function(i) {
          innerIndices <- which(usedIndicesBool[[i]])
          return(indexRanges[[i]]$getColumns(innerIndices))
        })

        if (length(indexRangesList) == 1) {
          if (inherits(indexRangesList[[1]], "indexRangeMatrixClass") &&
            indexRangesList[[1]]$numColumns > 1) {
            mtch <- match(indices, usedIndices)
            if (identical(mtch, seq_len(indexRangesList[[1]]$numColumns))) {
              indexRangeResult <- indexRangesList[[1]]
            } else {
              indexRangeResult <- indexRangeMatrixClass$new(
                indexRangesList[[1]]$values[, mtch, drop = FALSE],
                sort = FALSE
              )
            }
          } else {
            indexRangeResult <- indexRangesList[[1]]
          }
        } else {
          indexRangeResult <- crossIndexRanges(indexRangesList, order = match(indices, usedIndices)) # result is an indexRangeMatrix
        }
      }
      return(indexRangeResult)
    },

    # TODO: perhaps return list(min = ..., max = ...)?
    getMinMax = function() {
      ranges <- lapply(indexRanges, function(x) x$getMinMax())
      result <- matrix(0, length(indexSlotToRange), 2)
      for (i in seq_along(ranges)) {
        result[rangeToIndexSlot[[i]], ] <- ranges[[i]]
      }
      return(result)
    },

    # TODO: remove this
    # isEmpty = function() {
    #     return(any(sapply(self$indexRanges,
    #                       function(x) is(x, "indexRangeEmptyClass"))))
    # },

    isNone = function() { # No indexing case.
      return(!length(indexRanges))
    },

    # `toExpr` takes a `varRange` object and returns the corresponding
    # expression.  This inverts the initialize function of `varRangeClass` for
    # an expression input and imputes the index values in the case of a
    # a `varRange` initialized from a list of `indexRange`s.
    # Example 1: varRange2expr(varRangeClass$new(quote(x[1:10]))) ==> "x[1:10]"
    # Example 2: varRange2expr(varRangeClass$new(list(indexRange(quote(1:10), varName = 'x'))))
    #  ==> "x[1:10]"
    toExpr = function() {
      if (is.null(varName)) {
        nm <- as.name("no_name")
      } else {
        nm <- as.name(varName)
      }
      if (isNone()) {
        return(nm)
      } else {
        return(
          do.call("call",
            c(
              list("[", nm),
              indexRangeExprs[indexSlotToRange]
            ),
            quote = TRUE
          )
        )
      }
    },

    # `toChar` takes a varRange object and returns the corresponding
    # character string of the original expression (or the imputed expression
    # when initialized from a list of `indexRange`s).
    toChar = function() {
      safeDeparse(toExpr(), warn = TRUE)
    },

    # Convert to set of character strings, as few as possible,
    # similar to format of nodes in original nimble, but without
    # consideration of what is a node.
    # e.g., "y[1:3, 1:5]" and c("y[1, 1:5]", "y[3, 1:5]").
    toVarChars = function(expandScalars = FALSE) {
      if (isNone()) {
        return(varName)
      }
      indexRangeClasses <- sapply(indexRanges, function(x) class(x)[1])
      if (any(indexRangeClasses == "indexRangeMatrixListClass")) {
        stop("`toVarChars` does not handle indexRangeMatrixList class elements.")
      }

      if (expandScalars) {
        if (length(indexRanges) == 1) {
          externalMatrix <- indexRanges[[1]]$getValuesAsMatrix()
        } else {
          externalMatrix <- crossIndexRanges(indexRanges)$values
        } # not ordered
        tmp <- t(apply(externalMatrix, 1, as.character))
        if (ncol(externalMatrix) == 1) {
          tmp <- t(tmp)
        }
        indicesList <- list()
        indices <- unlist(rangeToIndexSlot)
        for (i in seq_along(indices)) {
          indicesList[[indices[i]]] <- tmp[, i]
        }
        return(paste0(varName, "[", do.call(pasteIndices, indicesList), "]"))
      }

      boolMatrixIndexRanges <- indexRangeClasses == "indexRangeMatrixClass"
      matrixIndices <- unlist(rangeToIndexSlot[boolMatrixIndexRanges])
      nonMatrixIndices <- unlist(rangeToIndexSlot[!boolMatrixIndexRanges])

      indicesList <- list()
      length(indicesList) <- length(indexSlotToRange)

      if (sum(!boolMatrixIndexRanges)) {
        nonMatrixExprs <- sapply(indexRangeExprs[!boolMatrixIndexRanges], deparse)
        for (i in seq_along(nonMatrixExprs)) {
          indicesList[[nonMatrixIndices[i]]] <- nonMatrixExprs[i]
        }
      }

      if (sum(boolMatrixIndexRanges)) {
        if (sum(boolMatrixIndexRanges) == 1) {
          externalMatrix <- indexRanges[[which(boolMatrixIndexRanges)]]$getValuesAsMatrix()
        } else {
          externalMatrix <- crossIndexRanges(indexRanges[boolMatrixIndexRanges])$values
        } # not ordered
        tmp <- t(apply(externalMatrix, 1, as.character))
        if (ncol(externalMatrix) == 1) {
          tmp <- t(tmp)
        }
        for (i in seq_along(matrixIndices)) {
          indicesList[[matrixIndices[i]]] <- tmp[, i]
        }
      }
      return(paste0(varName, "[", do.call(pasteIndices, indicesList), "]"))
    },
    toRule = function() {
      constants <- list()
      if (length(indexRanges)) {
        indexVars <- paste0("idx", seq_along(indexRanges))
        expr <- quote(y[1])
        expr[[2]] <- as.name(varName)
        expr[3:(2 + length(indexSlotToRange))] <- sapply(indexVars[indexSlotToRange], function(x) as.name(x))
        irMatrices <- which(sapply(indexRanges, function(range) inherits(range, "indexRangeMatrixClass")))
        if (length(irMatrices)) {
          idxExpr <- quote(k[idx])
          for (i in irMatrices) {
            cnt <- 1
            for (j in seq_along(rangeToIndexSlot[[i]])) {
              constantName <- paste0("k", rangeToIndexSlot[[i]][j])
              idxExpr[[2]] <- as.name(constantName)
              idxExpr[[3]] <- as.name(indexVars[i])
              expr[[2 + rangeToIndexSlot[[i]][j]]] <- idxExpr
              constants[[constantName]] <- indexRanges[[i]]$values[, cnt]
              cnt <- cnt + 1
            }
          }
        }
        singleContexts <- sapply(
          seq_along(indexRanges),
          function(i) {
            switch(class(indexRanges[[i]])[1],
              "indexRangeMatrixClass" =
                singleContextClass$new(
                  indexVarExpr = as.name(paste0("idx", i)),
                  indexRangeExpr = substitute(1:M, list(M = indexRanges[[i]]$numElements))
                ),
              "indexRangeSequenceClass" =
                singleContextClass$new(
                  indexVarExpr = as.name(paste0("idx", i)),
                  indexRangeExpr = substitute(M1:M2, list(
                    M1 = indexRanges[[i]]$start,
                    M2 = indexRanges[[i]]$end
                  ))
                ),
              "indexRangeScalarClass" =
                singleContextClass$new(
                  indexVarExpr = as.name(paste0("idx", i)),
                  indexRangeExpr = substitute(M:M, list(M = indexRanges[[i]]$value))
                ),
              stop("invalid indexRange type")
            )
          }
        )
        context <- modelContextClass$new(singleContexts)
      } else {
        expr <- as.name(varName)
        context <- modelContextClass$new()
      }
      return(rhsRuleClass$new(expr, context = context, constants = constants))
    },
    print = function() {
      cat("variable range for `", toChar(), "`.\n", sep = "")
    }
  )
)

# This now catches cases where the order of the indexRanges
# is permuted consistent with `rangeToIndexSlot` and `indexSlotToRange`.
# TODO: check testing and perhaps add more testing.
varRange_isEqual <- function(vr1, vr2) {
  if (length(vr1$indexRanges) != length(vr2$indexRanges)) {
    return(FALSE)
  }
  # mtch <- match(vr1$indexSlotToRange, vr2$indexSlotToRange)
  crossref <- unique(cbind(vr1$indexSlotToRange, vr2$indexSlotToRange))
  return(nrow(crossref) == length(vr1$indexRanges) &&
    isTRUE(all.equal(vr1$indexRanges[crossref[, 1]], vr2$indexRanges[crossref[, 2]])))
}

getVarName <- function(x) {
  if (inherits(x, "varRangeClass")) {
    return(x$varName)
  }
  if (is.character(x)) {
    # String operations are faster than parse/deparse.
    return(strsplit(x, "[", fixed = TRUE)[[1]][1])
  }
  if (is.call(x) || is.name(x)) {
    if (length(x) == 1) {
      return(safeDeparse(x, warn = TRUE))
    } else {
      return(safeDeparse(x[[2]], warn = TRUE))
    }
  }
  if (is.null(x)) {
    return(NULL)
  }
  stop("unexpected input: `", x, "`.")
}

# Remove duplicates from an arbitrary set of varRanges.
removeDuplicateVarRanges <- function(varRanges) {
  varNames <- unlist(lapply(varRanges, function(range) range$varName))
  uniqVarNames <- unique(varNames)
  varRanges <- lapply(uniqVarNames, function(nm) {
    varRanges[varNames == nm]
  })
  names(varRanges) <- uniqVarNames
  return(flatten(lapply(varRanges, function(vr) removeDuplicateVarRangesOne(vr))))
}

# Remove duplicates from a set of varRanges for a single variable.
removeDuplicateVarRangesOne <- function(varRanges) {
  mx <- length(varRanges)
  if (mx == 1) {
    return(varRanges)
  }

  varRangeIDs <- seq_len(mx)
  dups <- rep(FALSE, mx)
  for (id in 1:(mx - 1)) {
    equal <- unlist(lapply((id + 1):mx, function(id2) {
      varRange_isEqual(varRanges[[id]], varRanges[[id2]])
    }))
    dups[(id + 1):mx] <- dups[(id + 1):mx] | equal
  }
  return(varRanges[!dups])
}

# Flatten nested lists.
flatten <- function(x) {
  result <- do.call(c, x)
  names(result) <- NULL
  if (identical(result, list(NULL))) {
    return(NULL)
  }
  result <- result[!sapply(result, is.null)]
  return(result)
}


# TODO: need combine() that combines "adjacent" varRanges

# scalar+seq = seq
# seq + seq = seq
# seq + matrix = omit seq elements from the matrix
# mat + mat = mat
# needs to deal with identical indices
