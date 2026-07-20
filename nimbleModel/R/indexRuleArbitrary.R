# This rule handles arbitrary, unstructured translations, such as
# `y[i] <- x[k[i]]` and `y[i, k[i]]` <- x[i]` and `y[i+j] <- x[i,j]`.
indexRuleArbitraryClass <- R6Class(
  classname = "indexRuleArbitraryClass",
  inherit = indexRuleClass,
  portable = FALSE,
  public = list(
    setupResults = NULL,
    initialize = function(toIndexExprList,
                          fromIndexExprList,
                          context,
                          constants = list()) {
      setupResults <<-
        indexRuleArbitrary_setup(
          toIndexExprList,
          fromIndexExprList,
          context,
          constants
        )
    },
    apply = function(indexRange, collapse = TRUE) {
      if (!inherits(indexRange, "indexRangeClass")) {
        stop("an index rule must be applied to an `indexRange`.")
      }
      indexRuleArbitrary_applyMatrix(indexRange$getValuesAsMatrix(), setupResults,
                                     collapse = collapse)
    },
    getMax = function() {
      sapply(
        setupResults$fromInfo,
        function(fromInfo) fromInfo$offset + fromInfo$size
      )
    },
    getNumElements = function() {
      return(setupResults$unrolledSize)
    }
  )
)

indexRuleArbitrary_setup <- function(toIndexExprList,
                                     fromIndexExprList,
                                     context,
                                     constants) {
  # Valid only when indexing in both to and from.
  if (!length(toIndexExprList) || !length(fromIndexExprList)) {
    return(NULL)
  }

  allReplacements <- c(toIndexExprList, fromIndexExprList)
  toIndexNames <- lapply(names(toIndexExprList), as.name)
  fromIndexNames <- lapply(names(fromIndexExprList), as.name)
  allIndexNames <- c(toIndexNames, fromIndexNames)

  # Run the for loops in an environment
  # where all the results are created.
  unrolledIndicesEnv <-
    expandContextAndReplacements(
      allReplacements = allReplacements,
      allReplacementNameExpr = allIndexNames,
      context = context,
      constants = constants
    )

  # Extract the results from the environment.
  toUnrolledResults <-
    lapply(
      names(toIndexExprList),
      function(x) unrolledIndicesEnv[[x]]
    )
  fromUnrolledResults <-
    lapply(
      names(fromIndexExprList),
      function(x) unrolledIndicesEnv[[x]]
    )
  if (any(is.na(unlist(toUnrolledResults))) || any(is.na(unlist(fromUnrolledResults)))) {
    stop("indexRuleArbitrary_setup: Missing values found. Constants may be incorrect size.")
  }

  unrolledSize <- unrolledIndicesEnv$outputSize

  # Helper function to determine if results are scalar or not.
  isScalarIndex <- function(unrolledResult) {
    !is.list(unrolledResult)
  }

  from_allScalar <- all(sapply(fromUnrolledResults, isScalarIndex))
  to_allScalar <- all(sapply(toUnrolledResults, isScalarIndex))


  fromInfo <- lapply(names(fromIndexExprList), makeInfo, unrolledIndicesEnv)
  from_flatMax <- prod(unlist(lapply(fromInfo, `[[`, "size")))

  toInfo <- lapply(names(toIndexExprList), makeInfo, unrolledIndicesEnv)

  from2indicesFunctions <- make2IndicesFunctions(fromInfo)
  to2indicesFunctions <- make2IndicesFunctions(toInfo)

  # Set up `from_flat2iRow`.
  # iRow refers to the "row" of the unrolledIndicesEnv.
  # It is a cumulative index of all for-loop unrolling.
  # The output of `from_flat2iRow` will be a list.  The ith element of the
  # list gives the `iRow` values for which flat index i of the
  # "from" variable is involved.

  # Example:
  # for(i in 1:5)
  #   for(j in 1:3)
  #      y[i, j] <- foo(x[j+1, i+2]

  # The flat indices for x will be for x[2:4, 3:7],
  # starting at x[2, 3].
  # So x[2,5] will have flat index 7.
  # from_flat2iRow[[7]] will have 11, because the 11th iteration
  # of the nested for loops will touch x[2,5].
  # There can be multiple iterations that touch x[2,5] if one
  # of the indices is sub-indexed.
  if (from_allScalar) {
    # Case of all scalar indices can be handled more efficiently.
    allIndices <- do.call("cbind", fromUnrolledResults)
    from_flat <- from2indicesFunctions$rawIndex2flatIndex_multi(allIndices)
    # split() works when there is no raggedness to the declarations
    # from_flat2iRow <- split(1:unrolledSize, from_flat)
    # The following is slower than split() but more general.
    # Eventually this might be in C++.
    from_flat2iRow <- vector("list", length = from_flatMax)
    for (i in 1:unrolledSize) {
      from_flat2iRow[[from_flat[i]]] <-
        c(from_flat2iRow[[from_flat[i]]], i)
    }
  } else {
    # Case with some vector indices require more care and will be less efficient.
    allIndicesList <- do.call(
      "mapply",
      c(
        list(as.name("expand.grid")),
        fromUnrolledResults,
        list(SIMPLIFY = FALSE)
      )
    )
    iRows <- rep(1:unrolledSize, times = unlist(lapply(allIndicesList, nrow)))
    allIndices <- do.call("rbind", allIndicesList)
    from_flat <- from2indicesFunctions$rawIndex2flatIndex_multi(allIndices)
    # Again, split() would work if there is no raggedness in the the loop ranges.
    # from_flat2iRow <- split(iRows, from_flat)

    from_flat2iRow <- vector("list", length = from_flatMax)
    for (i in seq_along(iRows)) {
      from_flat2iRow[[from_flat[i]]] <-
        c(from_flat2iRow[[from_flat[i]]], iRows[i])
    }
  }

  # Set up iRow2toIndices.
  # `iRow2toIndices` is a list whose ith element
  # gives the indices (as a matrix) of the "to" variable
  # touched by unrolled `iRow=i`.

  # Example:
  #    for(i in 1:5)
  #     x[i+1, 1:3] <- foo(y[i])
  #
  # `iRow2toIndices[[2]]` will be the matrix
  #   3 1
  #   3 2
  #   3 3
  # because the 2nd iteration of the loop unrolling
  # creates x[3, 1:3].

  # N.B. We aim for this example to be handled more efficiently,
  # by a separate index inverter, but the current one should
  # handle any case.
  if (to_allScalar) {
    allIndices <- do.call("cbind", toUnrolledResults)
    iRow2toIndices <- split(allIndices, 1:unrolledSize) # Result is a list
  } else {
    # This returns a list in the right form.
    allIndicesList <- do.call(
      "mapply",
      c(
        list(as.name("expand.grid")),
        toUnrolledResults,
        list(SIMPLIFY = FALSE)
      )
    )
    iRows <- rep(1:unrolledSize, times = unlist(lapply(allIndicesList, nrow)))
    allIndices <- do.call("rbind", allIndicesList)
    iRow2toIndices <- split(allIndices, iRows)
  }

  # Simplify to list of numeric matrices (not data frames) for
  # consistent types and simpler handling later.
  iRow2toIndices <- lapply(iRow2toIndices, function(x) {
    if (is.null(dim(x))) {
      result <- as.numeric(x)
    } else {
      result <- matrix(as.numeric(as.matrix(x)), ncol = ncol(x))
      dimnames(result) <- NULL
    }
    return(result)
  })
  names(iRow2toIndices) <- NULL

  return(list(
    from2indicesFunctions = from2indicesFunctions,
    to2indicesFunctions = to2indicesFunctions,
    from_flat2iRow = from_flat2iRow,
    iRow2toIndices = iRow2toIndices,
    fromInfo = fromInfo,
    toInfo = toInfo,
    unrolledSize = unrolledSize,
    from_flatMax = from_flatMax
  ))
}

# A helper function to extract range, offset and size information
# from a set of index results.  Note these do not need to be
# relevant for the entire variable in the model.  They only
# need to be relevant for the block of the variable touched
# in this declaration with its context.
makeInfo <- function(indexName, unrolledIndicesEnv) {
  indexValues <- unlist(unrolledIndicesEnv[[indexName]])
  frange <- range(indexValues)
  foffset <- frange[1] - 1
  fsize <- diff(frange) + 1
  return(list(offset = foffset, size = fsize))
}

# A helper function that returns functions for converting from a set of
# "real" indices to a flat index defined for the part of the variable used
# in the declaration with its context.
# Data for the specific indexRule are in the closure of the functions.
make2IndicesFunctions <- function(info) {
  sizes <- unlist(lapply(info, `[[`, "size"))
  totSize <- prod(sizes)
  strides <- c(1, cumprod(sizes[-length(sizes)]))
  offsets <- unlist(lapply(info, `[[`, "offset"))
  convertSingle <- function(Fval) {
    shiftedF <- Fval - offsets
    valid <- all(shiftedF >= 1 & shiftedF <= sizes)
    if (!valid) {
      return(matrix(
        data = numeric(),
        nrow = 0, ncol = length(sizes)
      ))
    } else {
      return(1 + (sum((shiftedF - 1) * strides)))
    }
  }

  invertSingle <- function(flat) {
    flat <- flat - 1
    shiftedF <- integer()
    for (i in rev(seq_along(strides))) {
      if (i == 1) {
        shiftedF <- c(flat + 1, shiftedF)
      } else {
        thisIndex <- flat %/% strides[i]
        shiftedF <- c(thisIndex + 1, shiftedF)
        flat <- flat - thisIndex * strides[i]
      }
    }
    return(shiftedF + offsets)
  }
  convertMany <- function(Fval) {
    apply(Fval, 1, convertSingle)
  }

  return(list(
    rawIndex2flatIndex = convertSingle,
    rawIndex2flatIndex_multi = convertMany,
    flatIndex2rawIndex = invertSingle
  ))
}

indexRuleArbitrary_applyMatrix <- function(indexRangeMatrixValues,
                                           setupResults,
                                           collapse = TRUE) {
  # fromFlat is the flat index of each row of "from" indices.
  fromFlat <- setupResults$from2indicesFunctions$rawIndex2flatIndex_multi(indexRangeMatrixValues)
  # Deal with invalid from indices - need information retained for later collapsing with other columns.
  invalid <- sapply(fromFlat, function(x) length(x) == 0)
  if (length(invalid)) {
    fromFlat[invalid] <- NA
  }
  fromFlat <- unlist(fromFlat)

  # iRowsList has the declaration iRows for each fromFlat.
  iRowsList <- setupResults$from_flat2iRow[fromFlat]
  
  # CHECK: unique???
  # `toIndicesList` has the matrix of "to" indices for each fromFlat
  # need NAs in places where input matches no output to be able to
  # collapse via `collapse_indexRangeMatrices`.
  if (is.null(dim(setupResults$iRow2toIndices[[1]]))) {
    nr <- 1
  } else {
    nr <- nrow(setupResults$iRow2toIndices[[1]])
  }
  NAs <- matrix(rep(as.numeric(NA), length(setupResults$iRow2toIndices[[1]])),
    nrow = nr
  )
  toIndicesList <<- lapply(
    iRowsList,
    function(x) {
      result <- do.call(
        "rbind",
        setupResults$iRow2toIndices[x]
      )
      if (is.null(result)) {
        return(NAs)
      } else {
        return(result)
      }
    }
  )

  if (!length(toIndicesList)) {
    return(NULL)
  }

  # `applyGraphRule` will use `collapse=FALSE`, as we need to maintain correspondence
  # of rows of input indexRange (via toIndicesList) in order to cross results of multiple rules
  # applied to a multi-column input indexRange.
  # E.g., `y[i,j] <- x[k1[i],k2[j]]` can produce multiple output rows from an input row
  # e.g., if `k1[1:3] = c(2,2,4)` then x[2,] -> y[c(1,2),].
  # Or even  `y[i,j] <- x[k1[i],j]` which needs to cross with the result of the `j` block rule.

  if (collapse) {
    # This does not strip out NA cases or duplicates.
    toIndicesList <-
      toIndicesList[!unlist(lapply(toIndicesList, is.null))]
    return(indexRangeMatrixClass$new(do.call("rbind", toIndicesList)))
  } else {
    return(indexRangeMatrixListClass$new(toIndicesList))
  }
}
