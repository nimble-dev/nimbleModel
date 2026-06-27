# This file contains updated versions of nimble's contexts.
#
# A context for a model declaration is defined by the for-loops
# enclosing the declaration.
#

# FUTURE: we have no tests for contexts. Given this code is ported from original
# nimble, that may not be much of an issue.

# quick proxy to let code run without sifting through existing getNimbleOption calls
getNimbleOption <- function(...) FALSE

# A class for the information in one for-loop.

singleContextClass <- R6Class(
  classname = "singleContextClass",
  portable = FALSE,
  public = list(
    indexVarExpr = NULL,
    indexRangeExpr = NULL,
    forCode = NULL,
    initialize = function(indexVarExpr, indexRangeExpr, forCode) {
      if (missing(forCode)) {
        if (missing(indexVarExpr) || missing(indexRangeExpr)) {
          stop("either `forCode` or both `indexVarExpr` and `indexRangeExpr` must be provided.")
        }
        forCode <<- substitute(
          for (III in VVV) {},
          list(
            III = indexVarExpr,
            VVV = indexRangeExpr
          )
        )[1:3]
        indexVarExpr <<- indexVarExpr
        indexRangeExpr <<- indexRangeExpr
      } else {
        if (missing(indexVarExpr)) {
          indexVarExpr <<- forCode[[2]]
        } else {
          if (!identical(indexVarExpr, forCode[[2]])) {
            stop("`indexVarExpr` does not match the index variable in `forCode`.")
          }
          indexVarExpr <<- indexVarExpr
        }
        if (missing(indexRangeExpr)) {
          indexRangeExpr <<- forCode[[3]]
        } else {
          if (!identical(indexRangeExpr, forCode[[3]])) {
            stop("`indexRangeExpr` does not match the index range in `forCode`.")
          }
          indexRangeExpr <<- indexRangeExpr
        }
        # Remove any code from body of loop.
        if (length(forCode) > 3) forCode <- forCode[1:3]
        forCode <<- forCode
      }
    }
  )
)

# A class for the information in multiple for-loops (multiple single contexts).
modelContextClass <- R6Class(
  classname = "modelContextClass",
  portable = FALSE,
  public = list(
    singleContexts = list(), # list of `singleContext` objects
    indexVarExprs = list(), # list of index variable expressions

    indexVarNames = character(), # vector of index variable names

    # Sets all fields, which never change.
    initialize = function(singleContexts) {
      if (!missing(singleContexts) && length(singleContexts)) {
        singleContexts <<- lapply(
          singleContexts,
          function(x) {
            if (is.call(x)) {
              singleContextClass$new(forCode = x)
            } else if (inherits(x, "singleContextClass")) {
              x
            } else {
              stop("`singleContexts` must be a list of `singleContextClass` objects or a list of for-loop code.")
            }
          }
        )
        indexVarExprs <<- lapply(
          self$singleContexts,
          function(x) x$indexVarExpr
        )
        indexVarNames <<- if (length(indexVarExprs)) {
          sapply(indexVarExprs, as.character)
        } else {
          character(0)
        }
        names(singleContexts) <<- indexVarNames
      }
    },
    embedCodeInForLoop = function(innerLoopCode,
                                  useContext = NULL,
                                  allowNegativeIndexSequences = getNimbleModelOption("processBackwardsModelIndexRanges")) {
      # innerLoopCode: code to be embedded in (possibly nested) for-loops from this context
      # useContext: optional logical vector of which contexts to include
      # allowNegativeIndexSequences: if TRUE, for(i in 2:1) results in iterating over c(2,1), as R would.
      # otherwise (default is FALSE), behavior is like BUGS: for(i in 2:1) results in no iteration.
      if (is.null(useContext)) {
        useContext <- rep(TRUE, length(singleContexts))
      }
      iContext <- length(singleContexts)
      while (iContext >= 1) {
        if (useContext[iContext]) {
          newCode <- singleContexts[[iContext]]$forCode
          if (!allowNegativeIndexSequences) {
            indexRangeCode <- newCode[[3]]
            isColonExpr <- ifelse(is.name(indexRangeCode[[1]]),
              as.character(indexRangeCode[[1]]) == ":", FALSE
            )
            if (isColonExpr) {
              newCode[[3]] <- as.call(
                list(
                  as.name("seqNoDecrease"),
                  indexRangeCode[[2]],
                  indexRangeCode[[3]]
                )
              )
            }
          }
          newCode[[4]] <- innerLoopCode
          innerLoopCode <- newCode
        }
        iContext <- iContext - 1
      }
      return(innerLoopCode)
    }
  )
)


# Evaluate loops in context to determine unrolled index information.
expandContextAndReplacements <- function(allReplacements, allReplacementNameExprs, context, constants) {
  # `allReplacements` is a list like `list(i = i, i_plus_1 = i+1, mean_x_1to5 = mean(x[1:5]))`
  # `context` is a `modelContextClass` object
  # `constants` is an environment with constants that can be used to permanently replace values in the `allReplacements` code


  numContexts <- length(context$singleContexts)
  if (!numContexts) { # No indices or known indices
    if (!length(allReplacements)) {
      context$replacementsEnv <<- NULL
      return(NULL)
    }
  }

  # When done, we will have created a new environment and want to remove the constants from it.
  namesToRemoveAtEnd <- ls(constants)
  constantsCopy <- list2env(as.list(constants, all.names = TRUE),
    parent = getDefaultNamespace()
  )
  # Some replacements like min(j:100) should no longer be needed but are still there.

  # If this all works, `useContext` can be removed.
  useContext <- rep(TRUE, numContexts)

  valueVarNames <- if (numContexts > 0) paste0("INDEXVALUE_", 1:numContexts, "_") else character(0)
  # `indexRecordingCode` gives lines of code like "INDEXVALUE_1_[iAns] <- i". This will later have its name changed to "i"
  indexRecordingCode <- vector("list", length = numContexts)
  for (i in seq_along(context$singleContexts)) {
    if (useContext[i]) {
      indexRecordingCode[[i]] <- substitute(
        V[iAns] <- index,
        list(
          V = as.name(valueVarNames[i]),
          index = context$singleContexts[[i]]$indexVarExpr
        )
      )
    }
  }

  numReplacements <- length(allReplacements)
  useReplacement <- unlist(lapply(
    allReplacementNameExprs,
    function(x) { # do not use replacements that are identical to indexVars
      for (i in seq_along(context$singleContexts)) {
        if (identical(context$singleContexts[[i]]$indexVarExpr, x)) {
          return(FALSE)
        }
      }
      return(TRUE)
    }
  ))
  # `replacementRecordingCode` gives lines of code like "i_plus_1[iAns] <- i+1".
  replacementRecordingCode <- vector("list", length = numReplacements)
  for (i in seq_along(replacementRecordingCode)) {
    if (useReplacement[i]) {
      replacementRecordingCode[[i]] <- substitute(
        A[[iAns]] <- B,
        list(
          A = allReplacementNameExprs[[i]],
          B = allReplacements[[i]]
        )
      )
    }
  }

  # From here through the while loop combines the for loops from the contexts,
  # with the `replacementRecordingCode` and `indexRecordingCode` in the innermost.
  innerLoopCode <- as.call(c(list(quote(`{`)), replacementRecordingCode, indexRecordingCode, quote(iAns <- iAns + 1)))

  innerLoopCode <- context$embedCodeInForLoop(innerLoopCode, useContext)
  # This is a hacky way to deal with `nimC`, which is not in `constantsCopy`.
  if (length(innerLoopCode[[3]]) > 1 && innerLoopCode[[3]][[1]] == "nimC") {
    innerLoopCode[[3]][[1]] <- quote(c)
  }

  # At this point `innerLoopCode` has the full loop
  outputSize <- determineContextSize(context, useContext, constantsCopy)
  for (i in seq_along(context$singleContexts)) {
    if (useContext[i]) {
      assign(valueVarNames[i], numeric(outputSize), constantsCopy)
    }
  }
  for (i in seq_along(replacementRecordingCode)) {
    if (useReplacement[i]) {
      assign(names(allReplacements)[i], vector("list", length = outputSize), constantsCopy)
    }
  }
  assign("iAns", 1, constantsCopy)
  eval(innerLoopCode, constantsCopy)
  for (i in seq_along(context$singleContexts)) {
    if (useContext[i]) {
      constantsCopy[[as.character(context$singleContexts[[i]]$indexVarExpr)]] <- constantsCopy[[valueVarNames[i]]]
      rm(list = valueVarNames[i], envir = constantsCopy)
    }
  }
  # Turn lists into vectors when all elements are scalars.
  # When not, ensure all list elements are numeric, not integer, to avoid compiler mix-ups.
  for (i in seq_along(allReplacementNameExprs)) {
    if (useReplacement[i]) {
      unlistScalarCode <- substitute(
        {
          FOO_allScalar <- all(unlist(lapply(VARNAME, function(x) length(x) == 1)))
          if (FOO_allScalar) {
            VARNAME <- unlist(VARNAME) # Ok to have integers here
          } else {
            for (FOO_i in seq_along(VARNAME)) storage.mode(VARNAME[[FOO_i]]) <- "double" # but not here
            rm(FOO_i)
          }
          rm(FOO_allScalar)
        },
        list(VARNAME = allReplacementNameExprs[[i]])
      )
      eval(unlistScalarCode, envir = constantsCopy)
    }
  }

  rm(list = c(namesToRemoveAtEnd, "iAns"), envir = constantsCopy)
  assign("outputSize", outputSize, constantsCopy)
  return(constantsCopy) # becomes replacementsEnv
}

# Determines number of index elements in the nested looping by creating and
# executing nested for loops.
determineContextSize <- function(context, useContext = rep(TRUE, length(context$singleContexts)),
                                 evalEnv = new.env(parent = getDefaultNamespace())) {
  # FUTURE: Could improve this by checking for nested loops that don't use indices from outer loops.
  innerLoopCode <- quote(iAns <- iAns + 1)
  innerLoopCode <- context$embedCodeInForLoop(innerLoopCode, useContext)

  # This is a hacky way to deal with `nimC`, which is not in `evalEnv`.
  if (length(innerLoopCode[[3]]) > 1 && innerLoopCode[[3]][[1]] == "nimC") {
    innerLoopCode[[3]][[1]] <- quote(c)
  }

  assign("iAns", 0L, evalEnv)
  test <- try(eval(innerLoopCode, evalEnv), silent = TRUE)
  if (inherits(test, "try-error")) {
    stop("could not evaluate loop syntax `", safeDeparse(innerLoopCode), "`. Is indexing information provided via `constants`?")
  }
  ans <- evalEnv$iAns
  rm(list = c("iAns", context$indexVarNames[useContext]), envir = evalEnv)
  return(ans)
}

seqNoDecrease <- function(a, b) {
  if (a > b) {
    messageIfVerbose("  [Warning] Detected backwards indexing in `", a, ":",b, "`. This is likely unintended and will likely not produce valid model code.")
    numeric(0)
  } else {
    a:b
  }
}

getDefaultNamespace <- function() {
  return(baseenv())
}
