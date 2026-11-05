type2itype <- list(
  # Note that conversion to nodeRange orders indices within a set of indices (corresponding to an indexRange)
  # so the need for reordering based on slots is only because of slots being disjoint across sets.
  # E.g., a rangeToIndexSlot of `list(2, c(3,1))` becomes `list(c(1,3), 2)` while `list(4, c(3,1,2))` becomes `list(1:3, 4)`
  "0" = 0,
  "1_seq" = 1,
  "1_mat" = 2,
  "1_matp" = 3,
  "2_seq_seq" = 4,
  "2_seq_mat" = 5,
  "2_mat_seq" = 6,
  "2_mat_mat" = 7,
  "2_seq_matp" = 8,
  "2_matp_seq" = 9,
  "2_matp_matp" = 10, # includes reordering and mat{1} cases
  "2_x_y_ord" = 11, # i.e., 2_{mat,seq}_{mat,seq}_ord; should not be needed, but include for safety
  "3_allseq" = 12,
  "3_generic" = 13,
  "4_allseq" = 14,
  "4_generic" = 15,
  "5_allseq" = 16,
  "5_generic" = 17,
  "1_matp_ord" = 18 # done, but not needed as conversion to nodeRange puts indices in order; also not clear why a user would specify indices out of order for single indexRange case
  #############################################
)

# Stand-alone function for setting up inputs to instrClass constructor.
# This could be embedded in the constructor.
range2instr <- function(range) {
  instr <- list()
  if (!length(range$indexingRange$indexRanges)) { # No indexing
    instr$lens <- 1
    instr$index_types <- 0
    instr$nDim <- 0
  } else {
    instr$lens <- sapply(range$indexingRange$indexRanges, function(x) x$numElements)
    instr$dims <- as.numeric(sapply(range$indexingRange$rangeToIndexSlot, length))
    instr$nDim <- sum(instr$dims)
    instr$slots <- as.numeric(unlist(range$indexingRange$rangeToIndexSlot))
    instr$index_types <- sapply(range$indexingRange$indexRanges, function(x) {
      switch(class(x)[1],
        "indexRangeScalarClass" = 2,
        "indexRangeSequenceClass" = 1,
        "indexRangeMatrixClass" = 2
      )
    })
    instr$values <- lapply(range$indexingRange$indexRanges, function(x) {
      switch(class(x)[1],
        "indexRangeScalarClass" = x$value,
        "indexRangeSequenceClass" = x$start,
        "indexRangeMatrixClass" = c(t(matrix(x$values, nc = x$numColumns)))
      )
    }) # in calcRange, column major; need row major here for simpler/more efficient determination of indices
  }
  instr <- add_instr_precalcs(instr)
  instr$sortID <- range$sortID
  instr$declID <- range$declID
  class(instr) <- "Rinstr"
  return(instr)
}

# Add precalculated (once) fields to an instruction:
#   nNodes:     number of nodes in the instruction.
#   strides:    these are strides for the *indexing* (nested as written in the *model code*), not the *data*. E.g. in for(i in 1:3) for(j in 1:6) {...}: j goes fastest so has stride=1; i has stride=6.
#   idxSlotStarts: number of idx slots filled by the ranges before each range, i.e. where its slots start.
#   instr_type: the iteration plan, from `determineInstrType`.
# Every place that builds instructions calls this, so they can't drift apart.
add_instr_precalcs <- function(instr) {
  lens <- as.numeric(instr$lens)
  nNodes <- prod(lens) # computed as double so the overflow check below can't itself overflow
  if (nNodes > .Machine$integer.max) {
    stop("an instruction has ", nNodes, " nodes, more than the maximum of ", .Machine$integer.max)
  }
  instr$nNodes <- nNodes
  nRanges <- length(lens)
  instr$strides <- rev(cumprod(c(1, rev(lens)[-nRanges])))
  instr$idxSlotStarts <- if (length(instr$dims)) c(0, cumsum(instr$dims)[-length(instr$dims)]) else numeric(0)
  instr$instr_type <- determineInstrType(instr)
  return(instr)
}

# Eventually think about reordering order of looping for efficiency (and take parallelization into account).
# For the moment, we determine mat vs. seq here and then in declFunClass calculate we will determine whether to
# vectorize based on whether possible based on the declaration.
# Open question of when to determine if to use parallel calculate.
determineInstrType <- function(instr, use_vec = FALSE) {
  instr_type <- NULL
  if (!instr$nDim) {
    instr_type <- "0"
  }
  if (length(instr$dims) == 1) {
    if (instr$index_types[1] == 1) {
      instr_type <- "1_seq"
    } else {
      if (instr$dims[1] == 1) {
        instr_type <- "1_mat"
      } else {
        if (identical(instr$slots, as.numeric(1:length(instr$slots)))) instr_type <- "1_matp" else instr_type <- "1_matp_ord"
      }
    }
  }
  if (length(instr$dims) == 2) {
    if (identical(instr$dims, c(1, 1))) {
      if (identical(instr$slots, c(2, 1))) {
        instr_type <- "2_x_y_ord"
      } else {
        if (identical(instr$index_types, c(1, 1))) instr_type <- "2_seq_seq"
        if (identical(instr$index_types, c(1, 2))) instr_type <- "2_seq_mat"
        if (identical(instr$index_types, c(2, 1))) instr_type <- "2_mat_seq"
        if (identical(instr$index_types, c(2, 2))) instr_type <- "2_mat_mat"
      }
    } else {
      instr_type <- "2_matp_matp"
      if (instr$index_types[1] == 1) instr_type <- "2_seq_matp"
      if (instr$index_types[2] == 1) instr_type <- "2_matp_seq"
    }
  }
  if (length(instr$dims) == 3) {
    if (all(instr$index_types == 1) && identical(instr$slots, as.numeric(1:length(instr$slots)))) {
      instr_type <- "3_allseq"
    } else {
      instr_type <- "3_generic"
    }
  }
  if (length(instr$dims) == 4) {
    if (all(instr$index_types == 1) && identical(instr$slots, as.numeric(1:length(instr$slots)))) {
      instr_type <- "4_allseq"
    } else {
      instr_type <- "4_generic"
    }
  }
  if (length(instr$dims) == 5) {
    if (all(instr$index_types == 1) && identical(instr$slots, as.numeric(1:length(instr$slots)))) {
      instr_type <- "5_allseq"
    } else {
      instr_type <- "5_generic"
    }
  }
  if (is.null(instr_type)) stop("no available specific instruction type")
  return(type2itype[[instr_type]])
}

# TODO: document this since it may be user-facing.
# This feels clunky in terms of what inputs it can handle;
# we may want to work more on the interface/what inputs are allowed.
# This only omits data nodes if given chars or varRanges.
#' @export
makeInstrList <- function(model, input, includeData = TRUE, use_vec = FALSE) {
  # This works with:
  # (1) a char vector of "nodes"
  # (2) a list of (or single) varRanges
  # (3) an nList of (or single) instr_nClass objects (assumed to be in sort order)
  # (4) an R list of (or single) Rinstr objects (created by `range2instr`) (not assumed to be in sort order)
  # (5) an `RinstrList` or `instrList_nClass` (returned as is)
  # It returns an `RinstrList` (see `make_RinstrList`), or NULL if there are no nodes.

  # Already a list of instructions with offsets.
  if (inherits(input, "RinstrList") || inherits(input, "instrList_nClass")) {
    return(input)
  }
  # A single instruction.
  if (inherits(input, "instr_nClass") || inherits(input, "Rinstr")) {
    return(make_RinstrList(list(input)))
  }
  # An nList of instructions.
  if (inherits(input, "nList")) {
    if (!inherits(input[[1]], "instr_nClass")) {
      stop("nList input to `makeInstrList` should contain `instr_nClass` objects")
    } else {
      return(make_RinstrList(input))
    }
  }

  # An R list of `Rinstr` elements, not assumed to be in sort order.
  if (is.list(input) && all(sapply(input, \(x) inherits(x, "Rinstr")))) {
    # Sort by sortID.
    sortIDs <- lapply(input, \(x) x$sortID)
    sortIDranges <- sapply(sortIDs, \(x) range(x, na.rm = TRUE))
    multiSortID <- which(sortIDranges[1, ] != sortIDranges[2, ])
    for (i in multiSortID) {
      if (!all(diff(sortIDs[[i]]) == 1, na.rm = TRUE)) {
        stop("multiple sortID values found for the ", i, "th instruction. Only sequential backward dependence is allowed when providing a list of instructions")
      } else { # Check for any overlapping sortID values for the sequential backward dependence calcRange.
        if (any(sortIDranges[2, -i] > sortIDranges[1, i] & sortIDranges[1, -i] < sortIDranges[2, i])) {
          stop("the multiple sortID values in the ", i, "th instruction overlap with sortID values in other instructions")
        }
      }
    }
    ord <- order(sortIDranges[1, ])
    input <- input[ord]
    return(make_RinstrList(input))
  }

  # Finally handle character vectors or varRanges.
  if (inherits(input, "varRangeClass")) input <- list(input)

  if (!(is.character(input) || all(sapply(input, \(x) inherits(x, "varRangeClass"))))) {
    stop("unexpected type for `input` argument")
  }

  input <- model$getNodes(input, includeData = includeData)
  if (!length(input)) {
    return(NULL)
  }

  # First apply calcRule to get overlap between input and the rule.
  # Then make the calcRange to convert to loop indexing.
  # Note that `calcRule$apply` handles converting char to varRange and handling full variable extent.
  ranges <- unlist(lapply(input, function(vr) {
    lapply(model$modelDef$calcRules[[getVarName(vr)]]$rules, function(rule) {
      rule$makeCalcRange(rule$apply(vr))
    })
  }))

  sortIDs <- lapply(ranges, \(x) x$sortID)
  sortIDranges <- sapply(sortIDs, \(x) range(x, na.rm = TRUE))
  multiSortID <- which(sortIDranges[1, ] != sortIDranges[2, ])

  newInstrs <- list()
  rangesToRemove <- numeric(0)
  for (i in multiSortID) {
    if (!all(diff(sortIDs[[i]]) == 1, na.rm = TRUE)) {
      # This quickly creates a list of R lists, where the elements mimic instr_nClass objects,
      # from a calcRange with multiple sortID values. Creating many calcRanges or instr_nClass objects is slow.
      # Note: `makeScalarInstrInfoLists` builds instructions with nDim = 1 (one idx slot), so it looks like it
      # will only work for declarations with a single loop index (checked by `check_instr_nDims` below).
      newInstrs <- c(newInstrs, ranges[[i]]$makeScalarInstrInfoLists())
      rangesToRemove <- c(rangesToRemove, i)
    } else { # Check for any overlapping sortID values for the sequential backward dependence calcRange.
      if (any(sortIDranges[2, -i] > sortIDranges[1, i] & sortIDranges[1, -i] < sortIDranges[2, i])) {
        stop("the multiple sortID values in the ", i, "th calcRange overlap with sortID values in other calcRanges")
      }
    }
  }
  if (length(rangesToRemove)) {
    ranges <- ranges[-rangesToRemove]
  }

  # Again, returning a list of R lists that mimic instr_nClass objects is much faster
  # than instantiating instr_nClass objects.
  Rlist <- c(newInstrs, lapply(ranges, \(x) range2instr(x)))
  sortIDs <- sapply(Rlist, \(x) min(x$sortID, na.rm = TRUE)) # `min` still needed for case of ascending sortIDs (e.g., dependence on the past), which are not split.
  Rlist <- Rlist[order(sortIDs)]
  check_instr_nDims(model, Rlist)
  return(make_RinstrList(Rlist))
}

# Check that each instruction fills as many idx slots as its declaration has loop indices.
# That is the length of `idx` the declaration's per-node methods expect, and in compiled code
# it determines which runners exist for the declaration (see declFunClass_.h).
# The loop indices come from `loopIndexingRule`, not `context`: a lifted node is given the same
# for-loop context as its original node, even if it doesn't use all of those indices
# (see the test "lifted node that uses only some of its loop indices" in test-nimbleModel.R).
check_instr_nDims <- function(model, instrs) {
  declInfo <- model$modelDef$declInfo
  for (instr in instrs) {
    nIdx <- length(declInfo[[instr$declID]]$declRule$loopIndexingRule$indexVarNames)
    if (instr$nDim != nIdx) {
      stop(
        "an instruction for declaration ", instr$declID, " fills ", instr$nDim,
        " index slots, but the declaration has ", nIdx, " loop indices"
      )
    }
  }
}

# Wrap a sort-ordered list of instructions (Rinstr lists or instr_nClass objects, or an nList of them)
# in the R-list form of `instrList_nClass`. Uncompiled code uses this directly; compiled code copies it
# into an `instrList_nClass` (see `makeCompiledInstrList`). The steps of the whole list are numbered in
# list order, so instruction j covers list steps offsets[j] + 1 through offsets[j] + nNodes of instruction j.
make_RinstrList <- function(instrs) {
  n <- length(instrs)
  nNodes <- vapply(seq_len(n), \(j) as.numeric(instrs[[j]]$nNodes), numeric(1))
  nNodes_total <- sum(nNodes) # double, so the overflow check below can't itself overflow
  if (nNodes_total > .Machine$integer.max) {
    stop("a list of instructions has ", nNodes_total, " nodes, more than the maximum of ", .Machine$integer.max)
  }
  result <- list(
    instr_nList = instrs,
    offsets = if (n) c(0, cumsum(nNodes)[-n]) else numeric(0),
    nNodes_total = nNodes_total
  )
  class(result) <- "RinstrList"
  return(result)
}

# Fill `idx` for one step (node) of an instruction, with no loop. `step` is 1-based.
# The counter of range r is ((step - 1) %/% strides[r]) %% lens[r] + 1, and each counter
# is turned into idx values as the runners do: start + counter - 1 for a seq range, or the
# counter's dims[r] values for a mat/matp range, written through that range's slots.
set_instr_idx <- function(instr, step) {
  if (!instr$nDim) {
    return(0) # as passed by calc_0
  }
  idx <- numeric(instr$nDim)
  k <- step - 1
  for (r in seq_along(instr$lens)) {
    counter <- (k %/% instr$strides[r]) %% instr$lens[r] + 1
    d <- instr$dims[r]
    slots <- instr$slots[instr$idxSlotStarts[r] + seq_len(d)]
    if (instr$index_types[r] == 1) {
      idx[slots] <- instr$values[[r]][1] + counter - 1
    } else {
      idx[slots] <- instr$values[[r]][(counter - 1) * d + seq_len(d)]
    }
  }
  idx
}

# Find which instruction of an instruction list holds list step `step` (1-based), and the step
# within that instruction. Instruction j holds list steps offsets[j] + 1 through offsets[j] + nNodes,
# so j is the last instruction with offsets[j] < step (which skips any instructions with no nodes).
instrList_find_singleton <- function(instrList, step) {
  if (step < 1 || step > instrList$nNodes_total) {
    stop("step ", step, " is outside the instruction list's ", instrList$nNodes_total, " steps")
  }
  offsets <- instrList$offsets
  j <- findInterval(step - 1, offsets)
  list(j = j, k = step - offsets[j])
}

# Fill `idx` for list step `step` (1-based) of an instruction list.
set_instr_idx_from_instrList <- function(instrList, step) {
  loc <- instrList_find_singleton(instrList, step)
  set_instr_idx(instrList$instr_nList[[loc$j]], loc$k)
}


instr_nClass <- nClass(
  classname = "instr_nClass",
  Rpublic = list(
    initialize = function(calcRange, instr, ...) {
      super$initialize(...)
      if (!missing(calcRange) || !missing(instr)) {
        if (!missing(calcRange)) {
          instr <- range2instr(calcRange)
        }
        self$lens <- instr$lens %||% integer()
        self$index_types <- instr$index_types %||% integer()
        self$nDim <- instr$nDim %||% 0L
        self$dims <- instr$dims %||% integer()
        self$slots <- instr$slots %||% integer()
        self$values <- nList(integerVector())$new()
        self$values$setLength(length(self$dims))
        if (self$nDim) {
          for (i in 1:length(self$dims)) {
            self$values[[i]] <- instr$values[[i]]
          }
        }
        self$instr_type <- instr$instr_type %||% 0L # Use integer for compilation (would char be ok?).
        self$sortID <- instr$sortID %||% integer()
        self$declID <- instr$declID %||% 0L
        self$nNodes <- instr$nNodes %||% 0L
        self$strides <- instr$strides %||% integer()
        self$idxSlotStarts <- instr$idxSlotStarts %||% integer()
      }
    }
  ),
  Cpublic = list(
    lens = "integerVector",
    index_types = "integerVector",
    nDim = "integerScalar",
    dims = "integerVector",
    slots = "integerVector",
    values = "nCompiler:::nList(integerVector())",
    instr_type = "integerScalar",
    sortID = "integerVector",
    declID = "integerScalar",
    nNodes = "integerScalar",
    strides = "integerVector",
    idxSlotStarts = "integerVector",
    instr_nClass = nFunction(
      function() {
        values <- nList(integerVector())$new()
      },
      compileInfo = list(constructor = TRUE)
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("instr_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = TRUE,
    exportName = "instr_nClass_new",
    needed_units = list("nCompiler:::nList(integerVector())"),
    packageNames = c(uncompiled = "instr_nClass_R", compiled = "instr_nClass")
  )
)

# An ordered list of instructions, with the offset of each instruction's first step
# within the whole list. Filled from an `RinstrList` (see `make_RinstrList`).
instrList_nClass <- nClass(
  classname = "instrList_nClass",
  Cpublic = list(
    instr_nList = "nList(instr_nClass)",
    offsets = "integerVector",
    nNodes_total = "integerScalar",
    instrList_nClass = nFunction(
      function() {
        instr_nList <- nList(instr_nClass)$new()
      },
      compileInfo = list(constructor = TRUE)
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("instrList_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = TRUE,
    exportName = "instrList_nClass_new",
    needed_units = list("instr_nClass", "nList(instr_nClass)"),
    packageNames = c(uncompiled = "instrList_nClass_R", compiled = "instrList_nClass")
  )
)
