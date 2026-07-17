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
  instr$type <- determineInstrType(instr)
  instr$sortID <- range$sortID
  instr$declID <- range$declID
  return(instr)
}

# Eventually think about reordering order of looping for efficiency (and take parallelization into account).
# For the moment, we determine mat vs. seq here and then in declFunClass calculate we will determine whether to
# vectorize based on whether possible based on the declaration.
# Open question of when to determine if to use parallel calculate.
determineInstrType <- function(instr, use_vec = FALSE) {
  type <- NULL
  if (!instr$nDim) {
    type <- "0"
  }
  if (length(instr$dims) == 1) {
    if (instr$index_types[1] == 1) {
      type <- "1_seq"
    } else {
      if (instr$dims[1] == 1) {
        type <- "1_mat"
      } else {
        if (identical(instr$slots, as.numeric(1:length(instr$slots)))) type <- "1_matp" else type <- "1_matp_ord"
      }
    }
  }
  if (length(instr$dims) == 2) {
    if (identical(instr$dims, c(1, 1))) {
      if (identical(instr$slots, c(2, 1))) {
        type <- "2_x_y_ord"
      } else {
        if (identical(instr$index_types, c(1, 1))) type <- "2_seq_seq"
        if (identical(instr$index_types, c(1, 2))) type <- "2_seq_mat"
        if (identical(instr$index_types, c(2, 1))) type <- "2_mat_seq"
        if (identical(instr$index_types, c(2, 2))) type <- "2_mat_mat"
      }
    } else {
      type <- "2_matp_matp"
      if (instr$index_types[1] == 1) type <- "2_seq_matp"
      if (instr$index_types[2] == 1) type <- "2_matp_seq"
    }
  }
  if (length(instr$dims) == 3) {
    if (all(instr$index_types == 1) && identical(instr$slots, as.numeric(1:length(instr$slots)))) {
      type <- "3_allseq"
    } else {
      type <- "3_generic"
    }
  }
  if (length(instr$dims) == 4) {
    if (all(instr$index_types == 1) && identical(instr$slots, as.numeric(1:length(instr$slots)))) {
      type <- "4_allseq"
    } else {
      type <- "4_generic"
    }
  }
  if (length(instr$dims) == 5) {
    if (all(instr$index_types == 1) && identical(instr$slots, as.numeric(1:length(instr$slots)))) {
      type <- "5_allseq"
    } else {
      type <- "5_generic"
    }
  }
  if (is.null(type)) stop("no available specific instruction type")
  return(type2itype[[type]])
}

# TODO: document this since it may be user-facing.
# We may want to work more on the interface/what inputs are allowed.
# This only omits data nodes if given chars or varRanges.
#' @export
makeInstrList <- function(model, input, includeData = TRUE, use_vec = FALSE) {
  # This works with:
  # (1) a char vector of "nodes"
  # (2) a list of (or single) varRanges
  # (3) an nList of (or single) instr_nClass objects (assumed to be in sort order)
  # (4) an R list of instr_nClass objects (not assumed to be in sort order)
  # (5) an R list of instr_nClass-like R lists (produced by makeScalarInstrInfoLists when splitting a calcRange with multiple sortID values.
    
  # TODO: do we really need to handle case #4? 
  
  # A single instruction.
  if (inherits(input, "instr_nClass")) {
    return(list(input))
  }
  # An nList of instructions.
  if (inherits(input, "nList")) {
    if (!inherits(input[[1]], "instr_nClass")) {
      stop("nList input to `makeInstrList` should contain `instr_nClass` objects")
    } else {
      return(input)
    }
  }
  # An R list of instructions.
  if (is.list(input) && all(sapply(input, function(x) inherits(x, "instr_nClass")))) {
    # Create sort-ordered nList.
    instrList <- nList(instr_nClass)$new()
    numInstrs <- length(input)
    instrList$setLength(numInstrs)
    sortIDs <- lapply(input, \(x) x$sortID)
    sortIDranges <- sapply(sortIDs, \(x) range(x, na.rm = TRUE))
    multiSortID <- which(sortIDranges[1,] != sortIDranges[2,])
    for(i in multiSortID) 
      if(!all(diff(sortIDs[[i]]) == 1, na.rm = TRUE)) {
        stop("multiple sortID values found for the ", i, "th instruction. Only sequential backward dependence is allowed when providing a list of instructions")
      } else {  # Check for any overlapping sortID values for the sequential backward dependence calcRange.
        if(any(sortIDranges[2,-i] > sortIDranges[1,i] & sortIDranges[1,-i] < sortIDranges[2,i]))
          stop("the multiple sortID values in the ", i, "th instruction overlap with sortID values in other instructions")                
      }
    ord <- order(sortIDranges[1,])
    # We need a loop to populate an nList; can't use `input[ord]`.
    for (i in 1:numInstrs) {
      instrList[[i]] <- input[[ord[i]]]
    }
    return(instrList)
  }

  # An R list of instr_nClass-like R lists
  if(inherits(input, "Rlist_Rinstr"))
    return(input)

  # Finally handle character vectors or varRanges.
  if (inherits(input, "varRangeClass")) input <- list(input)

  if (!includeData) {
    input <- model$getNodes(input, includeData = FALSE)
    if(!length(input)) return(NULL)
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
  multiSortID <- which(sortIDranges[1,] != sortIDranges[2,])

  newInstrs <- list()
  rangesToRemove <- numeric(0)
  for(i in multiSortID) 
    if(!all(diff(sortIDs[[i]]) == 1, na.rm = TRUE)) {
      # This quickly creates a list of R lists, where the elements mimic instr_nClass objects,
      # from a calcRange with multiple sortID values. Creating many calcRanges or instr_nClass objects is slow.
      newInstrs <- c(newInstrs, ranges[[i]]$makeScalarInstrInfoLists())
      rangesToRemove <- c(rangesToRemove, i)
    } else {  # Check for any overlapping sortID values for the sequential backward dependence calcRange.
      if(any(sortIDranges[2,-i] > sortIDranges[1,i] & sortIDranges[1,-i] < sortIDranges[2,i]))
        stop("the multiple sortID values in the ", i, "th calcRange overlap with sortID values in other calcRanges")                
    }
  if(length(rangesToRemove))
    ranges <- ranges[-rangesToRemove]

  # Again, returning a list of R lists that mimic instr_nClass objects is much faster
  # than instantiating instr_nClass objects.
  Rlist <- c(newInstrs, lapply(ranges, \(x) range2instr(x)))
  sortIDs <- sapply(Rlist, \(x) x$sortID)
  Rlist <- Rlist[order(sortIDs)]
  class(Rlist) <- "Rlist_Rinstr"  # For checking idempotency.
  return(Rlist)
}


instr_nClass <- nClass(
  classname = "instr_nClass",
  Rpublic = list(
    initialize = function(calcRange, ...) {
      super$initialize(...)
      if(!missing(calcRange)) {
        instr <- range2instr(calcRange) 
        self$lens <- instr$lens %||% integer()
        self$index_types <- instr$index_types %||% integer()
        self$nDim <- instr$nDim %||% 0L
        self$dims <- instr$dims %||% integer()
        self$slots <- instr$slots %||% integer()
        self$values <- nList(integerVector)$new()
        self$values$setLength(length(self$dims))
        if (self$nDim) {
          for (i in 1:length(self$dims)) {
            self$values[[i]] <- instr$values[[i]]
          }
        }
        self$type <- instr$type %||% 0L # Use integer for compilation (would char be ok?).
        self$sortID <- instr$sortID %||% integer()
        self$declID <- instr$declID %||% 0L
      }
    }
  ),
  Cpublic = list(
    lens = "integerVector",
    index_types = "integerVector",
    nDim = "integerScalar",
    dims = "integerVector",
    slots = "integerVector",
    values = "nList(integerVector)",
    type = "integerScalar",
    sortID = "integerVector",
    declID = "integerScalar",
    instr_nClass = nFunction(
      function() {
        values <- nList(integerVector)$new()
      },
      compileInfo = list(constructor = TRUE)
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("instr_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = TRUE,
    exportName = "instr_nClass_new",
    needed_units = list("nList(integerVector)"),
    packageNames = c(uncompiled = "instr_nClass_R", compiled = "instr_nClass")
  )
)
