# nimCopy: copy values between models and modelValues objects.
#
# As in nimble:
# - "nodes" are really arbitrary indexed variable ranges and are copied as given, not expanded to nodes.
#   If nodesTo is missing, it defaults to nodes.
# - logProb = TRUE also copies the logProb elements of the stochastic nodes in nodes
#   (including nodes only partly in nodes). logProbOnly = TRUE copies only those.
# - row is the row of `from` if it is a modelValues object. rowTo is the row of `to`
#   if it is a modelValues object, defaulting to row.
#
# A difference from nimble:
# Finding the logProb elements of nodes needs a model, so with logProb or logProbOnly,
# and nodes given, one of from and to must be a model.
# This might necessitate enhancing modelValues to be know about logProb nodes.
#
# In the future, there are several efficiency issues that could be revisited:
#. - Instead of doing a full get for the "to" nodes just to get lengths, calculate these from indices or objects.
#. - Check on when assignment ends up creating whole object copies or not.
#.   This may be particularly the case when uncompiled code works with compiled objects.
#.  - Consider get-and-set for each, rather than get-all and set-all, but preliminary investigation 
#.   suggested that overhead of large allocation is minor compared to benefits of the latter.
#
#' Copy values between models and modelValues objects
#'
#' @param from a model or modelValues object to copy from.
#' @param to a model or modelValues object to copy to.
#' @param nodes variables or indexed ranges of variables to copy \code{from}, as a character vector or list
#'   of varRanges. If missing or NULL, all variables are copied.
#' @param nodesTo variables or indexed ranges of variables to copy \code{to} If missing or NULL,
#'   the same as \code{nodes}.
#' @param row the row of \code{from}, if it is a modelValues object.
#' @param rowTo the row of \code{to}, if it is a modelValues object. Defaults to \code{row}.
#' @param logProb whether to also copy the logProb elements of the stochastic nodes in
#'   \code{nodes} (or all logProb variables if \code{nodes} is missing). One can also manually include
#'  logProb elements in \code{nodes} or \code{nodesTo}. See details.
#' @param logProbOnly whether to copy only the logProb elements (or variables).
#'
#' @return NULL, invisibly.
#' @export
nimCopy <- function(from, to, nodes = NULL, nodesTo = NULL, row = NA, rowTo = NA,
                    logProb = FALSE, logProbOnly = FALSE) {
  is_fromMV <- inherits(from, "modelValues")
  is_toMV <- inherits(to, "modelValues")
  if (is_fromMV && is.na(row)) stop("nimCopy: row is needed to copy from a modelValues object.")
  if (is_toMV && is.na(rowTo)) {
    if (is.na(row)) stop("nimCopy: row or rowTo is needed to copy to a modelValues object.")
    rowTo <- row
  }
  model <- if (!is_fromMV) from else if (!is_toMV) to else NULL
  if (is.null(nodes)) {
    fromNodes <- nimCopy_prepareNodes(NULL, nimCopy_allVars(from, logProb, logProbOnly))
  } else {
    fromNodes <- nimCopy_prepareNodes(model, nodes, logProb, logProbOnly)
  }
  toNodes <- if (is.null(nodesTo)) fromNodes else nimCopy_prepareNodes(model, nodesTo, logProb, logProbOnly)

  vals <- lapply(fromNodes, \(vr) as.numeric(nimCopy_get(from, is_fromMV, vr, row)))
  vals <- unlist(vals, use.names = FALSE)
  toLengths <- vapply(toNodes, \(vr) length(nimCopy_get(to, is_toMV, vr, rowTo)), numeric(1))
  if (sum(toLengths) != length(vals)) {
    stop("nimCopy: from has ", length(vals), " values to copy, but to has ", sum(toLengths), ".")
  }
  offset <- 0
  for (i in seq_along(toNodes)) {
    nimCopy_set(to, is_toMV, toNodes[[i]], rowTo, vals[offset + seq_len(toLengths[i])])
    offset <- offset + toLengths[i]
  }
  invisible(NULL)
}

# The values of varRange vr in a model, or in row `row` of a modelValues object.
nimCopy_get <- function(source, isMV, vr, row) {
  var <- vr$varName
  value <- if (isMV) source[var, row] else source[[var]]
  eval(vr$toExpr(), envir = structure(list(value), names = var), enclos = baseenv())
}

# Set the values of varRange vr from v, in flattened (column-major) order.
nimCopy_set <- function(target, isMV, vr, row, v) {
  var <- vr$varName
  expr <- vr$toExpr()
  if (is.name(expr)) {
    # A whole variable: keep its dimensions.
    value <- if (isMV) target[var, row] else target[[var]]
    value[] <- v
  } else {
    env <- new.env(parent = baseenv())
    env[[var]] <- if (isMV) target[var, row] else target[[var]]
    env$LocalV_ <- v
    eval(substitute(E <- LocalV_, list(E = expr)), env)
    value <- env[[var]]
  }
  if (isMV) target[var, row] <- value else target[[var]] <- value
  invisible(NULL)
}

# All variables of a model or modelValues object, for nimCopy with nodes missing.
nimCopy_allVars <- function(source, logProb = FALSE, logProbOnly = FALSE) {
  vars <- if (inherits(source, "modelValues")) {
    modelValues_varNames(source)
  } else {
    source$getVarNames(includeLogProb = TRUE)
  }
  isLP <- startsWith(vars, "logProb_")
  if (logProbOnly) vars[isLP] else if (logProb) vars else vars[!isLP]
}

# Make nodes into a list of varRanges, and add the logProb elements of their stochastic nodes
# (logProb = TRUE) or use only those (logProbOnly = TRUE). Elements of nodes that are already
# logProb variables are kept with logProb = TRUE and dropped with logProbOnly = TRUE, as in nimble.
nimCopy_prepareNodes <- function(model, nodes, logProb = FALSE, logProbOnly = FALSE) {
  if (inherits(nodes, "varRangeClass")) nodes <- list(nodes)
  if (is.character(nodes)) nodes <- lapply(nodes, \(x) varRangeClass$new(x))
  if (!(logProb || logProbOnly)) {
    return(nodes)
  }
  if (is.null(model)) {
    stop(
      "nimCopy: with logProb = TRUE or logProbOnly = TRUE, from or to must be a model, ",
      "to find the logProb elements of the nodes."
    )
  }
  isLP <- vapply(nodes, \(vr) startsWith(vr$varName, "logProb_"), logical(1))
  lpNodes <- list()
  if (any(!isLP)) {
    lpNodes <- lapply(logProbNodes(model, nodes[!isLP]), \(x) varRangeClass$new(x))
  }
  if (logProbOnly) lpNodes else c(nodes, lpNodes)
}

# The logProb elements of the stochastic nodes in, or partly in, nodes, as character strings
# like "logProb_x[2:3, 1]". As in genLogProbExpr, node x[i, 1:2] has logProb element
# logProb_x[i, 1]: node-internal ranges collapse to their start. So a block of nodes
# usually gives a block of logProb elements. Other cases give one element per node.
logProbNodes <- function(model, nodes) {
  # getNodes takes character node names more generally than varRanges (e.g. a whole variable).
  if (!is.character(nodes)) nodes <- vapply(nodes, \(vr) safeDeparse(vr$toExpr()), character(1))
  nodeRanges <- model$getNodes(nodes, stochOnly = TRUE)
  ans <- lapply(nodeRanges, \(nr) {
    block <- logProb_block(nr)
    if (is.null(block)) logProb_per_node(model, nr) else block
  })
  unlist(ans, use.names = FALSE)
}

# The logProb block for a nodeRange whose ranges are each a sequence (or scalar) in one
# index slot, or NULL otherwise. Internal ranges have constant starts in this case,
# since a range that depends on the loop indices is not a sequence range.
logProb_block <- function(nr) {
  lpName <- makeLogProbName(nr$varName)
  ranges <- nr$indexRanges
  if (!length(ranges)) {
    return(lpName)
  }
  isSimple <- vapply(ranges, \(r) inherits(r, c("indexRangeSequenceClass", "indexRangeScalarClass")), logical(1))
  if (!all(isSimple) || !all(lengths(nr$rangeToIndexSlot) == 1)) {
    return(NULL)
  }
  inds <- character(length(ranges))
  for (r in seq_along(ranges)) {
    ir <- ranges[[r]]
    if (inherits(ir, "indexRangeScalarClass")) {
      ind <- ir$value
    } else if (nr$boolExternalIndexRanges[r] && ir$end > ir$start) {
      ind <- paste0(ir$start, ":", ir$end)
    } else {
      ind <- ir$start
    }
    inds[nr$rangeToIndexSlot[[r]]] <- ind
  }
  paste0(lpName, "[", paste(inds, collapse = ", "), "]")
}

# The logProb element of each node in a nodeRange, from the declarations' logProb expressions.
logProb_per_node <- function(model, nr) {
  instrList <- makeInstrList(model, list(nr))
  if (is.null(instrList)) {
    return(NULL)
  }
  declInfo <- model$modelDef$declInfo
  ans <- lapply(instrList$instr_nList, \(instr) {
    di <- declInfo[[instr$declID]]
    lpExpr <- di$genLogProbExpr()
    if (is.null(lpExpr)) {
      return(NULL)
    } # not stochastic
    if (is.name(lpExpr)) {
      return(as.character(lpExpr))
    }
    indexVarNames <- di$declRule$loopIndexingRule$indexVarNames
    indexExprs <- as.list(lpExpr)[-(1:2)]
    vapply(seq_len(instr$nNodes), \(k) {
      idxEnv <- list2env(as.list(structure(set_instr_idx(instr, k), names = indexVarNames)), parent = baseenv())
      inds <- vapply(indexExprs, \(e) eval(e, idxEnv), numeric(1))
      paste0(as.character(lpExpr[[2]]), "[", paste(inds, collapse = ", "), "]")
    }, character(1), USE.NAMES = FALSE)
  })
  unlist(ans, use.names = FALSE)
}
