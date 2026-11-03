# Classes and tools to support nimble2's values() and nimCopy(), both getting and setting.
#
# copier_nClass is just a data structure for one variable name and index information. It doesn't actually copy.
#
# multiCopier_nClass holds multiple copier_nClass's and does values() copying (for a model only).
# It has a C++ layer that uses the flatViewGroup class to do the copying.
#
# multiCopierMV_nClass is like multiCopier_nClass but for modelValues objects,
# but is used only for nimCopy.
#
# multiCopier_nClass and multiCopierMV_nClass share a C++ base class.
# The C++ step of nimCopy_ takes two multiCopier base classes and copies from one to the other.
#
# An important aspect on the C++ side is avoiding repeated name-based lookups of variables
# within the model or modelValues.


#' @export
copier_nClass <- nCompiler::nClass(
  classname = "copier_nClass",
  Cpublic = list(
    varName = "string",
    indsList = "nCompiler:::nList(integerVector())"
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("copier_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = TRUE,
    exportName = "copier_nClass_new",
    needed_units = list("nCompiler:::nList(integerVector())"),
    packageNames = c(uncompiled = "copier_nClass_R", compiled = "copier_nClass"),
    nClass_inherit = list("copier_nC_base"),
    depends = "nimbleModel",
    Hincludes = "<nimbleModel/predef/copier_nC_base/copier_nC_base.h>"
  )
)

#' @export
multiCopier_nClass <- nCompiler::nClass(
  classname = "multiCopier_nClass",
  Rpublic = list(
    initialize = function(...) {
      super$initialize(...)
      self$copiers <- nList(copier_nClass())$new()
    }
  ),
  Cpublic = list(
    copiers = "nCompiler:::nList(copier_nClass())",
    init = nFunction(
      name = "init",
      function(model) {
        cat("need uncompiled multiCopier init() implementation.")
      },
      compileInfo = list(
        C_fun = function(model = "nimbleModel:::modelBase_nClass()") {
          cppLiteral("multiCopier_model_nC_base::init(this->copiers, model)")
        }
      )
    ),
    getValues = nFunction(
      name = "getValues",
      function() {
        cat("need uncompiled multiCopier getValues() implementation.")
      },
      returnType = "numericVector",
      compileInfo = list(
        C_fun = function() {
          cppLiteral("return getValues_()") # refreshes data pointers; see multiCopier_model_nC_base
        }
      )
    ),
    # Note that this setValues works but is not actually used by nimble2 keyword processing
    # which instead uses copiers->setValues_() which provides an operator=.
    # It is possible that this setValues() will be useful in other contexts, or if not that we
    # can eventually remove it to avoid confusion.
    setValues = nFunction(
      name = "setValues",
      function(v = "numericVector") {
        cat("need uncompiled multiCopier setValues() implementation.")
      },
      compileInfo = list(
        C_fun = function(v = "numericVector") {
          cppLiteral("setValues_() = v") # refreshes data pointers; see multiCopier_model_nC_base
        }
      )
    ),
    # Get or set one entry (mode = "raw") or one flattened element (mode = "scalar"); i is 1-based.
    # These choose the mode at run time, for use from R. nimble2 keyword processing instead
    # calls getValues_raw_(i) etc. in multiCopier_model_nC_base directly, since its mode is fixed.
    getValues_indexed = nFunction(
      name = "getValues_indexed",
      function(i = "integerScalar", mode = "string") {
        cat("need uncompiled multiCopier getValues_indexed() implementation.")
      },
      returnType = "numericVector",
      compileInfo = list(
        C_fun = function(i = "integerScalar", mode = "string") {
          cppLiteral('if (mode == "raw") return getValues_raw_(i);
if (mode == "scalar") return getValues_scalar_(i);
Rcpp::stop("getValues_indexed: mode must be \\"raw\\" or \\"scalar\\".");')
        }
      )
    ),
    setValues_indexed = nFunction(
      name = "setValues_indexed",
      function(i = "integerScalar", v = "numericVector", mode = "string") {
        cat("need uncompiled multiCopier setValues_indexed() implementation.")
      },
      compileInfo = list(
        C_fun = function(i = "integerScalar", v = "numericVector", mode = "string") {
          cppLiteral('if (mode == "raw") {setValues_raw_(i) = v; return;}
if (mode == "scalar") {setValues_scalar_(i) = v; return;}
Rcpp::stop("setValues_indexed: mode must be \\"raw\\" or \\"scalar\\".");')
        }
      )
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("mCopy_nC")),
  compileInfo = list(
    cpp_classname = "mCopy_nC",
    interface = "full",
    createFromR = TRUE,
    exportName = "multiCopier_nClass_new",
    needed_units = list("nCompiler:::nList(copier_nClass())", "modelBase_nClass()"),
    packageNames = c(uncompiled = "multiCopier_nClass_R", compiled = "multiCopier_nClass"),
    nClass_inherit = list("multiCopier_model_nC_base"),
    depends = "nimbleModel",
    Hincludes = "<nimbleModel/predef/mCopy_nC_base/mCopy_model_nC_base.h>",
    opDefs = list(
      getOrSetValues = list(
        labelAbstractTypes = list(
          handler = "multiCopier_getOrSetValues_LAT"
        )
      )
    )
  )
)

multiCopier_getOrSetValues_LAT <- function(...) {
  browser()
}

#' @export
multiCopierMV_nClass <- nCompiler::nClass(
  classname = "multiCopierMV_nClass",
  Rpublic = list(
    initialize = function(...) {
      super$initialize(...)
      self$copiers <- nList(copier_nClass())$new()
    }
  ),
  Cpublic = list(
    copiers = "nCompiler:::nList(copier_nClass())",
    init = nFunction(
      name = "init",
      function(modelValues) {
        cat("need uncompiled multiCopierMV init() implementation.")
      },
      compileInfo = list(
        C_fun = function(modelValues = "nimbleModel:::modelValuesBase_nClass()") {
          cppLiteral("multiCopier_modelValues_nC_base::init(this->copiers, modelValues)")
        }
      )
    ),
    # Selects which row of each variable's nList the flatViewGroup reads
    # from/writes to. Must be called at least once, after the modelValues has
    # been resized to have at least one row, before getValues/setValues/
    # nimCopy_ are used. See multiCopier_modelValues_nC_base::setActiveRow.
    setActiveRow = nFunction(
      name = "setActiveRow",
      function(row) {
        cat("need uncompiled multiCopierMV setActiveRow() implementation.")
      },
      compileInfo = list(
        C_fun = function(row) {
          cppLiteral("multiCopier_modelValues_nC_base::setActiveRow(row)")
        }
      ),
      argTypes = list(row = "integerScalar")
    ),
    getValues = nFunction(
      name = "getValues",
      function() {
        cat("need uncompiled multiCopierMV getValues() implementation.")
      },
      returnType = "numericVector",
      compileInfo = list(
        C_fun = function() {
          cppLiteral("return flatViewGroup.copyIntoVector()")
        }
      )
    ),
    # See the note on multiCopier_nClass$setValues above: kept for other
    # contexts even though nimble2 keyword processing uses
    # copiers->flatViewGroup.setValues_() directly.
    setValues = nFunction(
      name = "setValues",
      function(v = "numericVector") {
        cat("need uncompiled multiCopierMV setValues() implementation.")
      },
      compileInfo = list(
        C_fun = function(v = "numericVector") {
          cppLiteral("flatViewGroup.copyFromVector(v)")
        }
      )
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("mCopyMV_nC")),
  compileInfo = list(
    cpp_classaname = "mCopyMV_nC",
    interface = "full",
    createFromR = TRUE,
    exportName = "multiCopierMV_nClass_new",
    needed_units = list("nCompiler:::nList(copier_nClass())", "modelValuesBase_nClass()"),
    packageNames = c(uncompiled = "multiCopierMV_nClass_R", compiled = "multiCopierMV_nClass"),
    nClass_inherit = list("multiCopier_modelValues_nC_base"),
    depends = "nimbleModel",
    Hincludes = "<nimbleModel/predef/mCopy_nC_base/mCopy_MV_nC_base.h>"
  )
)

#' @export
makeMultiCopier <- function(source, nodes,
                            sourceType = c("model", "modelValues"), ...) {
  # This will be called from a nimble2 keyword processor
  # The ... is to absorb further arguments that at the time of this writing are not fleshed out.
  if (is.character(nodes)) {
    nodes <- nodes |> lapply(\(x) nimbleModel:::varRangeClass$new(x))
  }
  sourceType <- match.arg(sourceType)
  multiCopier <-
    switch(sourceType,
      model = multiCopier_nClass$new(),
      modelValues = multiCopierMV_nClass$new()
    )
  # A single index i is the block i:i, which flattens identically.
  getRange <- function(indexRange) {
    if (inherits(indexRange, "indexRangeSequenceClass")) {
      return(c(indexRange$start, indexRange$end))
    }
    if (inherits(indexRange, "indexRangeScalarClass")) {
      return(c(indexRange$value, indexRange$value))
    }
    stop("In a copy operation, only contiguous index blocks are supported")
  }
  for (i in seq_along(nodes)) {
    thisNode <- nodes[[i]]
    multiCopier$copiers[[i]] <- copier_nClass$new()
    multiCopier$copiers[[i]]$varName <- thisNode$varName
    multiCopier$copiers[[i]]$indsList <- thisNode$indexRanges |> lapply(getRange)
  }
  attr(multiCopier, "NCgenerator") <-
    switch(sourceType,
      model = quote(nimbleModel:::multiCopier_nClass),
      modelValues = quote(nimbleModel:::multiCopierMV_nClass)
    )
  multiCopier
}

# Uncompiled implementations of values(model, nodes) and `values<-`(model, nodes, value).

# With mode = "scalar" (or eventually "node"), `nodes` must be indexed, as in
# values(model, nodes[i], mode = "scalar"). Then `nodes` and `i` are evaluated separately
# in the caller and passed to values_indexed(). With mode = "raw" (the default, as in nimble),
# nodes[i] is evaluated as usual, giving entry i of `nodes`.

#' @export
values <- function(model, nodes, mode = "raw") {
  if (!identical(mode, "raw")) {
    base_and_index <- split_indexed_nodes(substitute(nodes), parent.frame(), mode)
    return(values_indexed(model, base_and_index$nodes, base_and_index$i, mode))
  }
  if (is.character(nodes)) {
    nodes <- lapply(nodes, \(x) varRangeClass$new(x))
  }
  # To-do: other checking and error-trapping on inputs
  vals <- lapply(
    nodes,
    \(x) {
      eval(x$toExpr(), envir = model) |> as.numeric()
    }
  )
  do.call("c", vals)
}

#' @export
`values<-` <- function(model, nodes, mode = "raw", value) {
  if (!identical(mode, "raw")) {
    base_and_index <- split_indexed_nodes(substitute(nodes), parent.frame(), mode)
    values_indexed(model, base_and_index$nodes, base_and_index$i, mode) <- value
    return(model)
  }
  if (is.character(nodes)) {
    nodes <- lapply(nodes, \(x) varRangeClass$new(x))
  }
  # First find each target expression and length, so a wrong-length value
  # is an error before anything is assigned.
  targetExprs <- vector("list", length(nodes))
  targetLengths <- numeric(length(nodes))
  for (i in seq_along(nodes)) {
    thisNode <- nodes[[i]]
    expr <- thisNode$toExpr()
    if (is.name(expr)) {
      thisLength <- length(model[[as.character(expr)]])
      model_stuff <- quote(model$VAR)
      model_stuff[[3]] <- expr
      expr <- model_stuff
    } else {
      mm <- thisNode$getMinMax()
      sizes <- mm[, 2] - mm[, 1] + 1
      ## This section about NAs in the sizes
      ## anticipates how we would handle blank index slots
      ## by putting NAs in the getMinMax() result.
      ## But that is not in place while this is being drafted
      NAsizes <- which(is.na(sizes))
      if (length(NAsizes)) {
        thisDims <- dimOrLength(model[[thisNode$varName]])
        sizes[NAsizes] <- thisDims[NAsizes]
      }
      #
      thisLength <- prod(sizes)
      varExpr <- expr[[2]]
      model_stuff <- quote(model$VAR)
      model_stuff[[3]] <- varExpr
      expr[[2]] <- model_stuff
    }
    targetExprs[[i]] <- expr
    targetLengths[i] <- thisLength
  }
  if (length(value) != sum(targetLengths)) {
    stop("values(model, nodes) <- value: value has length ", length(value),
         " but nodes have total length ", sum(targetLengths), ".")
  }
  iV_start <- 1
  replExprTemplate <- quote(model_stuff <- value[iV_start:iV_end])
  for (i in seq_along(nodes)) {
    iV_end <- iV_start + targetLengths[i] - 1
    replExpr <- replExprTemplate
    replExpr[[2]] <- targetExprs[[i]]
    eval(replExpr)
    iV_start <- iV_end + 1
  }
  model
}

# From the unevaluated `nodes` argument of values(model, nodes[i], mode = ...),
# evaluate `nodes` and `i` separately in `env`.
split_indexed_nodes <- function(nodesExpr, env, mode) {
  if (!(is.call(nodesExpr) && identical(nodesExpr[[1]], as.name("[")) && length(nodesExpr) == 3)) {
    stop("values: mode = \"", mode, "\" requires a singly indexed `nodes` argument, as in nodes[i].")
  }
  list(nodes = eval(nodesExpr[[2]], env), i = eval(nodesExpr[[3]], env))
}

as_varRange_list <- function(nodes) {
  if (inherits(nodes, "varRangeClass")) {
    return(list(nodes))
  }
  if (is.character(nodes)) {
    return(lapply(nodes, \(x) varRangeClass$new(x)))
  }
  nodes
}

check_values_index <- function(i, n, what) {
  if (!(is.numeric(i) && length(i) == 1 && !is.na(i) && i >= 1 && i <= n && i == round(i))) {
    stop("values: index ", deparse(i), " is out of range for ", n, " ", what, ".")
  }
}

# Uncompiled values(model, nodes[i], mode = mode) and its assignment form.
# mode = "raw": entry i of `nodes`.
# mode = "scalar": element i of values(model, nodes), as a length-1 vector.
# mode = "node" is not yet supported.

#' @export
values_indexed <- function(model, nodes, i, mode = c("raw", "scalar", "node")) {
  mode <- match.arg(mode)
  nodes <- as_varRange_list(nodes)
  switch(mode,
    raw = {
      check_values_index(i, length(nodes), "entries of `nodes`")
      values(model, nodes[i])
    },
    scalar = {
      vals <- values(model, nodes)
      check_values_index(i, length(vals), "elements of values(model, nodes)")
      vals[i]
    },
    node = stop("values: mode = \"node\" is not supported yet.")
  )
}

#' @export
`values_indexed<-` <- function(model, nodes, i, mode = c("raw", "scalar", "node"), value) {
  mode <- match.arg(mode)
  nodes <- as_varRange_list(nodes)
  switch(mode,
    raw = {
      check_values_index(i, length(nodes), "entries of `nodes`")
      n <- length(values(model, nodes[i]))
      if (length(value) != n) {
        stop("values: entry ", i, " of `nodes` has ", n, " elements, but ", length(value), " values were given.")
      }
      values(model, nodes[i]) <- value
    },
    scalar = {
      if (length(value) != 1) {
        stop("values: mode = \"scalar\" requires a single value, but ", length(value), " were given.")
      }
      lens <- vapply(nodes, \(x) length(values(model, list(x))), numeric(1))
      check_values_index(i, sum(lens), "elements of values(model, nodes)")
      k <- findInterval(i - 1, cumsum(c(0, lens)), left.open = FALSE, rightmost.closed = FALSE)
      local_i <- i - sum(lens[seq_len(k - 1)])
      vals <- values(model, nodes[k])
      vals[local_i] <- value
      values(model, nodes[k]) <- vals
    },
    node = stop("values: mode = \"node\" is not supported yet.")
  )
  model
}
