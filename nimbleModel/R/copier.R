#' @export
copier_nClass <- nCompiler::nClass(
  classname = "copier_nClass",
  Cpublic = list(
    varName = "string",
    indsList = "nList(integerVector())"
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("copier_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = TRUE,
    exportName = "copier_nClass_new",
    needed_units = list("nList(integerVector())"),
    packageNames = c(uncompiled = "copier_nClass_R", compiled = "copier_nClass"),
    nClass_inherit = list("copier_nC_base")
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
    copiers = "nList(copier_nClass())",
    init = nFunction(
      name = "init",
      function(model) {
        cat("need uncompiled multiCopier init() implementation.")
      },
      compileInfo = list(
        C_fun = function(model = "nimbleModel:::modelBase_nClass()") {
          cppLiteral("multiCopier_nC_base::init(this->copiers, model)")
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
          cppLiteral("return flatViewGroup.copyIntoVector()")
        }
      )
    ),
    # Note that this setValues works but it not actually used by nimble2 keyword processing
    # which instead uses copiers->flatViewGroup.setValues_() which provides an operator=.
    # It is possible that this setValues() will be useful in other contexts, or if not that we
    # can eventually remove it to avoid confusion.
    setValues = nFunction(
      name = "setValues",
      function(v = "numericVector") {
        cat("need uncompiled multiCopier setValues() implementation.")
      },
      compileInfo = list(
        C_fun = function(v = "numericVector") {
          cppLiteral("flatViewGroup.copyFromVector(v)")
        }
      )
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |> file.path("multiCopier_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = TRUE,
    exportName = "multiCopier_nClass_new",
    needed_units = list("nList(copier_nClass())", "modelBase_nClass()"),
    packageNames = c(uncompiled = "multiCopier_nClass_R", compiled = "multiCopier_nClass"),
    nClass_inherit = list("multiCopier_nC_base"),
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
makeMultiCopier <- function(model, nodes, ...) {
  # This will be called from a nimble2 keyword processor
  # The ... is to absorb further arguments that at the time of this writing are not fleshed out.
  if (is.character(nodes)) {
    nodes <- nodes |> lapply(\(x) nimbleModel:::varRangeClass$new(x))
  }
  multiCopier <- multiCopier_nClass$new()
  getRange <- function(indexRange) {
    if (!inherits(indexRange, "indexRangeSequenceClass")) {
      stop("In a copy operation, only contiguous index blocks are supported")
    }
    c(indexRange$start, indexRange$end)
  }
  for (i in seq_along(nodes)) {
    thisNode <- nodes[[i]]
    multiCopier$copiers[[i]] <- copier_nClass$new()
    multiCopier$copiers[[i]]$varName <- thisNode$varName
    multiCopier$copiers[[i]]$indsList <- thisNode$indexRanges |> lapply(getRange)
  }
  multiCopier |> structure(NCgenerator = quote(nimbleModel:::multiCopier_nClass))
}

#' @export
values <- function(model, nodes) {
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
`values<-` <- function(model, nodes, value) {
  if (is.character(nodes)) {
    nodes <- lapply(nodes, \(x) varRangeClass$new(x))
  }
  iV_start <- 1
  replExprTemplate <- quote(model_stuff <- value[iV_start:iV_end])
  for (i in seq_along(nodes)) {
    thisNode <- nodes[[i]]
    expr <- thisNode$toExpr()
    if (is.name(expr)) {
      thisLength <- length(model[[expr]])
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
    iV_end <- iV_start + thisLength - 1
    replExpr <- replExprTemplate
    replExpr[[2]] <- expr
    eval(replExpr)
    iV_start <- iV_end + 1
  }
  model
}
