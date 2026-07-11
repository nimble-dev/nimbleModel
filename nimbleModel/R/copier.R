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
      function( model ) {
        cat("need uncompiled multiCopier init() implementation.")
      },
      compileInfo = list(
        C_fun = function(model = "nimbleModel:::modelBase_nClass()"){ 
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
        C_fun = function(){ 
          cppLiteral("return flatViewGroup.copyIntoVector()")
        }
      )
    ),
    setValues = nFunction(
      name = "setValues",
      function(v = "numericVector") {
        cat("need uncompiled multiCopier setValues() implementation.")
      },
      compileInfo = list(
        C_fun = function(v = "numericVector"){ 
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
    nClass_inherit = list("multiCopier_nC_base")
  )
)

#' @export
makeMultiCopier <- function(model, nodes, ...) {
  # This will be called from a nimble2 keyword processor
  # The ... is to absorb further arguments that at the time of this writing are not fleshed out.
  if(is.character(nodes)){
    nodes <- nodes |> lapply(\(x) nimbleModel:::varRangeClass$new(x))
  }
  multiCopier <- multiCopier_nClass$new()
  getRange <- function(indexRange) {
    if(!inherits(indexRange, "indexRangeSequenceClass"))
      stop("In a copy operation, only contiguous index blocks are supported")
    c(indexRange$start, indexRange$end)
  }
  for(i in seq_along(nodes)) {
    thisNode <- nodes[[i]]
    multiCopier$copiers[[i]] <- copier_nClass$new()
    multiCopier$copiers[[i]]$varName <- thisNode$varName
    multiCopier$copiers[[i]]$indsList <- thisNode$indexRanges |> lapply(getRange)
  }
  multiCopier |> structure(NCgenerator = quote(nimbleModel:::multiCopier_nClass))
}
