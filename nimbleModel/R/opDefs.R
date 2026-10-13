nimbleModel_error_LAT <- function(code, symTab, auxEnv, info) {
  # LATenv <- nCompiler:::labelAbstractTypesEnv
  stop(exprClassProcessingErrorMsg(
    code,
    'resize is not supported for the given argument type.'
  ), call. = FALSE)
  stop("resize is not supported for the given argument type.")
}

nimbleModel_nCompiler_opDefs <- list(
  resize = list(
    matchDef = function(obj, size) {},
    isGeneric = TRUE,
    labelAbstractTypes = list(
      handler = nimbleModel_error_LAT # this serves as a generic to catch errors. An overload will be provided for modelValues.
    )
  ),
  getsize = list(
    matchDef = function(obj) {},
    isGeneric = TRUE,
    labelAbstractTypes = list(
      handler = nimbleModel_error_LAT
    )
  )
)