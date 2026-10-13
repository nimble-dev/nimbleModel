.onLoad <- function(libname, pkgname) {
  # ns <- asNamespace(pkgname)
  nCompiler::registerOpDef(nimbleModel_nCompiler_opDefs)
}

.onUnload <- function(libpath) {
  nCompiler::deregisterOpDef(names(nimbleModel_nCompiler_opDefs))
}
