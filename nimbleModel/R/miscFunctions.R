# This needs to be sourced after `nimbleFunction` is defined, so can't be done in `distributions_inputList.R`.
calc_dmnormAltParams <- nCompiler::nFunction(
  name = "calc_dmnormAltParams",
  fun = function(cholesky = double(2), prec_param = double(), return_prec = double()) {
    if (prec_param == return_prec) {
      ans <- t(cholesky) %*% cholesky
    } else {
      I <- diag(dim(cholesky)[1])
      ans <- backsolve(cholesky, forwardsolve(t(cholesky), I))
      # Chris suggests:
      # tmp <- forwardsolve(L, I)
      # ans <- crossprod(tmp)
    }
    returnType(double(2))
    return(ans)
  }
)

calc_dwishAltParams <- nCompiler::nFunction(
  name = "calc_dwishAltParams",
  fun = function(cholesky = double(2), scale_param = double(), return_scale = double()) {
    if (scale_param == return_scale) {
      ans <- t(cholesky) %*% cholesky
    } else {
      I <- diag(dim(cholesky)[1])
      ans <- backsolve(cholesky, forwardsolve(t(cholesky), I))
      # Chris suggests:
      # tmp <- forwardsolve(L, I)
      # ans <- crossprod(tmp)
    }
    returnType(double(2))
    return(ans)
  }
)
