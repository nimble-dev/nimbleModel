## This needs to be sourced after `nimbleFunction` is defined, so can't be done in `distributions_inputList.R`.
calc_dmnormAltParams <- nimble::nimbleFunction(
    name = 'calc_dmnormAltParams',
    run = function(cholesky = double(2), prec_param = double(), return_prec = double()) {
        if(prec_param == return_prec) {
            ans <- t(cholesky) %*% cholesky
        } else {
            I <- diag(dim(cholesky)[1])
            ans <- backsolve(cholesky, forwardsolve(t(cholesky), I))
            ## Chris suggests:
            ## tmp <- forwardsolve(L, I)
            ## ans <- crossprod(tmp)
        }
        returnType(double(2))
        return(ans)
    }
)

calc_dwishAltParams <- nimble::nimbleFunction(
    name = 'calc_dwishAltParams',
    run = function(cholesky = double(2), scale_param = double(), return_scale = double()) {
        if(scale_param == return_scale) {
            ans <- t(cholesky) %*% cholesky
        } else {
            I <- diag(dim(cholesky)[1])
            ans <- backsolve(cholesky, forwardsolve(t(cholesky), I))
            ## Chris suggests:
            ## tmp <- forwardsolve(L, I)
            ## ans <- crossprod(tmp)
        }
        returnType(double(2))
        return(ans)
    }
)
