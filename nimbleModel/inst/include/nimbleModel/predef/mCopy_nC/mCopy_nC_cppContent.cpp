/* OPENER (Do not edit this comment) */
#ifndef __mCopy_nC_CPP
#define __mCopy_nC_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "mCopy_nC_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
// [[Rcpp::depends(nimbleModel)]]

    void  mCopy_nC::init ( std::shared_ptr<modelBase_nClass> model )  {
RESET_EIGEN_ERRORS
multiCopier_model_nC_base::init(this->copiers, model);
}
    Eigen::Tensor<double, 1>  mCopy_nC::getValues (  )  {
RESET_EIGEN_ERRORS
return getValues_();
}
    void  mCopy_nC::setValues ( Eigen::Tensor<double, 1> v )  {
RESET_EIGEN_ERRORS
setValues_() = v;
}
    Eigen::Tensor<double, 1>  mCopy_nC::getValues_indexed ( int i, std::string mode )  {
RESET_EIGEN_ERRORS
if (mode == "raw") return getValues_raw_(i);
if (mode == "scalar") return getValues_scalar_(i);
Rcpp::stop("getValues_indexed: mode must be \"raw\" or \"scalar\".");;
}
    void  mCopy_nC::setValues_indexed ( int i, Eigen::Tensor<double, 1> v, std::string mode )  {
RESET_EIGEN_ERRORS
if (mode == "raw") {setValues_raw_(i) = v; return;}
if (mode == "scalar") {setValues_scalar_(i) = v; return;}
Rcpp::stop("setValues_indexed: mode must be \"raw\" or \"scalar\".");;
}
      mCopy_nC::mCopy_nC (  )  {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "multiCopier_nClass_new")]]
    SEXP  new_mCopy_nC (  )  {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(mCopy_nC);;
}

// [[Rcpp::export(name = "set_CnClass_env_multiCopier_nClass_new")]]
    void  set_CnClass_env_mCopy_nC ( SEXP env )  {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(mCopy_nC, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_multiCopier_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_mCopy_nC (  )  {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(mCopy_nC);;
}

NCOMPILER_INTERFACE(
mCopy_nC,
NCOMPILER_FIELDS(
field("copiers", &mCopy_nC::copiers)
),
NCOMPILER_METHODS(
method("init", &mCopy_nC::init, args({{arg("model",copy)}})),
method("getValues", &mCopy_nC::getValues, args({{}})),
method("setValues", &mCopy_nC::setValues, args({{arg("v",copy)}})),
method("getValues_indexed", &mCopy_nC::getValues_indexed, args({{arg("i",copy)},{arg("mode",copy)}})),
method("setValues_indexed", &mCopy_nC::setValues_indexed, args({{arg("i",copy)},{arg("v",copy)},{arg("mode",copy)}}))
)
)
#endif
