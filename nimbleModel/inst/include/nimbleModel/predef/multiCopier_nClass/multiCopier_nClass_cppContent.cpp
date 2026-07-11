/* OPENER (Do not edit this comment) */
#ifndef __multiCopier_nClass_CPP
#define __multiCopier_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "multiCopier_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
// [[Rcpp::depends(nimbleModel)]]

    void  multiCopier_nClass::init ( std::shared_ptr<modelBase_nClass> model ) {
RESET_EIGEN_ERRORS
multiCopier_nC_base::init(this->copiers, model);
}
    Eigen::Tensor<double, 1>  multiCopier_nClass::getValues (  ) {
RESET_EIGEN_ERRORS
return flatViewGroup.copyIntoVector();
}
    void  multiCopier_nClass::setValues ( Eigen::Tensor<double, 1> v ) {
RESET_EIGEN_ERRORS
flatViewGroup.copyFromVector(v);
}
      multiCopier_nClass::multiCopier_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "multiCopier_nClass_new")]]
    SEXP  new_multiCopier_nClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(multiCopier_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_multiCopier_nClass_new")]]
    void  set_CnClass_env_multiCopier_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(multiCopier_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_multiCopier_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_multiCopier_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(multiCopier_nClass);;
}

NCOMPILER_INTERFACE(
multiCopier_nClass,
NCOMPILER_FIELDS(
field("copiers", &multiCopier_nClass::copiers)
),
NCOMPILER_METHODS(
method("init", &multiCopier_nClass::init, args({{arg("model",copy)}})),
method("getValues", &multiCopier_nClass::getValues, args({{}})),
method("setValues", &multiCopier_nClass::setValues, args({{arg("v",copy)}}))
)
)
#endif
