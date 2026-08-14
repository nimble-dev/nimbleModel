/* OPENER (Do not edit this comment) */
#ifndef __multiCopierMV_nClass_CPP
#define __multiCopierMV_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "multiCopierMV_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
// [[Rcpp::depends(nimbleModel)]]

    void  multiCopierMV_nClass::init ( std::shared_ptr<modelValuesBase_nClass> modelValues ) {
RESET_EIGEN_ERRORS
multiCopier_modelValues_nC_base::init(this->copiers, modelValues);
}
    void  multiCopierMV_nClass::setActiveRow ( int row ) {
RESET_EIGEN_ERRORS
multiCopier_modelValues_nC_base::setActiveRow(row);
}
    Eigen::Tensor<double, 1>  multiCopierMV_nClass::getValues (  ) {
RESET_EIGEN_ERRORS
return flatViewGroup.copyIntoVector();
}
    void  multiCopierMV_nClass::setValues ( Eigen::Tensor<double, 1> v ) {
RESET_EIGEN_ERRORS
flatViewGroup.copyFromVector(v);
}
      multiCopierMV_nClass::multiCopierMV_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "multiCopierMV_nClass_new")]]
    SEXP  new_multiCopierMV_nClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(multiCopierMV_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_multiCopierMV_nClass_new")]]
    void  set_CnClass_env_multiCopierMV_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(multiCopierMV_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_multiCopierMV_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_multiCopierMV_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(multiCopierMV_nClass);;
}

NCOMPILER_INTERFACE(
multiCopierMV_nClass,
NCOMPILER_FIELDS(
field("copiers", &multiCopierMV_nClass::copiers)
),
NCOMPILER_METHODS(
method("init", &multiCopierMV_nClass::init, args({{arg("modelValues",copy)}})),
method("setActiveRow", &multiCopierMV_nClass::setActiveRow, args({{arg("row",copy)}})),
method("getValues", &multiCopierMV_nClass::getValues, args({{}})),
method("setValues", &multiCopierMV_nClass::setValues, args({{arg("v",copy)}}))
)
)
#endif
