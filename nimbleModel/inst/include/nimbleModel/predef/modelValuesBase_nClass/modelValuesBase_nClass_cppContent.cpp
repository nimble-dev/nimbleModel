/* OPENER (Do not edit this comment) */
#ifndef __modelValuesBase_nClass_CPP
#define __modelValuesBase_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "modelValuesBase_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
// [[Rcpp::depends(nimbleModel)]]

      modelValuesBase_nClass::modelValuesBase_nClass (  ) {
RESET_EIGEN_ERRORS
Rcpp::Rcout << "base class compiled Cpublic init\n";;
flex_(current_nRow_) = 0.0;
this->sizes = Rcpp::List();;
}
    void  modelValuesBase_nClass::set_sizes ( Rcpp::List new_sizes ) {
RESET_EIGEN_ERRORS
(this)->sizes = new_sizes;
}
    void  modelValuesBase_nClass::resize ( int m ) {
RESET_EIGEN_ERRORS
Rcpp::stop("Should not be calling compiled modelValuesBase_nClass resize().");;
}
    int  modelValuesBase_nClass::getLength (  ) {
RESET_EIGEN_ERRORS
return(current_nRow_);
}

// [[Rcpp::export(name = "set_CnClass_env_modelValuesBase_nClass_new")]]
    void  set_CnClass_env_modelValuesBase_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(modelValuesBase_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_modelValuesBase_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_modelValuesBase_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(modelValuesBase_nClass);;
}

NCOMPILER_INTERFACE(
modelValuesBase_nClass,
NCOMPILER_FIELDS(
field("sizes", &modelValuesBase_nClass::sizes),
field("current_nRow_", &modelValuesBase_nClass::current_nRow_)
),
NCOMPILER_METHODS(
method("set_sizes", &modelValuesBase_nClass::set_sizes, args({{arg("new_sizes",copy)}})),
method("resize", &modelValuesBase_nClass::resize, args({{arg("m",copy)}})),
method("getLength", &modelValuesBase_nClass::getLength, args({{}}))
)
)
#endif
