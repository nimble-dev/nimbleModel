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
flex_((this)->dot_current_nRow) = 0.0;
this->dot_sizeList = Rcpp::List();;
}
    void  modelValuesBase_nClass::set_sizeList ( Rcpp::List sizeList ) {
RESET_EIGEN_ERRORS
dot_sizeList = sizeList;
(this)->dot_sizeList = sizeList;
}
    Rcpp::List  modelValuesBase_nClass::get_sizeList (  ) {
RESET_EIGEN_ERRORS
return((this)->dot_sizeList);
}
    void  modelValuesBase_nClass::resize ( int m ) {
RESET_EIGEN_ERRORS
Rcpp::stop("Should not be calling compiled modelValuesBase_nClass resize().");;
}
    int  modelValuesBase_nClass::getLength (  ) {
RESET_EIGEN_ERRORS
return((this)->dot_current_nRow);
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
field("dot_sizeList", &modelValuesBase_nClass::dot_sizeList),
field("dot_current_nRow", &modelValuesBase_nClass::dot_current_nRow)
),
NCOMPILER_METHODS(
method("set_sizeList", &modelValuesBase_nClass::set_sizeList, args({{arg("sizeList",copy)}})),
method("get_sizeList", &modelValuesBase_nClass::get_sizeList, args({{}})),
method("resize", &modelValuesBase_nClass::resize, args({{arg("m",copy)}})),
method("getLength", &modelValuesBase_nClass::getLength, args({{}}))
)
)
#endif
