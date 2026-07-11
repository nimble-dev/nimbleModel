/* OPENER (Do not edit this comment) */
#ifndef __copier_nClass_CPP
#define __copier_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "copier_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
// [[Rcpp::depends(nimbleModel)]]

      copier_nClass::copier_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "copier_nClass_new")]]
    SEXP  new_copier_nClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(copier_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_copier_nClass_new")]]
    void  set_CnClass_env_copier_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(copier_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_copier_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_copier_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(copier_nClass);;
}

NCOMPILER_INTERFACE(
copier_nClass,
NCOMPILER_FIELDS(
field("varName", &copier_nClass::varName),
field("indsList", &copier_nClass::indsList)
),
NCOMPILER_METHODS()
)
#endif
