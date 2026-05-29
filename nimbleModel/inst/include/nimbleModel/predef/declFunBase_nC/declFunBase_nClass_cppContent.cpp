/* OPENER (Do not edit this comment) */
#ifndef __declFunBase_nClass_CPP
#define __declFunBase_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "declFunBase_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

    bool  declFunBase_nClass::ping (  ) {
RESET_EIGEN_ERRORS
return(true);
}
    double  declFunBase_nClass::calculate ( std::shared_ptr<instr_nClass> instr ) {
RESET_EIGEN_ERRORS
if((instr)->type==0.0) {
 return(calc_0(instr));
}
return(0.0);
}
    double  declFunBase_nClass::calc_0 ( std::shared_ptr<instr_nClass> instr ) {
RESET_EIGEN_ERRORS
Rprintf("declFunBase_nClass calc_0 (should not see this)\n");;
return(0.0);
}
      declFunBase_nClass::declFunBase_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "set_CnClass_env_declFunBase_nClass_new")]]
    void  set_CnClass_env_declFunBase_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(declFunBase_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_declFunBase_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_declFunBase_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(declFunBase_nClass);;
}

NCOMPILER_INTERFACE(
declFunBase_nClass,
NCOMPILER_FIELDS(),
NCOMPILER_METHODS(
method("ping", &declFunBase_nClass::ping, args({{}})),
method("calculate", &declFunBase_nClass::calculate, args({{arg("instr",copy)}})),
method("calc_0", &declFunBase_nClass::calc_0, args({{arg("instr",copy)}}))
)
)
#endif
