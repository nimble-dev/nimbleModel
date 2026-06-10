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
// [[Rcpp::depends(nimbleModel)]]

    bool  declFunBase_nClass::ping (  ) {
RESET_EIGEN_ERRORS
return(true);
}
    double  declFunBase_nClass::calculate_cpp ( std::shared_ptr<instr_nClass> instr ) {
RESET_EIGEN_ERRORS
Rprintf("declFunBase_nClass virtual base calculate_cpp should never be called (something is wrong)\n");;
return(0.0);
}
    double  declFunBase_nClass::calculateDiff_cpp ( std::shared_ptr<instr_nClass> instr ) {
RESET_EIGEN_ERRORS
Rprintf("declFunBase_nClass virtual base calculateDiff_cpp should never be called (something is wrong)\n");;
return(0.0);
}
    double  declFunBase_nClass::getLogProb_cpp ( std::shared_ptr<instr_nClass> instr ) {
RESET_EIGEN_ERRORS
Rprintf("declFunBase_nClass virtual base getLogProb_cpp should never be called (something is wrong)\n");;
return(0.0);
}
    void  declFunBase_nClass::simulate_cpp ( std::shared_ptr<instr_nClass> instr ) {
RESET_EIGEN_ERRORS
Rprintf("declFunBase_nClass virtual base simulate_cpp should never be called (something is wrong)\n");;
}
    std::unique_ptr<ETaccessorBase>  declFunBase_nClass::getParam_cpp ( std::shared_ptr<instr_nClass> instr, int param ) {
RESET_EIGEN_ERRORS
Rprintf("declFunBase_nClass virtual base getParam_cpp should never be called (something is wrong)\n");;
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
method("calculate_cpp", &declFunBase_nClass::calculate_cpp, args({{arg("instr",copy)}})),
method("calculateDiff_cpp", &declFunBase_nClass::calculateDiff_cpp, args({{arg("instr",copy)}})),
method("getLogProb_cpp", &declFunBase_nClass::getLogProb_cpp, args({{arg("instr",copy)}})),
method("simulate_cpp", &declFunBase_nClass::simulate_cpp, args({{arg("instr",copy)}}))
)
)
#endif
