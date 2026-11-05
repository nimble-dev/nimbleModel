/* OPENER (Do not edit this comment) */
#ifndef __instrList_nClass_CPP
#define __instrList_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "instrList_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      instrList_nClass::instrList_nClass (  )  {
RESET_EIGEN_ERRORS
instr_nList = nClass_builder<nList_instr_nClass>()();
}

// [[Rcpp::export(name = "instrList_nClass_new")]]
    SEXP  new_instrList_nClass (  )  {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(instrList_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_instrList_nClass_new")]]
    void  set_CnClass_env_instrList_nClass ( SEXP env )  {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(instrList_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_instrList_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_instrList_nClass (  )  {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(instrList_nClass);;
}

NCOMPILER_INTERFACE(
instrList_nClass,
NCOMPILER_FIELDS(
field("instr_nList", &instrList_nClass::instr_nList),
field("offsets", &instrList_nClass::offsets),
field("nNodes_total", &instrList_nClass::nNodes_total)
),
NCOMPILER_METHODS()
)
#endif
