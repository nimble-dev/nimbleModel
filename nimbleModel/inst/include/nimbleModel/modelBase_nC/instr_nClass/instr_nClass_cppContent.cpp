/* OPENER (Do not edit this comment) */
#ifndef __instr_nClass_CPP
#define __instr_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "instr_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      instr_nClass::instr_nClass (  ) {
RESET_EIGEN_ERRORS
values = nClass_builder<nList_I1>()();
}

// [[Rcpp::export(name = "instr_nClass_new")]]
    SEXP  new_instr_nClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(instr_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_instr_nClass_new")]]
    void  set_CnClass_env_instr_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(instr_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_instr_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_instr_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(instr_nClass);;
}

NCOMPILER_INTERFACE(
instr_nClass,
NCOMPILER_FIELDS(
field("lens", &instr_nClass::lens),
field("index_types", &instr_nClass::index_types),
field("dim", &instr_nClass::dim),
field("dims", &instr_nClass::dims),
field("slots", &instr_nClass::slots),
field("values", &instr_nClass::values),
field("type", &instr_nClass::type),
field("sortID", &instr_nClass::sortID),
field("declID", &instr_nClass::declID)
),
NCOMPILER_METHODS()
)
#endif
