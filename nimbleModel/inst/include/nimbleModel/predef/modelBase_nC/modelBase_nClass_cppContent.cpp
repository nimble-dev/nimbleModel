/* OPENER (Do not edit this comment) */
#ifndef __modelBase_nClass_CPP
#define __modelBase_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "modelBase_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
// [[Rcpp::depends(nimbleModel)]]

    bool  modelBase_nClass::ping (  ) {
RESET_EIGEN_ERRORS
return(true);
}
    std::shared_ptr<nList_instr_nClass>  modelBase_nClass::makeCompiledInstrList ( SEXP input ) {
RESET_EIGEN_ERRORS
std::shared_ptr<nList_instr_nClass> ans;
ans = nClass_builder<nList_instr_nClass>()();
ans->set_all_values(input);;
return(ans);
}
    double  modelBase_nClass::calculate_impl ( std::shared_ptr<nList_instr_nClass> instrList ) {
RESET_EIGEN_ERRORS
Rprintf("modelBase_nClass calculate_impl (should not see this)\n");;
return(0.0);
}
    double  modelBase_nClass::calculateDiff_impl ( std::shared_ptr<nList_instr_nClass> instrList ) {
RESET_EIGEN_ERRORS
Rprintf("modelBase_nClass calculateDiff_impl (should not see this)\n");;
return(0.0);
}
    double  modelBase_nClass::getLogProb_impl ( std::shared_ptr<nList_instr_nClass> instrList ) {
RESET_EIGEN_ERRORS
Rprintf("modelBase_nClass getLogProb_impl (should not see this)\n");;
return(0.0);
}
    void  modelBase_nClass::simulate_impl ( std::shared_ptr<nList_instr_nClass> instrList ) {
RESET_EIGEN_ERRORS
Rprintf("modelBase_nClass simulate_impl (should not see this)\n");;
}
    std::unique_ptr<ETaccessorBase>  modelBase_nClass::getParam_impl ( std::shared_ptr<instr_nClass> instr, int param ) {
RESET_EIGEN_ERRORS
Rprintf("modelBase_nClass getParam_impl (should not see this)\n");;
}
    SEXP  modelBase_nClass::getParam_impl_R ( std::shared_ptr<instr_nClass> instr, int param ) {
RESET_EIGEN_ERRORS
return getParam_impl(instr, param)->get();;
}
      modelBase_nClass::modelBase_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "set_CnClass_env_modelBase_nClass_new")]]
    void  set_CnClass_env_modelBase_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(modelBase_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_modelBase_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_modelBase_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(modelBase_nClass);;
}

NCOMPILER_INTERFACE(
modelBase_nClass,
NCOMPILER_FIELDS(
field("declFunList", &modelBase_nClass::declFunList),
field("declFunNameToIndex", &modelBase_nClass::declFunNameToIndex)
),
NCOMPILER_METHODS(
method("ping", &modelBase_nClass::ping, args({{}})),
method("makeCompiledInstrList", &modelBase_nClass::makeCompiledInstrList, args({{arg("input",copy)}})),
method("calculate_impl", &modelBase_nClass::calculate_impl, args({{arg("instrList",copy)}})),
method("calculateDiff_impl", &modelBase_nClass::calculateDiff_impl, args({{arg("instrList",copy)}})),
method("getLogProb_impl", &modelBase_nClass::getLogProb_impl, args({{arg("instrList",copy)}})),
method("simulate_impl", &modelBase_nClass::simulate_impl, args({{arg("instrList",copy)}})),
method("getParam_impl_R", &modelBase_nClass::getParam_impl_R, args({{arg("instr",copy)},{arg("param",copy)}}))
)
)
#endif
