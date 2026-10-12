/* OPENER (Do not edit this comment) */
#ifndef __MVBase_nC_CPP
#define __MVBase_nC_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "MVBase_nC_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
// [[Rcpp::depends(nimbleModel)]]

      MVBase_nC::MVBase_nC (  )  {
RESET_EIGEN_ERRORS
flex_((this)->dot_current_nRow) = 0.0;
this->dot_sizeList = Rcpp::List();;
}
    void  MVBase_nC::set_sizeList ( Rcpp::List sizeList )  {
RESET_EIGEN_ERRORS
(this)->dot_sizeList = sizeList;
}
    Rcpp::List  MVBase_nC::get_sizeList (  )  {
RESET_EIGEN_ERRORS
return((this)->dot_sizeList);
}
    void  MVBase_nC::resize ( int m )  {
RESET_EIGEN_ERRORS
Rcpp::stop("Should not be calling compiled modelValuesBase_nClass resize().");;
}
    int  MVBase_nC::getLength (  )  {
RESET_EIGEN_ERRORS
return((this)->dot_current_nRow);
}
    int  MVBase_nC::getSize (  )  {
RESET_EIGEN_ERRORS
return((this)->getLength());
}

// [[Rcpp::export(name = "set_CnClass_env_modelValuesBase_nClass_new")]]
    void  set_CnClass_env_MVBase_nC ( SEXP env )  {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(MVBase_nC, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_modelValuesBase_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_MVBase_nC (  )  {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(MVBase_nC);;
}

NCOMPILER_INTERFACE(
MVBase_nC,
NCOMPILER_FIELDS(
field("dot_sizeList", &MVBase_nC::dot_sizeList),
field("dot_current_nRow", &MVBase_nC::dot_current_nRow)
),
NCOMPILER_METHODS(
method("set_sizeList", &MVBase_nC::set_sizeList, args({{arg("sizeList",copy)}})),
method("get_sizeList", &MVBase_nC::get_sizeList, args({{}})),
method("resize", &MVBase_nC::resize, args({{arg("m",copy)}})),
method("getLength", &MVBase_nC::getLength, args({{}})),
method("getSize", &MVBase_nC::getSize, args({{}}))
)
)
#endif
