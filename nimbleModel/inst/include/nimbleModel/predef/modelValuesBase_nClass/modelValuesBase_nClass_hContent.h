/* OPENER (Do not edit this comment) */
#ifndef __modelValuesBase_nClass_H
#define __modelValuesBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "modelValuesBase_nClass_c_.h"

class modelValuesBase_nClass : public interface_resolver< genericInterfaceC<modelValuesBase_nClass> >, public loadedObjectHookC<modelValuesBase_nClass> {
public:
      modelValuesBase_nClass (  ) ;
    void  set_sizes ( Rcpp::List new_sizes ) ;
  Rcpp::List sizes;
  int current_nRow_;

};

    void  set_CnClass_env_modelValuesBase_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_modelValuesBase_nClass (  ) ;

#include <nimbleModel/predef/modelValuesClass_/modelValuesClass_.h>

#endif
