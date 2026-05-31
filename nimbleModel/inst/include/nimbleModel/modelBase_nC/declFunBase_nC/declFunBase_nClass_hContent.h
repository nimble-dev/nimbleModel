/* OPENER (Do not edit this comment) */
#ifndef __declFunBase_nClass_H
#define __declFunBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "instr_nClass_c_.h"

class declFunBase_nClass : public interface_resolver< genericInterfaceC<declFunBase_nClass> >, public loadedObjectHookC<declFunBase_nClass> {
public:
   virtual  bool  ping (  ) ;
   virtual  double  calculate ( std::shared_ptr<instr_nClass> instr ) ;
   virtual  double  calc_0 ( std::shared_ptr<instr_nClass> instr ) ;
      declFunBase_nClass (  ) ;
};

    void  set_CnClass_env_declFunBase_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_declFunBase_nClass (  ) ;

#include <nimbleModel/predef/declFunClass_/declFunClass_.h>

#endif
