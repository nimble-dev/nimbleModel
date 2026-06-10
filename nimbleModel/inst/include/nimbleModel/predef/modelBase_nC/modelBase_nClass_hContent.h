/* OPENER (Do not edit this comment) */
#ifndef __modelBase_nClass_H
#define __modelBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "modelBase_nClass_c_.h"
#include "declFunBase_nClass_c_.h"
#include "instr_nClass_c_.h"
#include "nList_instr_nClass_c_.h"

class modelBase_nClass : public interface_resolver< genericInterfaceC<modelBase_nClass> >, public loadedObjectHookC<modelBase_nClass> {
public:
   virtual  bool  ping (  ) ;
    std::shared_ptr<nList_instr_nClass>  makeCompiledInstrList ( SEXP input ) ;
   virtual  double  calculate_impl ( std::shared_ptr<nList_instr_nClass> instrList ) ;
   virtual  double  calculateDiff_impl ( std::shared_ptr<nList_instr_nClass> instrList ) ;
   virtual  double  getLogProb_impl ( std::shared_ptr<nList_instr_nClass> instrList ) ;
   virtual  void  simulate_impl ( std::shared_ptr<nList_instr_nClass> instrList ) ;
   virtual  std::unique_ptr<ETaccessorBase>  getParam_impl ( std::shared_ptr<instr_nClass> instr, int param ) ;
    SEXP  getParam_impl_R ( std::shared_ptr<instr_nClass> instr, int param ) ;
      modelBase_nClass (  ) ;
  double declFunList;
  Rcpp::List declFunNameToIndex;

};

    void  set_CnClass_env_modelBase_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_modelBase_nClass (  ) ;

#include <nimbleModel/predef/modelClass_/modelClass_.h>

#endif
