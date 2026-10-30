/* OPENER (Do not edit this comment) */
#ifndef __modelBase_nClass_H
#define __modelBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "modelBase_nClass_c_.h"
#include "instrList_nClass_c_.h"
#include "instr_nClass_c_.h"
#include "declFunBase_nClass_c_.h"

class modelBase_nClass : public interface_resolver< genericInterfaceC<modelBase_nClass> >, public loadedObjectHookC<modelBase_nClass> {
public:
  static const char* nc_class_key() {return "modelBase_nClass";}
   virtual  bool  ping (  )  ;
    std::shared_ptr<instrList_nClass>  makeCompiledInstrList ( SEXP input )  ;
    std::shared_ptr<instr_nClass>  makeCompiledInstr ( SEXP input )  ;
   virtual  double  calculate_impl ( std::shared_ptr<instrList_nClass> instrList )  ;
   virtual  double  calculateDiff_impl ( std::shared_ptr<instrList_nClass> instrList )  ;
   virtual  double  getLogProb_impl ( std::shared_ptr<instrList_nClass> instrList )  ;
   virtual  void  simulate_impl ( std::shared_ptr<instrList_nClass> instrList )  ;
   virtual  int  getParamID_impl ( std::shared_ptr<instr_nClass> node, std::string param )  ;
   virtual  std::unique_ptr<ETaccessorBase>  getParam_impl ( std::shared_ptr<instr_nClass> node, int paramID )  ;
   virtual  double  getBound_impl ( std::shared_ptr<instr_nClass> node, int boundID )  ;
   virtual  double  calculate_indexed_impl ( std::shared_ptr<instrList_nClass> instrList, int step )  ;
   virtual  double  calculateDiff_indexed_impl ( std::shared_ptr<instrList_nClass> instrList, int step )  ;
   virtual  double  getLogProb_indexed_impl ( std::shared_ptr<instrList_nClass> instrList, int step )  ;
   virtual  void  simulate_indexed_impl ( std::shared_ptr<instrList_nClass> instrList, int step )  ;
   virtual  std::unique_ptr<ETaccessorBase>  getParam_indexed_impl ( std::shared_ptr<instrList_nClass> instrList, int step, int paramID )  ;
   virtual  double  getBound_indexed_impl ( std::shared_ptr<instrList_nClass> instrList, int step, int boundID )  ;
      modelBase_nClass (  )  ;

};

    void  set_CnClass_env_modelBase_nClass ( SEXP env )  ;

    Rcpp::Environment  get_CnClass_env_modelBase_nClass (  )  ;


#include <nimbleModel/predef/modelClass_/modelClass_.h>
#endif
