/* OPENER (Do not edit this comment) */
#ifndef __declFunBase_nClass_H
#define __declFunBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "declFunBase_nClass_c_.h"
#include "instr_nClass_c_.h"

class declFunBase_nClass : public interface_resolver< genericInterfaceC<declFunBase_nClass> >, public loadedObjectHookC<declFunBase_nClass> {
public:
  static const char* nc_class_key() {return "declFunBase_nClass";}
   virtual  bool  ping (  )  ;
   virtual  double  calculate_cpp ( std::shared_ptr<instr_nClass> instr )  ;
   virtual  double  calculateDiff_cpp ( std::shared_ptr<instr_nClass> instr )  ;
   virtual  double  getLogProb_cpp ( std::shared_ptr<instr_nClass> instr )  ;
   virtual  void  simulate_cpp ( std::shared_ptr<instr_nClass> instr )  ;
   virtual  int  getParamID_cpp ( std::string param )  ;
   virtual  std::unique_ptr<ETaccessorBase>  getParam_cpp ( std::shared_ptr<instr_nClass> instr, int paramID )  ;
   virtual  double  getBound_cpp ( std::shared_ptr<instr_nClass> instr, int boundID )  ;
   virtual  double  calculate_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step )  ;
   virtual  double  calculateDiff_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step )  ;
   virtual  double  getLogProb_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step )  ;
   virtual  void  simulate_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step )  ;
   virtual  std::unique_ptr<ETaccessorBase>  getParam_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step, int paramID )  ;
   virtual  double  getBound_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step, int boundID )  ;
   virtual  Eigen::Tensor<double, 1>  getValues_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step )  ;
   virtual  void  setValues_singleton_cpp ( std::shared_ptr<instr_nClass> instr, int step, Eigen::Tensor<double, 1> v )  ;
   virtual  int  valuesLength_cpp ( std::shared_ptr<instr_nClass> instr )  ;
   virtual  int  getValues_into_cpp ( std::shared_ptr<instr_nClass> instr, Eigen::Tensor<double, 1> & out, int offset )  ;
   virtual  int  setValues_from_cpp ( std::shared_ptr<instr_nClass> instr, Eigen::Tensor<double, 1> & v, int offset )  ;
      declFunBase_nClass (  )  ;

};

    void  set_CnClass_env_declFunBase_nClass ( SEXP env )  ;

    Rcpp::Environment  get_CnClass_env_declFunBase_nClass (  )  ;


#include <nimbleModel/predef/declFunClass_/declFunClass_.h>
#endif
