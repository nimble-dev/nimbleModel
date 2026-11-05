/* OPENER (Do not edit this comment) */
#ifndef __instrList_nClass_H
#define __instrList_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nList_instr_nClass_c_.h"
#include "instrList_nClass_c_.h"

class instrList_nClass : public interface_resolver< genericInterfaceC<instrList_nClass> >, public loadedObjectHookC<instrList_nClass> {
public:
  static const char* nc_class_key() {return "instrList_nClass";}
      instrList_nClass (  )  ;
  std::shared_ptr<nList_instr_nClass> instr_nList;
  Eigen::Tensor<int, 1> offsets;
  int nNodes_total;

};

    SEXP  new_instrList_nClass (  )  ;

    void  set_CnClass_env_instrList_nClass ( SEXP env )  ;

    Rcpp::Environment  get_CnClass_env_instrList_nClass (  )  ;


#endif
