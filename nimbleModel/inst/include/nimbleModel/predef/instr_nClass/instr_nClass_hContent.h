/* OPENER (Do not edit this comment) */
#ifndef __instr_nClass_H
#define __instr_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nList_I1_c_.h"

class instr_nClass : public interface_resolver< genericInterfaceC<instr_nClass> >, public loadedObjectHookC<instr_nClass> {
public:
      instr_nClass (  ) ;
  Eigen::Tensor<int, 1> lens;
  Eigen::Tensor<int, 1> index_types;
  int dim;
  Eigen::Tensor<int, 1> dims;
  Eigen::Tensor<int, 1> slots;
  std::shared_ptr<nList_I1> values;
  int type;
  Eigen::Tensor<int, 1> sortID;
  int declID;
};

    SEXP  new_instr_nClass (  ) ;

    void  set_CnClass_env_instr_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_instr_nClass (  ) ;


#endif
