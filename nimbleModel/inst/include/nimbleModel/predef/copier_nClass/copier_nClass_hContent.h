/* OPENER (Do not edit this comment) */
#ifndef __copier_nClass_H
#define __copier_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nList_I1_c_.h"
#include "copier_nClass_c_.h"
#include <nimbleModel/predef/copier_nC_base/copier_nC_base.h>

class copier_nClass : public interface_resolver< genericInterfaceC<copier_nClass>, copier_nC_base >, public loadedObjectHookC<copier_nClass> {
public:
      copier_nClass (  ) ;
  std::string varName;
  std::shared_ptr<nList_I1> indsList;

};

    SEXP  new_copier_nClass (  ) ;

    void  set_CnClass_env_copier_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_copier_nClass (  ) ;


#endif
