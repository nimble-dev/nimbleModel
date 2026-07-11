/* OPENER (Do not edit this comment) */
#ifndef __multiCopier_nClass_H
#define __multiCopier_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nList_copier_nClass_c_.h"
#include "multiCopier_nClass_c_.h"
#include "modelBase_nClass_c_.h"
#include <nimbleModel/predef/multiCopier_nC_base/multiCopier_nC_base.h>

class multiCopier_nClass : public interface_resolver< genericInterfaceC<multiCopier_nClass>, multiCopier_nC_base >, public loadedObjectHookC<multiCopier_nClass> {
public:
    void  init ( std::shared_ptr<modelBase_nClass> model ) ;
    Eigen::Tensor<double, 1>  getValues (  ) ;
    void  setValues ( Eigen::Tensor<double, 1> v ) ;
      multiCopier_nClass (  ) ;
  std::shared_ptr<nList_copier_nClass> copiers;

};

    SEXP  new_multiCopier_nClass (  ) ;

    void  set_CnClass_env_multiCopier_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_multiCopier_nClass (  ) ;


#endif
