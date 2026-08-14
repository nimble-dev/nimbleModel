/* OPENER (Do not edit this comment) */
#ifndef __multiCopierMV_nClass_H
#define __multiCopierMV_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nList_copier_nClass_c_.h"
#include "multiCopierMV_nClass_c_.h"
#include "modelValuesBase_nClass_c_.h"
#include <nimbleModel/predef/multiCopier_nC_base/multiCopier_modelValues_nC_base.h>
class multiCopierMV_nClass : public interface_resolver< genericInterfaceC<multiCopierMV_nClass>, multiCopier_modelValues_nC_base >, public loadedObjectHookC<multiCopierMV_nClass> {
public:
    void  init ( std::shared_ptr<modelValuesBase_nClass> modelValues ) ;
    void  setActiveRow ( int row ) ;
    Eigen::Tensor<double, 1>  getValues (  ) ;
    void  setValues ( Eigen::Tensor<double, 1> v ) ;
      multiCopierMV_nClass (  ) ;
  std::shared_ptr<nList_copier_nClass> copiers;

};

    SEXP  new_multiCopierMV_nClass (  ) ;

    void  set_CnClass_env_multiCopierMV_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_multiCopierMV_nClass (  ) ;


#endif
