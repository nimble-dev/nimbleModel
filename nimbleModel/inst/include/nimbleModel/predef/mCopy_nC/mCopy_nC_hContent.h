/* OPENER (Do not edit this comment) */
#ifndef __mCopy_nC_H
#define __mCopy_nC_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nList_copier_nClass_c_.h"
#include "mCopy_nC_c_.h"
#include "modelBase_nClass_c_.h"
#include <nimbleModel/predef/mCopy_nC_base/mCopy_model_nC_base.h>

class mCopy_nC : public interface_resolver< genericInterfaceC<mCopy_nC>, multiCopier_model_nC_base >, public loadedObjectHookC<mCopy_nC> {
public:
  static const char* nc_class_key() {return "mCopy_nC";}
    void  init ( std::shared_ptr<modelBase_nClass> model )  ;
    Eigen::Tensor<double, 1>  getValues (  )  ;
    void  setValues ( Eigen::Tensor<double, 1> v )  ;
      mCopy_nC (  )  ;
  std::shared_ptr<nList_copier_nClass> copiers;

};

    SEXP  new_mCopy_nC (  )  ;

    void  set_CnClass_env_mCopy_nC ( SEXP env )  ;

    Rcpp::Environment  get_CnClass_env_mCopy_nC (  )  ;


#endif
