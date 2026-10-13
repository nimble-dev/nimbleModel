/* OPENER (Do not edit this comment) */
#ifndef __MVBase_nC_H
#define __MVBase_nC_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "MVBase_nC_c_.h"

class MVBase_nC : public interface_resolver< genericInterfaceC<MVBase_nC> >, public loadedObjectHookC<MVBase_nC> {
public:
  static const char* nc_class_key() {return "MVBase_nC";}
      MVBase_nC (  )  ;
    void  set_sizeList ( Rcpp::List sizeList )  ;
    Rcpp::List  get_sizeList (  )  ;
   virtual  void  resize ( int m )  ;
   virtual  int  getLength (  )  ;
   virtual  int  getSize (  )  ;
  Rcpp::List dot_sizeList;
  int dot_current_nRow;

};

    void  set_CnClass_env_MVBase_nC ( SEXP env )  ;

    Rcpp::Environment  get_CnClass_env_MVBase_nC (  )  ;

#include <nimbleModel/predef/modelValuesClass_/modelValuesClass_.h>

#endif
