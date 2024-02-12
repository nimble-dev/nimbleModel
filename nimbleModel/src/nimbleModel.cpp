#include "indexRange.h"
#include <R_ext/Rdynload.h>

#define FUN(name, numArgs) \
  {#name, (DL_FUNC) &name, numArgs}

#define CFUN(name, numArgs) \
  {"R_"#name, (DL_FUNC) &name, numArgs}

R_CallMethodDef CallEntries[] = {
  FUN(C_setIndexRangeScalar, 1),
  FUN(C_setIndexRangeSequence, 2),
  FUN(C_getNumColumns, 1),
  FUN(C_toExpr, 1),
  FUN(C_getMinMax, 1),
  FUN(C_getColumns, 2),
  FUN(C_getValue, 1),
  FUN(C_getStart, 1),
  FUN(C_getEnd, 1),
  FUN(C_getClass, 1),
 {NULL, NULL, 0}
};


extern "C"
void
R_init_nimbleModel(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
