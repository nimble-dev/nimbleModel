#include "indexRange.h"

SEXP vectorInt_2_SEXP(const vector<int> &v) {
  SEXP Sans;
  int nn = v.size();
  PROTECT(Sans = Rf_allocVector(INTSXP, nn));
  if(nn > 0) {
    copy(v.begin(), v.end(), INTEGER(Sans));
  }
  UNPROTECT(1);
  return(Sans);
}

SEXP int_2_SEXP(int i) {
  SEXP Sans;
  PROTECT(Sans = Rf_allocVector(INTSXP, 1));
  INTEGER(Sans)[0] = i;
  UNPROTECT(1);
  return(Sans);
}

SEXP string_2_STRSEXP(string v) {
  SEXP Sans;
  PROTECT(Sans = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(Sans, 0, Rf_mkChar(v.c_str()));
  UNPROTECT(1);
  return(Sans);
}

string STRSEXP_2_string(SEXP Ss, int i = 0) {
  if(!Rf_isString(Ss)) {
    PRINTF("Error: STRSEXP_2_string called for SEXP that is not a string!\n"); 
    return(string(""));
  }
  if(LENGTH(Ss) <= i) {
    PRINTF("Error: STRSEXP_2_string called for (C) element %i of an SEXP that has length %i!\n", i, LENGTH(Ss));
    return(string(""));
  }
  int l = LENGTH(STRING_ELT(Ss, i));
  string ans(CHAR(STRING_ELT(Ss,i)),l);
  return(ans);
}

int SEXP_2_int(SEXP Sn, int i = 0) {
  if(!(Rf_isNumeric(Sn) || Rf_isLogical(Sn))) PRINTF("Error: SEXP_2_int called for SEXP that is not numeric or logical\n");
  if(LENGTH(Sn) <= i) PRINTF("Error: SEXP_2_int called for element %i which is beyond the length of %i.\n", i, LENGTH(Sn));
  if(Rf_isInteger(Sn) || Rf_isLogical(Sn)) {
    if(Rf_isInteger(Sn))
      return(INTEGER(Sn)[i]);
    else
      return(LOGICAL(Sn)[i]);
  } else {
    if(Rf_isReal(Sn)) {
      double ans = REAL(Sn)[i];
      if(ans != floor(ans)) PRINTF("Warning from SEXP_2_int: input element is a real with a non-integer value\n");
      return(static_cast<int>(ans));
    } else {
      PRINTF("Error: We could not handle input type to  SEXP_2_int\n");
    }
  }
  return(0);
}

double SEXP_2_double(SEXP Sn, int i = 0) {
  if(!(Rf_isNumeric(Sn) || Rf_isLogical(Sn))) PRINTF("Error: SEXP_2_double called for SEXP that is not numeric or logical\n");
  if(LENGTH(Sn) <= i) PRINTF("Error: SEXP_2_double called for element %i >= length of %i.\n", i, LENGTH(Sn));
  if(Rf_isReal(Sn)) {
    return(REAL(Sn)[i]);
  } 
  if(Rf_isInteger(Sn) || Rf_isLogical(Sn)) {
    if(Rf_isInteger(Sn))
      return(static_cast<double>(INTEGER(Sn)[i]));
    else
      return(static_cast<double>(LOGICAL(Sn)[i]));
  }
  PRINTF("Error: We could not handle the input type to SEXP_2_double\n");
  return(0.);
}

int indexRangeClass::getItem(int item) {
  return 0;
}

string indexRangeClass::toExpr() {
  return "";
}

vector<int> indexRangeClass::getMinMax() {
  vector<int> ans(2, 0);
  return ans;
}

void indexRangeClass::setDelay(int delay_) {
  current = 1;
  local = 1;
  delay = delay_;   // or use this.delay?
}

int indexRangeClass::getNext() {
  int item = current;
  if (local < delay) {
    local++;
  } else {
    local = 1;
    current++;
    if (current > numElements)
      current = 1;
  }
  return getItem(item);
}

int indexRangeScalarClass::getItem(int item)  {
  return value;
}

string indexRangeScalarClass::toExpr()  {
  return std::to_string(value);
}

vector<int> indexRangeScalarClass::getMinMax() {
  vector<int> ans(2, value);
  return ans;
}

int indexRangeSequenceClass::getItem(int item)  {
  return start + item - 1;
}

string indexRangeSequenceClass::toExpr()  {
  return std::to_string(start) + ":" + std::to_string(end);
}

vector<int> indexRangeSequenceClass::getMinMax() {
  vector<int> ans(2, 0);
  ans[0] = start;
  ans[1] = end;
  return ans;
}

// need all matrix stuff


SEXP C_setIndexRangeScalar(SEXP value_) {
  int value = SEXP_2_int(value_);
  indexRangeScalarClass* indexRange = new indexRangeScalarClass(value);
  SEXP SextPtrAns;
  PROTECT(SextPtrAns = R_MakeExternalPtr(indexRange, R_NilValue, R_NilValue));
  UNPROTECT(1);
  return(SextPtrAns);
  
}

SEXP C_setIndexRangeSequence(SEXP start_, SEXP end_) {
  int start = SEXP_2_int(start_);
  int end = SEXP_2_int(end_);
  indexRangeSequenceClass* indexRange = new indexRangeSequenceClass(start, end);
  SEXP SextPtrAns;
  PROTECT(SextPtrAns = R_MakeExternalPtr(indexRange, R_NilValue, R_NilValue));
  UNPROTECT(1);
  return(SextPtrAns);
}

SEXP C_getNumColumns(SEXP IndexRangeExtPtr) {
  indexRangeClass* indexRange = static_cast<indexRangeClass *>(R_ExternalPtrAddr(IndexRangeExtPtr));
  return int_2_SEXP(indexRange->numColumns);
}

SEXP C_toExpr(SEXP IndexRangeExtPtr) {
  indexRangeClass* indexRange = static_cast<indexRangeClass *>(R_ExternalPtrAddr(IndexRangeExtPtr));
  return string_2_STRSEXP(indexRange->toExpr());
}

SEXP C_getMinMax(SEXP IndexRangeExtPtr) {
  indexRangeClass* indexRange = static_cast<indexRangeClass *>(R_ExternalPtrAddr(IndexRangeExtPtr));
  vector<int> result = indexRange->getMinMax();
  return(vectorInt_2_SEXP(result));
}

SEXP C_getColumns(SEXP IndexRangeExtPtr, SEXP innerIndices) {
  return IndexRangeExtPtr;
}

SEXP C_getValue(SEXP IndexRangeExtPtr) {
  indexRangeScalarClass* indexRange = static_cast<indexRangeScalarClass *>(R_ExternalPtrAddr(IndexRangeExtPtr));
  return int_2_SEXP(indexRange->value);
}

SEXP C_getStart(SEXP IndexRangeExtPtr) {
  indexRangeSequenceClass* indexRange = static_cast<indexRangeSequenceClass *>(R_ExternalPtrAddr(IndexRangeExtPtr));
  return int_2_SEXP(indexRange->start);
}
SEXP C_getEnd(SEXP IndexRangeExtPtr) {
  indexRangeSequenceClass* indexRange = static_cast<indexRangeSequenceClass *>(R_ExternalPtrAddr(IndexRangeExtPtr));
  return int_2_SEXP(indexRange->end);
}

SEXP C_getClass(SEXP IndexRangeExtPtr) {
  indexRangeClass* indexRange = static_cast<indexRangeClass *>(R_ExternalPtrAddr(IndexRangeExtPtr));
  return string_2_STRSEXP(indexRange->className);
}

