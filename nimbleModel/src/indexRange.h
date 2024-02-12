#include "R.h"
#include<Rinternals.h>
#include<vector>
#include<algorithm>
#include<string>
#include<iostream>
using std::vector;
using std::string;

#define PRINTF Rprintf

class indexRangeClass {
public:  
  void setDelay(int delay_);
  int getNext();
  virtual string toExpr();
  virtual int getItem(int item);
  virtual vector<int> getMinMax();
  int numElements;
  int numColumns = 1;
  int current;
  int local;
  int delay;
  string className;  // could be enum class
};

// == nimType::INT
// enum class nimType { INT = 1, DOUBLE = 2, BOOL = 3, UNDEFINED = -1 };


class indexRangeScalarClass : public indexRangeClass {
public:
  int value;
indexRangeScalarClass(int value_) : indexRangeClass(), value(value_) {
    numElements = 1;
    className = "indexRangeScalarClass";
  }
  int getItem(int item) override;
  string toExpr() override;
  vector<int> getMinMax() override;
}; 

class indexRangeSequenceClass : public indexRangeClass {
public:
  int start;
  int end;
indexRangeSequenceClass(int start_, int end_) : indexRangeClass(), start(start_), end(end_) {
    numElements = end - start + 1;
    className = "indexRangeSequenceClass";
  }
  int getItem(int item) override;
  string toExpr() override;
  vector<int> getMinMax() override;
}; 


extern "C" {
  SEXP C_setIndexRangeScalar(SEXP value_);
  SEXP C_setIndexRangeSequence(SEXP start_, SEXP end_);
  SEXP C_getNumColumns(SEXP IndexRangeExtPtr);
  SEXP C_toExpr(SEXP IndexRangeExtPtr);
  SEXP C_getMinMax(SEXP IndexRangeExtPtr);
  SEXP C_getColumns(SEXP IndexRangeExtPtr, SEXP innerIndices);
  SEXP C_getValue(SEXP IndexRangeExtPtr);
  SEXP C_getStart(SEXP IndexRangeExtPtr);
  SEXP C_getEnd(SEXP IndexRangeExtPtr);
  SEXP C_getClass(SEXP IndexRangeExtPtr);
}
