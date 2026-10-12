#ifndef __MODELVALUESCLASS__
#define __MODELVALUESCLASS__

class modelValuesClass_ : public MVBase_nC {
public:
  int stuff;
  // resize_one could move to the base class
  template<int nDim, typename NLtype, typename SType>
  void resize_one(NLtype &NL,
                  int m,
                  const SType &sizes) {
    if constexpr(nDim > 0) {
      if(sizes.size() != nDim) Rcpp::stop("incorrect number of sizes when resizing an nList in a modelValues.");
    }
    int old_m = NL->getLength();
    NL->setLength(m); // Change number of elements in the list for this variable. Should be data-preserving.
    std::array<Eigen::Index, nDim> dims;
    if constexpr(nDim > 0) {
      std::copy(sizes.data(), sizes.data() + sizes.size(), dims.begin());
      // To-do: make the Eigen::Tensor resizes preserve contents
      if(m > old_m) {
        for(int i = old_m; i < m; i++) {
          (*NL)[i].resize(dims);
        }
      }
    }
  }
  template<int nDim, typename NLtype>
  void resize_one(NLtype &NL,
                  int m,
                  SEXP Ssizes) {
    if constexpr(nDim == 0) {
      return resize_one<nDim, NLtype, SEXP>(NL, m, Ssizes); // Ssizes will be compile-time ignored.
    }
    if(Ssizes == R_NilValue) {
      Rcpp::stop("A non-scalar variable in a modelValues is missing a sizeList entry.");
    }
    resize_one<nDim, NLtype, Eigen::Tensor<int, 1>>(NL, m, Rcpp::as<Eigen::Tensor<int, 1>>(Ssizes));
  }
  SEXP getSizeOrNULL(const Rcpp::List &sizeList, const std::string &varName) {
    if (sizeList.containsElementNamed(varName.c_str())) {
      return as<SEXP>(sizeList[varName]);
    } else {
      return R_NilValue;
    }
  }
};

#endif
