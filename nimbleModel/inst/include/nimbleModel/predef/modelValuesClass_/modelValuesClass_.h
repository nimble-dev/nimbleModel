#ifndef __MODELVALUESCLASS__
#define __MODELVALUESCLASS__

class modelValuesClass_ : public modelValuesBase_nClass {
public:
    int stuff;
    // resize_one could move to the base class
    template<int nDim, typename NLtype, typename SType>
    void resize_one(NLtype &NL,
                    int m,
                    const SType &sizes) {
        if(sizes.size() != nDim) Rcpp::stop("incorrect number of sizes when resizing an nList in a modelValues.");
        NL->setLength(m);
        std::array<Eigen::Index, nDim> dims;
        std::copy(sizes.data(), sizes.data() + sizes.size(), dims.begin());
        // To-do: make the Eigen::Tensor resizes preserve contents
        for(int i = 0; i < m; i++) {
            (*NL)[i].resize(dims);
        }
    }
    template<int nDim, typename NLtype>
    void resize_one(NLtype &NL,
                    int m,
                    SEXP Ssizes) {
        resize_one<nDim, NLtype, Eigen::Tensor<int, 1>>(NL, m, Rcpp::as<Eigen::Tensor<int, 1>>(Ssizes));
    }
};

#endif
