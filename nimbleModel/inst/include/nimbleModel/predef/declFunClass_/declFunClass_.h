// to be included from the predefined nodeFxnBase_nClass.
// Add "#include <nCompiler/predef/nodeFxnClass_/nodeFxnClass_.h>" the compileInfo$HpostCode
// so it appears after the declaration of nodeFxnBase_nClass.

// NIDX is the number of loop indices of the declaration, i.e. the length of `idx`.
template<class Derived, int NIDX>
class declFunClass_ : public declFunBase_nClass {
public:
    double v;
    // Reusable idx tensor for singleton runners.
    Eigen::Tensor<int, 1> singleton_idx_;
    declFunClass_() : singleton_idx_(NIDX) {};

    // Fill `idx` for one step (node) of `instr`, with no loop. `step` is 1-based; it becomes
    // 0-based here, where the counters are used as C++ indices into each range's values.
    // Slots stay 1-based on the instruction and are converted at the moment of use.
    // This is important for decls like z[i] <- 10*i, where i on the RHS must be 1-based.
    void set_instr_idx_(const instr_nClass& instr, int step, Eigen::Tensor<int, 1>& idx) {
        const int k = step - 1;
        const int nRanges = instr.lens.size();
        for(int r = 0; r < nRanges; ++r) {
            const int counter = (k / instr.strides[r]) % instr.lens[r];
            const auto& vals = instr.values->operator[](r);
            const int start = instr.idxSlotStarts[r];
            if(instr.index_types[r] == 1) {
                idx[instr.slots[start] - 1] = vals[0] + counter;
            } else {
                const int d = instr.dims[r];
                for(int p = 0; p < d; ++p)
                    idx[instr.slots[start + p] - 1] = vals[counter * d + p];
            }
        }
    }
    // Run one per-node method for one step. A declaration with no loop indices
    // has one node and gets a dummy idx, as in calc_0_.
    template<auto Method>
    double singleton_op_(std::shared_ptr<instr_nClass> instr, int step) {
        RESET_EIGEN_ERRORS;
        if constexpr (NIDX == 0) {
            return (static_cast<Derived*>(this)->*Method)(instr->lens);
        } else {
            set_instr_idx_(*instr, step, singleton_idx_);
            return (static_cast<Derived*>(this)->*Method)(singleton_idx_);
        }
    }
    double calculate_singleton_cpp(std::shared_ptr<instr_nClass> instr, int step) override {
        return singleton_op_< &Derived::calc_one >(instr, step);
    }
    double calculateDiff_singleton_cpp(std::shared_ptr<instr_nClass> instr, int step) override {
        return singleton_op_< &Derived::calcDiff_one >(instr, step);
    }
    double getLogProb_singleton_cpp(std::shared_ptr<instr_nClass> instr, int step) override {
        return singleton_op_< &Derived::getLogProb_one >(instr, step);
    }
    void simulate_singleton_cpp(std::shared_ptr<instr_nClass> instr, int step) override {
        singleton_op_< &Derived::sim_one >(instr, step);
    }
    std::unique_ptr<ETaccessorBase> getParam_singleton_cpp(std::shared_ptr<instr_nClass> instr, int step, int paramID) override {
        RESET_EIGEN_ERRORS;
        if constexpr (NIDX == 0) {
            return static_cast<Derived*>(this)->getParam_one(instr->lens, paramID);
        } else {
            set_instr_idx_(*instr, step, singleton_idx_);
            return static_cast<Derived*>(this)->getParam_one(singleton_idx_, paramID);
        }
    }
    double getBound_singleton_cpp(std::shared_ptr<instr_nClass> instr, int step, int boundID) override {
        RESET_EIGEN_ERRORS;
        if constexpr (NIDX == 0) {
            return static_cast<Derived*>(this)->getBound_one(instr->lens, boundID);
        } else {
            set_instr_idx_(*instr, step, singleton_idx_);
            return static_cast<Derived*>(this)->getBound_one(singleton_idx_, boundID);
        }
    }

    double calculate_cpp( std::shared_ptr<instr_nClass> instr) override {
        return calc_op_< &Derived::calc_one >(instr);
    }
    double calculateDiff_cpp( std::shared_ptr<instr_nClass> instr) override {
        return calc_op_< &Derived::calcDiff_one >(instr);
    }
    double getLogProb_cpp( std::shared_ptr<instr_nClass> instr) override {
        return calc_op_< &Derived::getLogProb_one >(instr);
    }
    // sim_one returns a dummy 0, so simulate shares the calc runners and ignores their sum.
    void simulate_cpp( std::shared_ptr<instr_nClass> instr) override {
        calc_op_< &Derived::sim_one >(instr);
    }
    // Each case is compiled only if its instruction type can occur for this declaration
    // (see NIDX), so a declaration doesn't instantiate runners it can never use.
    // Types with only single-slot ranges fill exactly as many slots as they have ranges;
    // types with a matp range fill more.
    template<auto Method>
    double  calc_op_ ( std::shared_ptr<instr_nClass> instr ) {
        RESET_EIGEN_ERRORS;
        switch(instr->instr_type) {
        case 0:  if constexpr (NIDX == 0) return calc_0_< Method >(instr); break;
        case 1:  if constexpr (NIDX == 1) return calc_1_seq_< Method >(instr); break;
        case 2:  if constexpr (NIDX == 1) return calc_1_mat_< Method >(instr); break;
        case 3:  if constexpr (NIDX >= 2) return calc_1_matp_< Method >(instr); break;
        case 4:  if constexpr (NIDX == 2) return calc_2_seq_seq_< Method >(instr); break;
        case 5:  if constexpr (NIDX == 2) return calc_2_seq_mat_< Method >(instr); break;
        case 6:  if constexpr (NIDX == 2) return calc_2_mat_seq_< Method >(instr); break;
        case 7:  if constexpr (NIDX == 2) return calc_2_mat_mat_< Method >(instr); break;
        case 8:  if constexpr (NIDX >= 3) return calc_2_seq_matp_< Method >(instr); break;
        case 9:  if constexpr (NIDX >= 3) return calc_2_matp_seq_< Method >(instr); break;
        case 10: if constexpr (NIDX >= 3) return calc_2_matp_matp_< Method >(instr); break;
        case 11: if constexpr (NIDX == 2) return calc_2_x_y_ord_< Method >(instr); break;
        case 12: if constexpr (NIDX == 3) return calc_3_allseq_< Method >(instr); break;
        case 13: if constexpr (NIDX >= 3) return calc_3_generic_< Method >(instr); break;
        case 14: if constexpr (NIDX == 4) return calc_4_allseq_< Method >(instr); break;
        case 15: if constexpr (NIDX >= 4) return calc_4_generic_< Method >(instr); break;
        case 16: if constexpr (NIDX == 5) return calc_5_allseq_< Method >(instr); break;
        case 17: if constexpr (NIDX >= 5) return calc_5_generic_< Method >(instr); break;
        case 18: if constexpr (NIDX >= 2) return calc_1_matp_ord_< Method >(instr); break;
        }
        Rcpp::stop("instruction type %d cannot occur for a declaration with %d loop indices",
                   instr->instr_type, NIDX);
    }
    template<auto Method>
    double calc_0_ (std::shared_ptr<instr_nClass> instr) {
        return( (static_cast<Derived*>(this)->*Method)(instr->lens) ); // lens serves as a dummy here, to have the right type to pass
    }
    // Values in `idx` will be 1-based, with subtraction to 0-based occurring in generated decl functions.
    // For efficiency we might eventually reconsider that.
    template<auto Method>
    double calc_1_seq_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        if(len < 1) return(0);
        int iStart = instr->values->operator[](0)[0];
        int iEnd = iStart + len;
        Eigen::Tensor<int, 1> idx(1);
        double logProb(0.);
        for(int i = iStart; i < iEnd; ++i) {
            idx[0] = i;
            logProb += (static_cast<Derived*>(this)->*Method)(idx);  // `idx` must be a tensor, not a scalar.
        }
        return(logProb);
    }
    template<auto Method>
    double calc_1_mat_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        const auto& vals = instr->values->operator[](0);
        if(len != vals.size()) std::cout<<"len != vals.size() in calc_1_mat_"<<std::endl;
        if(len < 1) return(0);
        Eigen::Tensor<int, 1> idx(1);
        double logProb(0.);
        for(int i = 0; i < len; ++i) {
            idx[0] = vals[i];
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
        }
        return(logProb);
    }
    template<auto Method>
    double calc_1_matp_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        int nDim = instr->nDim;
        const auto& vals = instr->values->operator[](0);
        if(len*nDim != vals.size()) std::cout<<"len*nDim != vals.size() in calc_1_matp_"<<std::endl;
        if(len < 1) return(0);
        Eigen::Tensor<int, 1> idx(nDim);
        double logProb(0.);
        for(int i = 0; i < len; ++i) {
            for(int p = 0; p < nDim; ++p)
                idx[p] = vals[i*nDim+p];
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
        }
        return(logProb);
    }
    template<auto Method>
    double calc_1_matp_ord_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        int nDim = instr->nDim;
        const auto& vals = instr->values->operator[](0);
        if(len*nDim != vals.size()) std::cout<<"len*nDim != vals.size() in calc_1_matp_"<<std::endl;
        if(len < 1) return(0);
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = 0; i < len; ++i) {
            for(int p = 0; p < nDim; ++p)
              idx[slots[p]] = vals[i*nDim+p];
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_seq_seq_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        Eigen::Tensor<int, 1> idx(2);
        double logProb(0.);
        for(int i = iStart1; i < iEnd1; ++i) {
          idx[0] = i;
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[1] = j;
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_seq_mat_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals2 = instr->values->operator[](1);
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in calc_2_seq_mat_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        Eigen::Tensor<int, 1> idx(2);
        double logProb(0.);
        for(int i = iStart1; i < iEnd1; ++i) {
          idx[0] = i;
          for(int j = 0; j < len2; ++j) {
            idx[1] = vals2[j];
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_mat_seq_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in calc_2_mat_seq_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        Eigen::Tensor<int, 1> idx(2);
        double logProb(0.);
        for(int i = 0; i < len1; ++i) {
          idx[0] = vals1[i];
            for(int j = iStart2; j < iEnd2; ++j) {
              idx[1] = j;
              logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_mat_mat_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in calc_2_mat_mat_"<<std::endl;
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in calc_2_mat_mat_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        Eigen::Tensor<int, 1> idx(2);
        double logProb(0.);
        for(int i = 0; i < len1; ++i) {
          idx[0] = vals1[i];
            for(int j = 0; j < len2; ++j) {
              idx[1] = vals2[j];
              logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_seq_matp_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim2 = instr->dims[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals2 = instr->values->operator[](1);
        if(len2*dim2 != vals2.size()) std::cout<<"len2*dim2 != vals2.size() in calc_2_seq_matp_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = iStart1; i < iEnd1; ++i) {
          // TODO: because of reordering in nodeRange determination, we should be able
          // to assume that slots[0] is 1 and slots[1:(nDim-1)] is 2:nDim.
          idx[slots[0]] = i;
          for(int j = 0; j < len2; ++j) {
            for(int p = 0; p < dim2; ++p)
              idx[slots[p+1]] = vals2[j*dim2+p];
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_matp_seq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim1 = instr->dims[0];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        if(len1*dim1 != vals1.size()) std::cout<<"len1*dim1 != vals1.size() in calc_2_matp_seq_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = 0; i < len1; ++i) {
          for(int p = 0; p < dim1; ++p)
            idx[slots[p]] = vals1[i*dim1+p];
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[slots[dim1]] = j;
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_matp_matp_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim1 = instr->dims[0];
        int dim2 = instr->dims[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        if(len1*dim1 != vals1.size()) std::cout<<"len1*dim1 != vals1.size() in calc_2_matp_matp_"<<std::endl;
        if(len2*dim2 != vals2.size()) std::cout<<"len2*dim2 != vals2.size() in calc_2_matp_matp_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = 0; i < len1; ++i) {
            for(int p = 0; p < dim1; ++p)
              idx[slots[p]] = vals1[i*dim1+p];
            for(int j = 0; j < len2; ++j) {
              for(int p = 0; p < dim2; ++p)
                idx[slots[p+dim1]] = vals2[j*dim2+p];
              logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    // 2 indexRanges, single slot; assumes need to reorder.
    template<auto Method>
    double calc_2_x_y_ord_(std::shared_ptr<instr_nClass> instr) {
        if(instr->slots[0] != 2 || instr->slots[1] != 1)
          std::cout<<"slots not equal to 2,1 in calc_2_seq_seq_ord_"<<std::endl;
        int dim1 = instr->dims[0];
        int dim2 = instr->dims[1];
        int index_type1 = instr->index_types[0];
        int index_type2 = instr->index_types[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in calc_2_x_y_ord_"<<std::endl;
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in calc_2_x_y_ord_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        // Some of the ranges might be seq ranges:
        int iStart1 = instr->values->operator[](0)[0];
        int iStart2 = instr->values->operator[](1)[0];
        Eigen::Tensor<int, 1> idx(2);
        double logProb(0.);
        // We could manually write out the four cases (seq-mat, seq-seq, mat-seq, mat-mat) to avoid
        // conditionals inside of loops.
        for(int i = 0; i < len1; ++i) {
          if(index_type1 == 1) {
            idx[1] = iStart1 + i;
          } else idx[1] = vals1[i];
          for(int j = 0; j < len2; ++j) {
            if(index_type2 == 1) {
              idx[0] = iStart2 + j;
            } else idx[0] = vals2[j];
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_3_allseq_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        int len3 = instr->lens[2];
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        if(len3 < 1) return(0);
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        int iStart3 = instr->values->operator[](2)[0];
        int iEnd3 = iStart3 + len3;
        Eigen::Tensor<int, 1> idx(3);
        double logProb(0.);
        for(int i = iStart1; i < iEnd1; ++i) {
          idx[0] = i;
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[1] = j;
            for(int k = iStart3; k < iEnd3; ++k) {
              idx[2] = k;
              logProb += (static_cast<Derived*>(this)->*Method)(idx);
            }
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_3_generic_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim1 = instr->dims[0];
        int dim2 = instr->dims[1];
        int dim3 = instr->dims[2];
        int cumdim2 = dim1+dim2;
        int index_type1 = instr->index_types[0];
        int index_type2 = instr->index_types[1];
        int index_type3 = instr->index_types[2];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        int len3 = instr->lens[2];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        const auto& vals3 = instr->values->operator[](2);
        if(index_type1 == 2 && len1*dim1 != vals1.size()) std::cout<<"len1*dim1 != vals1.size() in calc_3_generic_"<<std::endl;
        if(index_type2 == 2 && len2*dim2 != vals2.size()) std::cout<<"len2*dim2 != vals2.size() in calc_3_generic_"<<std::endl;
        if(index_type3 == 2 && len3*dim3 != vals3.size()) std::cout<<"len3*dim3 != vals3.size() in calc_3_generic_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        if(len3 < 1) return(0);
        // Some of the ranges might be seq ranges:
        int iStart1 = instr->values->operator[](0)[0];
        int iStart2 = instr->values->operator[](1)[0];
        int iStart3 = instr->values->operator[](2)[0];
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = 0; i < len1; ++i) {
          if(index_type1 == 1) {
            idx[slots[0]] = iStart1 + i;
          } else
            for(int p = 0; p < dim1; ++p)
              idx[slots[p]] = vals1[i*dim1+p];
          for(int j = 0; j < len2; ++j) {
            if(index_type2 == 1) {
              idx[slots[dim1]] = iStart2 + j;
            } else
              for(int p = 0; p < dim2; ++p)
                idx[slots[p+dim1]] = vals2[j*dim2+p];
            for(int k = 0; k < len3; ++k) {
              if(index_type3 == 1) {
                idx[slots[cumdim2]] = iStart3 + k;
              } else
                for(int p = 0; p < dim3; ++p)
                  idx[slots[p+cumdim2]] = vals3[k*dim3+p];
              logProb += (static_cast<Derived*>(this)->*Method)(idx);
            }
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_4_allseq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> lens(4);
        Eigen::Tensor<int, 1> iStart(4);
        Eigen::Tensor<int, 1> iEnd(4);
        for(int p = 0; p < 4; ++p) {
          lens[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + lens[p];
          if(lens[p] < 1) return(0);
        }
        Eigen::Tensor<int, 1> idx(4);
        double logProb(0.);
        for(int i0 = iStart[0]; i0 < iEnd[0]; ++i0) {
          idx[0] = i0;
          for(int i1 = iStart[1]; i1 < iEnd[1]; ++i1) {
            idx[1] = i1;
            for(int i2 = iStart[2]; i2 < iEnd[2]; ++i2) {
              idx[2] = i2;
              for(int i3 = iStart[3]; i3 < iEnd[3]; ++i3) {
                idx[3] = i3;
                logProb += (static_cast<Derived*>(this)->*Method)(idx);
              }
            }
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_4_generic_(std::shared_ptr<instr_nClass> instr) {
      int nDim = instr->nDim;
      Eigen::Tensor<int, 1> lens(4);
      // Some of the indexRanges might be seq ranges.
      Eigen::Tensor<int, 1> iStart(4);
      Eigen::Tensor<int, 1> index_types(4);
      Eigen::Tensor<int, 1> dims(4);
      for(int p = 0; p < 4; ++p) {
        lens[p] = instr->lens[p];
        iStart[p] = instr->values->operator[](p)[0];
        index_types[p] = instr->index_types[p];
        dims[p] = instr->dims[p];
        if(lens[p] < 1) return(0);
      }
      Eigen::Tensor<int, 1> slots(nDim);
      for(int p = 0; p < nDim; ++p) {
        slots[p] = instr->slots[p]-1;
      }
      int cumdim1 = dims[0]+dims[1];
      int cumdim2 = dims[0]+dims[1]+dims[2];
      const auto& vals0 = instr->values->operator[](0);
      const auto& vals1 = instr->values->operator[](1);
      const auto& vals2 = instr->values->operator[](2);
      const auto& vals3 = instr->values->operator[](3);
      if(index_types[0] == 2 && lens[0]*dims[0] != vals0.size()) std::cout<<"lens[0]*dims[0] != vals0.size() in calc_4_generic_"<<std::endl;
      if(index_types[1] == 2 && lens[1]*dims[1] != vals1.size()) std::cout<<"lens[1]*dims[1] != vals1.size() in calc_4_generic_"<<std::endl;
      if(index_types[2] == 2 && lens[2]*dims[2] != vals2.size()) std::cout<<"lens[2]*dims[2] != vals2.size() in calc_4_generic_"<<std::endl;
      if(index_types[3] == 2 && lens[3]*dims[3] != vals3.size()) std::cout<<"lens[3]*dims[3] != vals3.size() in calc_4_generic_"<<std::endl;

      Eigen::Tensor<int, 1> idx(nDim);
      double logProb(0.);
      for(int i0 = 0; i0 < lens[0]; ++i0) {
        if(index_types[0] == 1) {
          idx[slots[0]] = iStart[0] + i0;
        } else
          for(int p = 0; p < dims[0]; ++p)
            idx[slots[p]] = vals0[i0*dims[0]+p];
        for(int i1 = 0; i1 < lens[1]; ++i1) {
          if(index_types[1] == 1) {
            idx[slots[dims[0]]] = iStart[1] + i1;
          } else
            for(int p = 0; p < dims[1]; ++p)
              idx[slots[p+dims[0]]] = vals1[i1*dims[1]+p];
          for(int i2 = 0; i2 < lens[2]; ++i2) {
            if(index_types[2] == 1) {
              idx[slots[cumdim1]] = iStart[2] + i2;
            } else
              for(int p = 0; p < dims[2]; ++p)
                idx[slots[p+cumdim1]] = vals2[i2*dims[2]+p];
            for(int i3 = 0; i3 < lens[3]; ++i3) {
              if(index_types[3] == 1) {
                idx[slots[cumdim2]] = iStart[3] + i3;
              } else
                for(int p = 0; p < dims[3]; ++p)
                  idx[slots[p+cumdim2]] = vals3[i3*dims[3]+p];
              logProb += (static_cast<Derived*>(this)->*Method)(idx);
            }
          }
        }
      }
      return(logProb);
    }
    template<auto Method>
    double calc_5_allseq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> lens(5);
        Eigen::Tensor<int, 1> iStart(5);
        Eigen::Tensor<int, 1> iEnd(5);
        for(int p = 0; p < 5; ++p) {
          lens[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + lens[p];
          if(lens[p] < 1) return(0);
        }
        Eigen::Tensor<int, 1> idx(5);
        double logProb(0.);
        for(int i0 = iStart[0]; i0 < iEnd[0]; ++i0) {
          idx[0] = i0;
          for(int i1 = iStart[1]; i1 < iEnd[1]; ++i1) {
            idx[1] = i1;
            for(int i2 = iStart[2]; i2 < iEnd[2]; ++i2) {
              idx[2] = i2;
              for(int i3 = iStart[3]; i3 < iEnd[3]; ++i3) {
                idx[3] = i3;
                for(int i4 = iStart[4]; i4 < iEnd[4]; ++i4) {
                  idx[4] = i4;
                  logProb += (static_cast<Derived*>(this)->*Method)(idx);
                }
              }
            }
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_5_generic_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> lens(5);
        // Some of the indexRanges might be seq ranges.
        Eigen::Tensor<int, 1> iStart(5);
        Eigen::Tensor<int, 1> index_types(5);
        Eigen::Tensor<int, 1> dims(5);
        for(int p = 0; p < 5; ++p) {
          lens[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          index_types[p] = instr->index_types[p];
          dims[p] = instr->dims[p];
          if(lens[p] < 1) return(0);
        }
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p) {
          slots[p] = instr->slots[p]-1;
        }
        int cumdim1 = dims[0]+dims[1];
        int cumdim2 = dims[0]+dims[1]+dims[2];
        int cumdim3 = dims[0]+dims[1]+dims[2]+dims[3];
        const auto& vals0 = instr->values->operator[](0);
        const auto& vals1 = instr->values->operator[](1);
        const auto& vals2 = instr->values->operator[](2);
        const auto& vals3 = instr->values->operator[](3);
        const auto& vals4 = instr->values->operator[](4);
        if(index_types[0] == 2 && lens[0]*dims[0] != vals0.size()) std::cout<<"lens[0]*dims[0] != vals0.size() in calc_5_generic_"<<std::endl;
        if(index_types[1] == 2 && lens[1]*dims[1] != vals1.size()) std::cout<<"lens[1]*dims[1] != vals1.size() in calc_5_generic_"<<std::endl;
        if(index_types[2] == 2 && lens[2]*dims[2] != vals2.size()) std::cout<<"lens[2]*dims[2] != vals2.size() in calc_5_generic_"<<std::endl;
        if(index_types[3] == 2 && lens[3]*dims[3] != vals3.size()) std::cout<<"lens[3]*dims[3] != vals3.size() in calc_5_generic_"<<std::endl;
        if(index_types[4] == 2 && lens[4]*dims[4] != vals4.size()) std::cout<<"lens[4]*dims[4] != vals4.size() in calc_5_generic_"<<std::endl;

        Eigen::Tensor<int, 1> idx(nDim);
         double logProb(0.);
         for(int i0 = 0; i0 < lens[0]; ++i0) {
           if(index_types[0] == 1) {
             idx[slots[0]] = iStart[0] + i0;
           } else
             for(int p = 0; p < dims[0]; ++p)
               idx[slots[p]] = vals0[i0*dims[0]+p];
           for(int i1 = 0; i1 < lens[1]; ++i1) {
             if(index_types[1] == 1) {
               idx[slots[dims[0]]] = iStart[1] + i1;
             } else
               for(int p = 0; p < dims[1]; ++p)
                 idx[slots[p+dims[0]]] = vals1[i1*dims[1]+p];
             for(int i2 = 0; i2 < lens[2]; ++i2) {
               if(index_types[2] == 1) {
                 idx[slots[cumdim1]] = iStart[2] + i2;
               } else
                 for(int p = 0; p < dims[2]; ++p)
                   idx[slots[p+cumdim1]] = vals2[i2*dims[2]+p];
               for(int i3 = 0; i3 < lens[3]; ++i3) {
                 if(index_types[3] == 1) {
                   idx[slots[cumdim2]] = iStart[3] + i3;
                 } else
                   for(int p = 0; p < dims[3]; ++p)
                     idx[slots[p+cumdim2]] = vals3[i3*dims[3]+p];
                 for(int i4 = 0; i4 < lens[4]; ++i4) {
                   if(index_types[4] == 1) {
                     idx[slots[cumdim3]] = iStart[4] + i4;
                   } else
                     for(int p = 0; p < dims[4]; ++p)
                       idx[slots[p+cumdim3]] = vals4[i4*dims[4]+p];
                   logProb += (static_cast<Derived*>(this)->*Method)(idx);
                 }
               }
             }
           }
         }
         return(logProb);
    }

    // getParam
    //
    // This is a bit simpler because we only need the first index.
        // Unpacks the first node from `instr`, warns (does not error) if `instr`
    // actually represents more than one node, and dispatches to the
    // per-distribution generated `getParam_one(idx, paramID)`.
    int getParamID_cpp(std::string param) override {
        RESET_EIGEN_ERRORS;
        return static_cast<Derived*>(this)->getParamID_one(param);
    }
    std::unique_ptr<ETaccessorBase> getParam_cpp(std::shared_ptr<instr_nClass> instr, int paramID) override {
        RESET_EIGEN_ERRORS;
        bool multiple = false;
        Eigen::Tensor<int, 1> idx = first_idx_(instr, multiple);
        if (multiple) {
            Rcpp::warning("getParam: instr covers more than one node; using the first node only.");
        }
        return static_cast<Derived*>(this)->getParam_one(idx, paramID);
    }
    double getBound_cpp(std::shared_ptr<instr_nClass> instr, int boundID) override {
        RESET_EIGEN_ERRORS;
        bool multiple = false;
        Eigen::Tensor<int, 1> idx = first_idx_(instr, multiple);
        if (multiple) {
            Rcpp::warning("getBound: instr covers more than one node; using the first node only.");
        }
        return static_cast<Derived*>(this)->getBound_one(idx, boundID);
    }
    Eigen::Tensor<int, 1> first_idx_(std::shared_ptr<instr_nClass> instr, bool &multiple) {
        int nDim = instr->nDim;
        switch (instr->instr_type) {
        case 0: {
            // Dummy/unindexed node: `calc_0_` passes `instr->lens` through as a
            // placeholder idx that generated code for an unindexed decl ignores.
            multiple = false;
            return instr->lens;
        }
        case 1: case 2: { // 1_seq, 1_mat: single scalar index.
            multiple = instr->lens[0] > 1;
            Eigen::Tensor<int, 1> idx(1);
            idx[0] = instr->values->operator[](0)[0];
            return idx;
        }
        case 3: { // 1_matp: single range, nDim components, no slot reordering.
            multiple = instr->lens[0] > 1;
            Eigen::Tensor<int, 1> idx(nDim);
            const auto& vals = instr->values->operator[](0);
            for (int p = 0; p < nDim; ++p) idx[p] = vals[p];
            return idx;
        }
        case 18: { // 1_matp_ord: single range, nDim components, slot-placed.
            multiple = instr->lens[0] > 1;
            Eigen::Tensor<int, 1> idx(nDim);
            Eigen::Tensor<int, 1> slots(nDim);
            for (int p = 0; p < nDim; ++p) slots[p] = instr->slots[p] - 1;
            const auto& vals = instr->values->operator[](0);
            for (int p = 0; p < nDim; ++p) idx[slots[p]] = vals[p];
            return idx;
        }
        case 4: case 5: case 6: case 7: { // 2_{seq,mat}_{seq,mat}: two scalar indices, no slots.
            multiple = (instr->lens[0] * instr->lens[1]) > 1;
            Eigen::Tensor<int, 1> idx(2);
            idx[0] = instr->values->operator[](0)[0];
            idx[1] = instr->values->operator[](1)[0];
            return idx;
        }
        case 8: { // 2_seq_matp: range0 is a single seq slot; range1 is dims[1] components.
            multiple = (instr->lens[0] * instr->lens[1]) > 1;
            int dim2 = instr->dims[1];
            Eigen::Tensor<int, 1> idx(nDim);
            Eigen::Tensor<int, 1> slots(nDim);
            for (int p = 0; p < nDim; ++p) slots[p] = instr->slots[p] - 1;
            idx[slots[0]] = instr->values->operator[](0)[0];
            const auto& vals2 = instr->values->operator[](1);
            for (int p = 0; p < dim2; ++p) idx[slots[p + 1]] = vals2[p];
            return idx;
        }
        case 9: { // 2_matp_seq: range0 is dims[0] components; range1 is a single seq slot.
            multiple = (instr->lens[0] * instr->lens[1]) > 1;
            int dim1 = instr->dims[0];
            Eigen::Tensor<int, 1> idx(nDim);
            Eigen::Tensor<int, 1> slots(nDim);
            for (int p = 0; p < nDim; ++p) slots[p] = instr->slots[p] - 1;
            const auto& vals1 = instr->values->operator[](0);
            for (int p = 0; p < dim1; ++p) idx[slots[p]] = vals1[p];
            idx[slots[dim1]] = instr->values->operator[](1)[0];
            return idx;
        }
        case 10: { // 2_matp_matp: both ranges are multi-component, slot-placed.
            multiple = (instr->lens[0] * instr->lens[1]) > 1;
            int dim1 = instr->dims[0];
            int dim2 = instr->dims[1];
            Eigen::Tensor<int, 1> idx(nDim);
            Eigen::Tensor<int, 1> slots(nDim);
            for (int p = 0; p < nDim; ++p) slots[p] = instr->slots[p] - 1;
            const auto& vals1 = instr->values->operator[](0);
            const auto& vals2 = instr->values->operator[](1);
            for (int p = 0; p < dim1; ++p) idx[slots[p]] = vals1[p];
            for (int p = 0; p < dim2; ++p) idx[slots[p + dim1]] = vals2[p];
            return idx;
        }
        case 11: { // 2_x_y_ord: hardcoded swap (idx[1] from range0, idx[0] from range1).
            // At the first tuple, iStart == values[p][0] regardless of index_type,
            // so no need to branch on index_types here.
            multiple = (instr->lens[0] * instr->lens[1]) > 1;
            Eigen::Tensor<int, 1> idx(2);
            idx[1] = instr->values->operator[](0)[0];
            idx[0] = instr->values->operator[](1)[0];
            return idx;
        }
        case 12: { // 3_allseq: three scalar indices, no slots.
            multiple = (instr->lens[0] * instr->lens[1] * instr->lens[2]) > 1;
            Eigen::Tensor<int, 1> idx(3);
            for (int r = 0; r < 3; ++r) idx[r] = instr->values->operator[](r)[0];
            return idx;
        }
        case 13: { // 3_generic: slot-placed, each range either 1 (seq) or dims[r] components.
            multiple = (instr->lens[0] * instr->lens[1] * instr->lens[2]) > 1;
            return first_idx_generic_(instr, 3, multiple);
        }
        case 14: { // 4_allseq: four scalar indices, no slots.
            multiple = (instr->lens[0] * instr->lens[1] * instr->lens[2] * instr->lens[3]) > 1;
            Eigen::Tensor<int, 1> idx(4);
            for (int r = 0; r < 4; ++r) idx[r] = instr->values->operator[](r)[0];
            return idx;
        }
        case 15: { // 4_generic
            multiple = (instr->lens[0] * instr->lens[1] * instr->lens[2] * instr->lens[3]) > 1;
            return first_idx_generic_(instr, 4, multiple);
        }
        case 16: { // 5_allseq: five scalar indices, no slots.
            multiple = (instr->lens[0] * instr->lens[1] * instr->lens[2] * instr->lens[3] * instr->lens[4]) > 1;
            Eigen::Tensor<int, 1> idx(5);
            for (int r = 0; r < 5; ++r) idx[r] = instr->values->operator[](r)[0];
            return idx;
        }
        case 17: { // 5_generic
            multiple = (instr->lens[0] * instr->lens[1] * instr->lens[2] * instr->lens[3] * instr->lens[4]) > 1;
            return first_idx_generic_(instr, 5, multiple);
        }
        default:
            Rcpp::stop("getParam or getBound: unrecognized instr type.");
        }
        return instr->lens; // unreachable; keeps compiler happy.
    }

    // Shared "generic" (slot-placed, mixed seq/matp per range) first-tuple logic
    // for instr types 13, 15, 17, parameterized by the number of ranges `nRanges`.
    // Mirrors calc_3_generic_/calc_4_generic_/calc_5_generic_ at their first
    // (i=j=k=...=0) iteration.
    Eigen::Tensor<int, 1> first_idx_generic_(std::shared_ptr<instr_nClass> instr, int nRanges, bool &multiple) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for (int p = 0; p < nDim; ++p) slots[p] = instr->slots[p] - 1;
        int offset = 0;
        for (int r = 0; r < nRanges; ++r) {
            const auto& vals = instr->values->operator[](r);
            if (instr->index_types[r] == 1) {
                idx[slots[offset]] = vals[0];
            } else {
                for (int p = 0; p < instr->dims[r]; ++p) idx[slots[offset + p]] = vals[p];
            }
            offset += instr->dims[r];
        }
        return idx;
    }

    virtual ~declFunClass_() {};
};
