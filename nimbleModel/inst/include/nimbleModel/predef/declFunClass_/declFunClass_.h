// to be included from the predefined nodeFxnBase_nClass.
// Add "#include <nCompiler/predef/nodeFxnClass_/nodeFxnClass_.h>" to that file,
// after the declaration of nodeFxnBase_nClass.

template<class Derived>
class declFunClass_ : public declFunBase_nClass {
public:
    double v;
    declFunClass_() {};

    double calculate_cpp( std::shared_ptr<instr_nClass> instr) override {
        return calc_op_< &Derived::calc_one >(instr);
    }
    double calculateDiff_cpp( std::shared_ptr<instr_nClass> instr) override {
        return calc_op_< &Derived::calcDiff_one >(instr);
    }
    double getLogProb_cpp( std::shared_ptr<instr_nClass> instr) override {
        return calc_op_< &Derived::getLogProb_one >(instr);
    }
    template<auto Method>
    double  calc_op_ ( std::shared_ptr<instr_nClass> instr ) {
        RESET_EIGEN_ERRORS;
        int instr_type = instr->type;
        if(instr_type == 0) return calc_0_< Method >(instr);
        if(instr_type == 1) return calc_1_seq_< Method >(instr);
        if(instr_type == 2) return calc_1_mat_< Method >(instr);
        if(instr_type == 3) return calc_1_matp_< Method >(instr);
        if(instr_type == 4) return calc_2_seq_seq_< Method >(instr);
        if(instr_type == 5) return calc_2_seq_mat_< Method >(instr);
        if(instr_type == 6) return calc_2_mat_seq_< Method >(instr);
        if(instr_type == 7) return calc_2_mat_mat_< Method >(instr);
        if(instr_type == 8) return calc_2_seq_matp_< Method >(instr);
        if(instr_type == 9) return calc_2_matp_seq_< Method >(instr);
        if(instr_type == 10) return calc_2_matp_matp_< Method >(instr);
        if(instr_type == 11) return calc_2_x_y_ord_< Method >(instr);
        if(instr_type == 12) return calc_3_allseq_< Method >(instr);
        if(instr_type == 13) return calc_3_generic_< Method >(instr);
        if(instr_type == 14) return calc_4_allseq_< Method >(instr);
        if(instr_type == 15) return calc_4_generic_< Method >(instr);
        if(instr_type == 16) return calc_5_allseq_< Method >(instr);
        if(instr_type == 17) return calc_5_generic_< Method >(instr);
        if(instr_type == 18) return calc_1_matp_ord_< Method >(instr);
        // TODO: we should probably error out if no method found.
        return(0);
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
  
    // simulate
    void  simulate_cpp ( std::shared_ptr<instr_nClass> instr ) {
        RESET_EIGEN_ERRORS;
        int instr_type = instr->type;
        if(instr_type == 0) return sim_0_(instr);
        if(instr_type == 1) return sim_1_seq_(instr);
        if(instr_type == 2) return sim_1_mat_(instr);
        if(instr_type == 3) return sim_1_matp_(instr);
        if(instr_type == 4) return sim_2_seq_seq_(instr);
        if(instr_type == 5) return sim_2_seq_mat_(instr);
        if(instr_type == 6) return sim_2_mat_seq_(instr);
        if(instr_type == 7) return sim_2_mat_mat_(instr);
        if(instr_type == 8) return sim_2_seq_matp_(instr);
        if(instr_type == 9) return sim_2_matp_seq_(instr);
        if(instr_type == 10) return sim_2_matp_matp_(instr);
        if(instr_type == 11) return sim_2_x_y_ord_(instr);
        if(instr_type == 12) return sim_3_allseq_(instr);
        if(instr_type == 13) return sim_3_generic_(instr);
        if(instr_type == 14) return sim_4_allseq_(instr);
        if(instr_type == 15) return sim_4_generic_(instr);
        if(instr_type == 16) return sim_5_allseq_(instr);
        if(instr_type == 17) return sim_5_generic_(instr);
        if(instr_type == 18) return sim_1_matp_ord_(instr);
        // TODO: we should probably error out if no method found.
     }
    void sim_0_ (std::shared_ptr<instr_nClass> instr) {
       static_cast<Derived*>(this)->sim_one(instr->lens); // lens serves as a dummy here, to have the right type to pass
    }
    void sim_1_seq_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        if(len < 1) return;
        int iStart = instr->values->operator[](0)[0];
        int iEnd = iStart + len;
        Eigen::Tensor<int, 1> idx(1);
        for(int i = iStart; i < iEnd; ++i) {
            idx[0] = i;
            static_cast<Derived*>(this)->sim_one(idx);
        }
    }
    void sim_1_mat_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        const auto& vals = instr->values->operator[](0);
        if(len != vals.size()) std::cout<<"len != vals.size() in sim_1_mat_"<<std::endl;
        if(len < 1) return;
        Eigen::Tensor<int, 1> idx(1);
        for(int i = 0; i < len; ++i) {
            idx[0] = vals[i];
            static_cast<Derived*>(this)->sim_one(idx);
        }
    }
    void sim_1_matp_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        int nDim = instr->nDim;
        const auto& vals = instr->values->operator[](0);
        if(len*nDim != vals.size()) std::cout<<"len*nDim != vals.size() in sim_1_matp_"<<std::endl;
        if(len < 1) return;
        Eigen::Tensor<int, 1> idx(nDim);
        for(int i = 0; i < len; ++i) {
          for(int p = 0; p < nDim; ++p)
            idx[p] = vals[i*nDim+p];
          static_cast<Derived*>(this)->sim_one(idx);
        }
    }
    void sim_1_matp_ord_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        int nDim = instr->nDim;
        const auto& vals = instr->values->operator[](0);
        if(len*nDim != vals.size()) std::cout<<"len*nDim != vals.size() in sim_1_matp_"<<std::endl;
        if(len < 1) return;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        for(int i = 0; i < len; ++i) {
          for(int p = 0; p < nDim; ++p)
            idx[slots[p]] = vals[i*nDim+p];
          static_cast<Derived*>(this)->sim_one(idx);
        }
    }
    void sim_2_seq_seq_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        if(len1 < 1) return;
        if(len2 < 1) return;
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        Eigen::Tensor<int, 1> idx(2);
        for(int i = iStart1; i < iEnd1; ++i) {
          idx[0] = i;
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[1] = j;
            static_cast<Derived*>(this)->sim_one(idx);
          }
        }
    }
    void sim_2_seq_mat_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals2 = instr->values->operator[](1);
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in sim_2_seq_mat_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        Eigen::Tensor<int, 1> idx(2);
        for(int i = iStart1; i < iEnd1; ++i) {
          idx[0] = i;
          for(int j = 0; j < len2; ++j) {
            idx[1] = vals2[j];
            static_cast<Derived*>(this)->sim_one(idx);
          }
        }
    }
    void sim_2_mat_seq_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in sim_2_mat_seq_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        Eigen::Tensor<int, 1> idx(2);
        for(int i = 0; i < len1; ++i) {
          idx[0] = vals1[i];
            for(int j = iStart2; j < iEnd2; ++j) {
              idx[1] = j;
              static_cast<Derived*>(this)->sim_one(idx);
          }
        }
    }
    void sim_2_mat_mat_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in sim_2_mat_mat_"<<std::endl;
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in sim_2_mat_mat_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        Eigen::Tensor<int, 1> idx(2);
        for(int i = 0; i < len1; ++i) {
          idx[0] = vals1[i];
            for(int j = 0; j < len2; ++j) {
              idx[1] = vals2[j];
              static_cast<Derived*>(this)->sim_one(idx);
          }
        }
    }
    void sim_2_seq_matp_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim2 = instr->dims[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals2 = instr->values->operator[](1);
        if(len2*dim2 != vals2.size()) std::cout<<"len2*dim2 != vals2.size() in sim_2_seq_matp_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        for(int i = iStart1; i < iEnd1; ++i) {
          // TODO: because of reordering in nodeRange determination, we should be able
          // to assume that slots[0] is 1 and slots[1:(nDim-1)] is 2:nDim.
          idx[slots[0]] = i;
          for(int j = 0; j < len2; ++j) {
            for(int p = 0; p < dim2; ++p)
              idx[slots[p+1]] = vals2[j*dim2+p];  
            static_cast<Derived*>(this)->sim_one(idx); 
          }
        }
    }
    void sim_2_matp_seq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim1 = instr->dims[0];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        if(len1*dim1 != vals1.size()) std::cout<<"len1*dim1 != vals1.size() in sim_2_matp_seq_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        for(int i = 0; i < len1; ++i) {
          for(int p = 0; p < dim1; ++p)
            idx[slots[p]] = vals1[i*dim1+p];
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[slots[dim1]] = j;
            static_cast<Derived*>(this)->sim_one(idx); 
          }
        }
    }
    void sim_2_matp_matp_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim1 = instr->dims[0];
        int dim2 = instr->dims[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        if(len1*dim1 != vals1.size()) std::cout<<"len1*dim1 != vals1.size() in sim_2_matp_matp_"<<std::endl;
        if(len2*dim2 != vals2.size()) std::cout<<"len2*dim2 != vals2.size() in sim_2_matp_matp_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        for(int i = 0; i < len1; ++i) {
            for(int p = 0; p < dim1; ++p)
              idx[slots[p]] = vals1[i*dim1+p];  
            for(int j = 0; j < len2; ++j) {
              for(int p = 0; p < dim2; ++p)
                idx[slots[p+dim1]] = vals2[j*dim2+p];  
              static_cast<Derived*>(this)->sim_one(idx); 
          }
        }
    }
    // 2 indexRanges, single slot; assumes need to reorder.
    void sim_2_x_y_ord_(std::shared_ptr<instr_nClass> instr) {
        if(instr->slots[0] != 2 || instr->slots[1] != 1)
          std::cout<<"slots not equal to 2,1 in sim_2_seq_seq_ord_"<<std::endl;
        int dim1 = instr->dims[0];
        int dim2 = instr->dims[1];
        int index_type1 = instr->index_types[0];
        int index_type2 = instr->index_types[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in sim_3_generic_"<<std::endl;
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in sim_3_generic_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        // Some of the ranges might be seq ranges:
        int iStart1 = instr->values->operator[](0)[0];
        int iStart2 = instr->values->operator[](1)[0];
        Eigen::Tensor<int, 1> idx(2);
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
            static_cast<Derived*>(this)->sim_one(idx); 
          }
        }
    }
    void sim_3_allseq_(std::shared_ptr<instr_nClass> instr) {
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        int len3 = instr->lens[2];
        if(len1 < 1) return;
        if(len2 < 1) return;
        if(len3 < 1) return;
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        int iStart3 = instr->values->operator[](2)[0];
        int iEnd3 = iStart3 + len3;
        Eigen::Tensor<int, 1> idx(3);
        for(int i = iStart1; i < iEnd1; ++i) {
          idx[0] = i;
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[1] = j;
            for(int k = iStart3; k < iEnd3; ++k) {
              idx[2] = k;
              static_cast<Derived*>(this)->sim_one(idx); 
            }
          }
        }
    }
    void sim_3_generic_(std::shared_ptr<instr_nClass> instr) {
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
        if(index_type1 == 2 && len1*dim1 != vals1.size()) std::cout<<"len1*dim1 != vals1.size() in sim_3_generic_"<<std::endl;
        if(index_type2 == 2 && len2*dim2 != vals2.size()) std::cout<<"len2*dim2 != vals2.size() in sim_3_generic_"<<std::endl;
        if(index_type3 == 2 && len3*dim3 != vals3.size()) std::cout<<"len3*dim3 != vals3.size() in sim_3_generic_"<<std::endl;
        if(len1 < 1) return;
        if(len2 < 1) return;
        if(len3 < 1) return;
        // Some of the ranges might be seq ranges:
        int iStart1 = instr->values->operator[](0)[0];
        int iStart2 = instr->values->operator[](1)[0];
        int iStart3 = instr->values->operator[](2)[0];
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
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
              static_cast<Derived*>(this)->sim_one(idx); 
            }
          }
        }
    }
    void sim_4_allseq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> lens(4);
        Eigen::Tensor<int, 1> iStart(4);
        Eigen::Tensor<int, 1> iEnd(4);
        for(int p = 0; p < 4; ++p) {
          lens[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + lens[p];
          if(lens[p] < 1) return;
        }
        Eigen::Tensor<int, 1> idx(4);
        for(int i0 = iStart[0]; i0 < iEnd[0]; ++i0) {
          idx[0] = i0;
          for(int i1 = iStart[1]; i1 < iEnd[1]; ++i1) {
            idx[1] = i1;
            for(int i2 = iStart[2]; i2 < iEnd[2]; ++i2) {
              idx[2] = i2;
              for(int i3 = iStart[3]; i3 < iEnd[3]; ++i3) {
                idx[3] = i3;
                static_cast<Derived*>(this)->sim_one(idx);
              }
            }
          }
        }
    }
    void sim_4_generic_(std::shared_ptr<instr_nClass> instr) {
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
        if(lens[p] < 1) return;
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
      if(index_types[0] == 2 && lens[0]*dims[0] != vals0.size()) std::cout<<"lens[0]*dims[0] != vals0.size() in sim_4_generic_"<<std::endl;
      if(index_types[1] == 2 && lens[1]*dims[1] != vals1.size()) std::cout<<"lens[1]*dims[1] != vals1.size() in sim_4_generic_"<<std::endl;
      if(index_types[2] == 2 && lens[2]*dims[2] != vals2.size()) std::cout<<"lens[2]*dims[2] != vals2.size() in sim_4_generic_"<<std::endl;
      if(index_types[3] == 2 && lens[3]*dims[3] != vals3.size()) std::cout<<"lens[3]*dims[3] != vals3.size() in sim_4_generic_"<<std::endl;
      
      Eigen::Tensor<int, 1> idx(nDim);
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
              static_cast<Derived*>(this)->sim_one(idx); 
            }
          }
        }
      }
    }
    void sim_5_allseq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> lens(5);
        Eigen::Tensor<int, 1> iStart(5);
        Eigen::Tensor<int, 1> iEnd(5);
        for(int p = 0; p < 5; ++p) {
          lens[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + lens[p];
          if(lens[p] < 1) return;
        }
        Eigen::Tensor<int, 1> idx(5);
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
                  static_cast<Derived*>(this)->sim_one(idx);
                }
              }
            }
          }
        }
    }
    void sim_5_generic_(std::shared_ptr<instr_nClass> instr) {
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
          if(lens[p] < 1) return;
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
        if(index_types[0] == 2 && lens[0]*dims[0] != vals0.size()) std::cout<<"lens[0]*dims[0] != vals0.size() in sim_5_generic_"<<std::endl;
        if(index_types[1] == 2 && lens[1]*dims[1] != vals1.size()) std::cout<<"lens[1]*dims[1] != vals1.size() in sim_5_generic_"<<std::endl;
        if(index_types[2] == 2 && lens[2]*dims[2] != vals2.size()) std::cout<<"lens[2]*dims[2] != vals2.size() in sim_5_generic_"<<std::endl;
        if(index_types[3] == 2 && lens[3]*dims[3] != vals3.size()) std::cout<<"lens[3]*dims[3] != vals3.size() in sim_5_generic_"<<std::endl;
        if(index_types[4] == 2 && lens[4]*dims[4] != vals4.size()) std::cout<<"lens[4]*dims[4] != vals4.size() in sim_5_generic_"<<std::endl;
        
        Eigen::Tensor<int, 1> idx(nDim);
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
                 for(int p = 0; p < dims[2];  ++p)
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
                   static_cast<Derived*>(this)->sim_one(idx); 
                 }
               }
             }
           }
         }
    }
    virtual ~declFunClass_() {};
};
