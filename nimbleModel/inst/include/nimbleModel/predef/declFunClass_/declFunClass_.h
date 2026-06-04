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
        if(instr_type == 4) return calc_1_matp_ord_< Method >(instr);
        if(instr_type == 5) return calc_2_seq_seq_< Method >(instr);
        if(instr_type == 6) return calc_2_seq_seq_ord_< Method >(instr);
        return(0);
    }
    template<auto Method>
    double calc_0_ (std::shared_ptr<instr_nClass> instr) {
        return( (static_cast<Derived*>(this)->*Method)(instr->lens) ); // lens serves as a dummy here, to have the right type to pass
    }
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
            logProb += (static_cast<Derived*>(this)->*Method)(idx);
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
    double calc_2_seq_seq_ord_(std::shared_ptr<instr_nClass> instr) {
        if(instr->slots[0] != 2 || instr->slots[1] != 1)
          std::cout<<"slots not equal to 2,1 in calc_2_seq_seq_ord_"<<std::endl;
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
          idx[1] = i;
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[0] = j;
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
        for(int i = 0; i < len1; ++j) {
          idx[0] = vals1[i]
            for(int j = iStart2; i < iEnd2; ++j) {
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
        for(int i = 0; i < len1; ++j) {
          idx[0] = vals1[i]
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
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals2 = instr->values->operator[](1);
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in calc_2_seq_matp_"<<std::endl;
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
          idx[slots[0]] = i;
          for(int j = 0; j < len2; ++j) {
            for(int p = 0; p < nDim; ++p)
              idx[slots[p+1]] = vals2[j*nDim+p];  
            logProb += (static_cast<Derived*>(this)->*Method)(idx); 
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_2_matp_matp_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        int dim1 = instr$dims[0];
        int dim2 = instr$dims[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in calc_2_matp_matp_"<<std::endl;
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in calc_2_matp_matp_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = 0; i < len1; ++j) {
            for(int p = 0; p < dim1; ++p)
              idx[slots[p]] = vals1[i*dim1+p];  
            for(int j = 0; j < len2; ++j) {
              for(int p = dim1; p < nDim; ++p)
                idx[slots[p]] = vals2[j*dim2+p];  
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
        int dims1 = instr$dims[0];
        int dims2 = instr$dims[1];
        int dims3 = instr$dims[2];
        int cumdims2 = dims1+dims2;
        int index_types1 = instr$index_typess[0];
        int index_types2 = instr$index_typess[1];
        int index_types3 = instr$index_typess[1];
        int len1 = instr->lens[0];
        int len2 = instr->lens[1];
        int len3 = instr->lens[2];
        const auto& vals1 = instr->values->operator[](0);
        const auto& vals2 = instr->values->operator[](1);
        const auto& vals3 = instr->values->operator[](2);
        if(len1 != vals1.size()) std::cout<<"len1 != vals1.size() in calc_3_generic_"<<std::endl;
        if(len2 != vals2.size()) std::cout<<"len2 != vals2.size() in calc_3_generic_"<<std::endl;
        if(len3 != vals3.size()) std::cout<<"len3 != vals3.size() in calc_3_generic_"<<std::endl;
        if(len1 < 1) return(0);
        if(len2 < 1) return(0);
        if(len3 < 1) return(0);
        // Some of the ranges might be seq ranges:
        int iStart1 = instr->values->operator[](0)[0];
        int iEnd1 = iStart1 + len1;
        int iStart2 = instr->values->operator[](1)[0];
        int iEnd2 = iStart2 + len2;
        int iStart3 = instr->values->operator[](2)[0];
        int iEnd3 = iStart3 + len3;
        Eigen::Tensor<int, 1> idx(nDim);
        Eigen::Tensor<int, 1> slots(nDim);
        for(int p = 0; p < nDim; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = 0; i < len1; ++i) {
          if(index_types1 == 1) {
            idx[slots[0]] = iStart1 + i;
          } else 
            for(int p = 0; p < dims1; ++p)
              idx[slots[p]] = vals1[i*dims1+p];  
          for(int j = 0; j < len2; ++j) {
            if(index_types2 == 1) {
              idx[slots[dims1]] = iStart2 + j;
            } else 
              for(int p = dims1; p < cumdims2; ++p)
                idx[slots[p]] = vals2[j*dims2+p];
            for(int k = 0; k < len3; ++k) {
              if(index_types3 == 1) {
                idx[slots[cumdims2]] = iStart3 + k;
              } else 
                for(int p = cumdims2; p < nDim; ++p)
                  idx[slots[p]] = vals3[k*dims3+p];
              logProb += (static_cast<Derived*>(this)->*Method)(idx); 
            }
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_4_allseq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> len(4);
        Eigen::Tensor<int, 1> iStart(4);
        Eigen::Tensor<int, 1> iEnd(4);
        for(int p = 0; p < nDim; ++p) {
          len[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + len[p];
          if(len[p] < 1) return(0);
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
        Eigen::Tensor<int, 1> len(4);
        // Some of the indexRanges might be seq ranges.
        Eigen::Tensor<int, 1> iStart(4);
        Eigen::Tensor<int, 1> iEnd(4);
        Eigen::Tensor<int, 1> index_types(4);
        Eigen::Tensor<int, 1> dims(4);
        for(int p = 0; p < nDim; ++p) {
          len[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + len[p];
          index_types[p] = instr$index_types[p];
          dims[p] = instr$dims[p];
          slots[p] = instr->slots[p]-1;
          if(len[p] < 1) return(0);
        }
        cumdims1 = dims[0]+dims[1];
        cumdims2 = dims[0]+dims[1]+dims[2];
        const auto& vals0 = instr->values->operator[](0);
        const auto& vals1 = instr->values->operator[](1);
        const auto& vals2 = instr->values->operator[](2);
        const auto& vals3 = instr->values->operator[](3);
        if(len[0] != vals0.size()) std::cout<<"len[0] != vals0.size() in calc_4_generic_"<<std::endl;
        if(len[1] != vals1.size()) std::cout<<"len[1] != vals1.size() in calc_4_generic_"<<std::endl;
        if(len[2] != vals2.size()) std::cout<<"len[2] != vals2.size() in calc_4_generic_"<<std::endl;
        if(len[3] != vals3.size()) std::cout<<"len[3] != vals3.size() in calc_4_generic_"<<std::endl;
        
        Eigen::Tensor<int, 1> idx(nDim);
         double logProb(0.);
         for(int i0 = 0; i0 < len[0]; ++i0) {
          if(index_types[0] == 1) {
            idx[slots[0]] = iStart[0] + i0;
          } else 
            for(int p = 0; p < dim[0]; ++p)
              idx[slots[p]] = vals0[i0*dim[0]+p];  
          for(int i1 = 0; i1 < len[1]; ++i1) {
            if(index_types[1] == 1) {
              idx[slots[dim[0]]] = iStart[1] + i1;
            } else 
              for(int p = dim[0]; p < cumdims1; ++p)
                idx[slots[p]] = vals1[i1*dim[1]+p];
            for(int i2 = 0; i2 < len[2]; ++i2) {
              if(index_types[2] == 1) {
                idx[slots[cumdims1]] = iStart[2] + i2;
              } else 
                for(int p = cumdims1; p < cumdims2; ++p)
                  idx[slots[p]] = vals2[i2*dim[2]+p];
              for(int i3 = 0; i3 < len[3]; ++i3) {
                if(index_types[3] == 1) {
                  idx[slot[cumdims2]] = iStart[3] + i3;
                } else 
                  for(int p = cumdims2; p < nDim; ++p)
                    idx[slots[p]] = vals3[i3*dim[3]+p];
                logProb += (static_cast<Derived*>(this)->*Method)(idx); 
            }
          }
        }
        return(logProb);
    }
    template<auto Method>
    double calc_5_allseq_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> len(5);
        Eigen::Tensor<int, 1> iStart(5);
        Eigen::Tensor<int, 1> iEnd(5);
        for(int p = 0; p < nDim; ++p) {
          len[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + len[p];
          if(len[p] < 1) return(0);
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
        return(logProb);
    }
    template<auto Method>
    double calc_5_generic_(std::shared_ptr<instr_nClass> instr) {
        int nDim = instr->nDim;
        Eigen::Tensor<int, 1> len(5);
        // Some of the indexRanges might be seq ranges.
        Eigen::Tensor<int, 1> iStart(5);
        Eigen::Tensor<int, 1> iEnd(5);
        Eigen::Tensor<int, 1> index_types(5);
        Eigen::Tensor<int, 1> dims(5);
        for(int p = 0; p < nDim; ++p) {
          len[p] = instr->lens[p];
          iStart[p] = instr->values->operator[](p)[0];
          iEnd[p] = iStart[p] + len[p];
          index_types[p] = instr$index_types[p];
          dims[p] = instr$dims[p];
          slots[p] = instr->slots[p]-1;
          if(len[p] < 1) return(0);
        }
        cumdims1 = dims[0]+dims[1];
        cumdims2 = dims[0]+dims[1]+dims[2];
        cumdims3 = dims[0]+dims[1]+dims[2]+dims[3];
        const auto& vals0 = instr->values->operator[](0);
        const auto& vals1 = instr->values->operator[](1);
        const auto& vals2 = instr->values->operator[](2);
        const auto& vals3 = instr->values->operator[](3);
        const auto& vals4 = instr->values->operator[](4);
        if(len[0] != vals0.size()) std::cout<<"len[0] != vals0.size() in calc_5_generic_"<<std::endl;
        if(len[1] != vals1.size()) std::cout<<"len[1] != vals1.size() in calc_5_generic_"<<std::endl;
        if(len[2] != vals2.size()) std::cout<<"len[2] != vals2.size() in calc_5_generic_"<<std::endl;
        if(len[3] != vals3.size()) std::cout<<"len[3] != vals3.size() in calc_5_generic_"<<std::endl;
        if(len[4] != vals4.size()) std::cout<<"len[4] != vals4.size() in calc_5_generic_"<<std::endl;
        
        Eigen::Tensor<int, 1> idx(nDim);
         double logProb(0.);
         for(int i0 = 0; i0 < len[0]; ++i0) {
          if(index_types[0] == 1) {
            idx[slots[0]] = iStart[0] + i0;
          } else 
            for(int p = 0; p < dims[0]; ++p)
              idx[slots[p]] = vals0[i0*dims[0]+p];  
          for(int i1 = 0; i1 < len[1]; ++i1) {
            if(index_types[1] == 1) {
              idx[slots[dims[0]]] = iStart[1] + i1;
            } else 
              for(int p = dims[0]; p < cumdims[1]; ++p)
                idx[slots[p]] = vals1[i1*dims[1]+p];
            for(int i2 = 0; i2 < len[2]; ++i2) {
              if(index_types[2] == 1) {
                idx[slots[cumdims[1]]] = iStart[2] + i2;
              } else 
                for(int p = cumdims[1]; p < cumdims2; ++p)
                  idx[slots[p]] = vals2[i2*dims[2]+p];
              for(int i3 = 0; i3 < len[3]; ++i3) {
                if(index_types[3] == 1) {
                  idx[slots[cumdims2]] = iStart[3] + i3;
                } else 
                  for(int p = cumdims2; p < cumdims3; ++p)
                    idx[slots[p]] = vals3[i3*dims[3]+p];
                for(int i4 = 0; i4 < len[4]; ++i4) {
                  if(index_types[4] == 1) {
                    idx[slots[cumdims3]] = iStart[4] + i4;
                  } else 
                    for(int p = cumdims3; p < nDim; ++p)
                      idx[slots[p]] = vals4[i4*dims[4]+p];
                  logProb += (static_cast<Derived*>(this)->*Method)(idx); 
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
        if(instr_type == 4) return sim_1_matp_ord_(instr);
        if(instr_type == 5) return sim_2_seq_seq_(instr);
        if(instr_type == 6) return sim_2_seq_seq_ord_(instr);
    }
    void sim_0_ (std::shared_ptr<instr_nClass> instr) {
       static_cast<Derived*>(this)->sim_one(instr->lens); // lens serves as a dummy here, to have the right type to pass
    }
    void sim_1_seq_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        if(len < 1) return;
        int iStart = instr->values->operator[](0)[0] + 1;
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
        int nDim = instr->dims[0];
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
        int nDim = instr->dims[0];
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
    void sim_2_seq_seq_ord_(std::shared_ptr<instr_nClass> instr) {
        if(instr->slots[0] != 2 || instr->slots[1] != 1)
          std::cout<<"slots not equal to 2,1 in calc_2_seq_seq_ord_"<<std::endl;
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
          idx[1] = i;
          for(int j = iStart2; j < iEnd2; ++j) {
            idx[0] = j;
            static_cast<Derived*>(this)->sim_one(idx);
          }
        }
    }
    virtual ~declFunClass_() {};
};
