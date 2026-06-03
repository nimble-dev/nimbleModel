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
        int dm = instr->dims[0];
        const auto& vals = instr->values->operator[](0);
        if(len*dm != vals.size()) std::cout<<"len*dm != vals.size() in calc_1_matp_"<<std::endl;
        if(len < 1) return(0);
        Eigen::Tensor<int, 1> idx(dm);
        double logProb(0.);
        for(int i = 0; i < len; ++i) {
            for(int p = 0; p < dm; ++p)
                idx[p] = vals[i*dm+p];
            logProb += (static_cast<Derived*>(this)->*Method)(idx); 
        }
        return(logProb);
    }
    template<auto Method>
    double calc_1_matp_ord_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        int dm = instr->dims[0];
        const auto& vals = instr->values->operator[](0);
        if(len*dm != vals.size()) std::cout<<"len*dm != vals.size() in calc_1_matp_"<<std::endl;
        if(len < 1) return(0);
        Eigen::Tensor<int, 1> idx(dm);
        Eigen::Tensor<int, 1> slots(dm);
        for(int p = 0; p < dm; ++p)
          slots[p] = instr->slots[p]-1;
        double logProb(0.);
        for(int i = 0; i < len; ++i) {
            for(int p = 0; p < dm; ++p)
              idx[slots[p]] = vals[i*dm+p];  
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
        int dm = instr->dims[0];
        const auto& vals = instr->values->operator[](0);
        if(len*dm != vals.size()) std::cout<<"len*dm != vals.size() in sim_1_matp_"<<std::endl;
        if(len < 1) return;
        Eigen::Tensor<int, 1> idx(dm);
        for(int i = 0; i < len; ++i) {
          for(int p = 0; p < dm; ++p)
            idx[p] = vals[i*dm+p];
          static_cast<Derived*>(this)->sim_one(idx);
        }
    }
    void sim_1_matp_ord_(std::shared_ptr<instr_nClass> instr) {
        int len = instr->lens[0];
        int dm = instr->dims[0];
        const auto& vals = instr->values->operator[](0);
        if(len*dm != vals.size()) std::cout<<"len*dm != vals.size() in sim_1_matp_"<<std::endl;
        if(len < 1) return;
        Eigen::Tensor<int, 1> idx(dm);
        Eigen::Tensor<int, 1> slots(dm);
        for(int p = 0; p < dm; ++p)
          slots[p] = instr->slots[p]-1;
        for(int i = 0; i < len; ++i) {
          for(int p = 0; p < dm; ++p)
            idx[slots[p]] = vals[i*dm+p];
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
