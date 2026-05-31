// to be included from the predefined nodeFxnBase_nClass.
// Add "#include <nCompiler/predef/nodeFxnClass_/nodeFxnClass_.h>" to that file,
// after the declaration of nodeFxnBase_nClass.

template<class Derived>
class declFunClass_ : public declFunBase_nClass {
public:
    double v;
    declFunClass_() {};

    double  calculate ( std::shared_ptr<instr_nClass> nodeInstr ) override {
RESET_EIGEN_ERRORS
double logProb(0.0);
// const auto& methodInstr = nodeInstr->methodInstr;
// const auto& indsInstrVec = nodeInstr->indsInstrVec;
// logProb += static_cast<Derived*>(this)->calc_one((*indsInstrVec)[0]);
return(logProb);
    }

    virtual ~declFunClass_() {};
};
