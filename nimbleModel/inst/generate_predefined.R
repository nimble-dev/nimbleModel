## # To update the set of predefined nClasses:
##
## # generate new predef/instr_nC. Move that directly to package code inst/nimbleModel/predef/instr_nC (i.e. move to package)
nCompiler::nCompile(instr_nClass = nimbleModel:::instr_nClass, control = list(generate_predefined = TRUE))
test <- nCompiler::nCompile(instr_nClass = nimbleModel:::instr_nClass)
## #
## # generate new predef/declFunBase_nC. Move to package.
## # You should see "#include <nimbleModel/predef/declFunClass_/declFunClass_.h>" in the hContent
## # after declaration of declFunBase_nClass
## # and "// [[Rcpp::depends(nimbleModel)]]" in the cppContent
nCompiler::nCompile(nimbleModel:::declFunBase_nClass, control = list(generate_predefined = TRUE))
test <- nCompiler::nCompile(nimbleModel:::declFunBase_nClass)
## #
## # generate new predef/modelBase_nC. Move to package.
## # You should see "#include <nimbleModel/predef/modelClass_/modelClass_.h>" in the hContent
## # after the declaration of modelBase_nClass.
## # and "// [[Rcpp::depends(nimbleModel)]]" in the cppContent
nCompiler::nCompile(modelBase_nClass = nimbleModel:::modelBase_nClass, control = list(generate_predefined = TRUE))
test <- nCompiler::nCompile(nimbleModel:::modelBase_nClass)

## # generate new predef/copier_nClass. Move to package.
## # You should see "#include <nimbleModel/predef/copier_nC_base/copier_nC_base.h>" in the hContent BEFORE the class declaration
## # and "// [[Rcpp::depends(nimbleModel)]]" in the cppContent
nCompiler::nCompile(copier_nClass = nimbleModel:::copier_nClass, control = list(generate_predefined = TRUE))
test <- nCompiler::nCompile(nimbleModel:::copier_nClass)

## # generate new predef/mCopy_nC Move to package.
## # You should see "#include <nimbleModel/predef/mCopy_nC_base/mCopy_model_nC_base.h>" in the hContent BEFORE the class declaration
## # and "// [[Rcpp::depends(nimbleModel)]]" in the cppContent
nCompiler::nCompile(multiCopier_nClass = nimbleModel:::multiCopier_nClass, control = list(generate_predefined = TRUE))
test <- nCompiler::nCompile(nimbleModel:::multiCopier_nClass)

## # generate new predef/mCopyMV_nC. Move to package.
## # You should see "#include <nimbleModel/predef/mCopy_nC_base/mCopy_modelValues_nC_base.h>" in the hContent BEFORE the class declaration
## # and "// [[Rcpp::depends(nimbleModel)]]" in the cppContent
nCompiler::nCompile(multiCopierMV_nClass = nimbleModel:::multiCopierMV_nClass, control = list(generate_predefined = TRUE))
test <- nCompiler::nCompile(nimbleModel:::multiCopierMV_nClass)

## # generate new predef/MVBase_nC Move to package.
## # You should see "#include <nimbleModel/predef/modelValuesClass_/modelValuesClass_.h>" in the hContent
## # and "// [[Rcpp::depends(nimbleModel)]]" in the cppContent
nCompiler::nCompile(modelValuesBase_nClass = nimbleModel:::modelValuesBase_nClass, control = list(generate_predefined = TRUE))
test <- nCompiler::nCompile(nimbleModel:::modelValuesBase_nClass)
