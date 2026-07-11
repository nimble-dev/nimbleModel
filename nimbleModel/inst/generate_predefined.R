## # To update the set of predefined nClasses:
##
## # generate new predef/instr_nC. Move that directly to package code inst/nimbleModel/predef/instr_nC
nCompile(instr_nClass = nimbleModel:::instr_nClass, control=list(generate_predefined=TRUE))
test <- nCompile(instr_nClass = nimbleModel:::instr_nClass)
## #
## # generate new predef/declFunBase_nC. Move to package and add
## # "#include <nimbleModel/predef/declFunClass_/declFunClass_.h>" in the hContent
## # And add "// [[Rcpp::depends(nimbleModel)]]" to the cppContent
## # after declaration of declFunBase_nClass
nCompile(nimbleModel:::declFunBase_nClass, control=list(generate_predefined=TRUE))
test <- nCompile(nimbleModel:::declFunBase_nClass)
## #
## # generate new predef/modelBase_nC. Move to package and add
## # "#include <nimbleModel/predef/modelClass_/modelClass_.h>" to the hContent
## # And add "// [[Rcpp::depends(nimbleModel)]]" to the cppContent
## # after the declaration of modelBase_nClass.
nCompile(modelBase_nClass = nimbleModel:::modelBase_nClass, control=list(generate_predefined=TRUE))
test <- nCompile(nimbleModel:::modelBase_nClass)

## # generate new predef/copier_nC. Move to package and add
## # "#include <nimbleModel/predef/copier_nC_base/copier_nC_base.h>" to the hContent before the class declaration
## # And add "// [[Rcpp::depends(nimbleModel)]]" to the cppContent
## # after the declaration of copier_nClass.
nCompile(copier_nClass = nimbleModel:::copier_nClass, control=list(generate_predefined=TRUE))
test <- nCompile(nimbleModel:::copier_nClass)

## # generate new predef/multiCopier_nC. Move to package and add
## # "#include <nimbleModel/predef/multiCopier_nC_base/multiCopier_nC_base.h>" to the hContent before the class declaration
## # And add "// [[Rcpp::depends(nimbleModel)]]" to the cppContent
## # after the declaration of copier_nClass.
## # Note there will be an expected set of errors when re-generating before the #include has been added.
nCompile(multiCopier_nClass = nimbleModel:::multiCopier_nClass, control=list(generate_predefined=TRUE))
test <- nCompile(nimbleModel:::multiCopier_nClass)
