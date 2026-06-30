test_that("declRules are generated correctly", {
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    context_i <- modelContextClass$new(list(singleContext1))

    modelDecl <- modelDeclClass$new(quote(y[i+2] ~ dnorm(0,1)), context_i, 1)
    modelDecl$processDecl(NULL, list(), .GlobalEnv) 
    expect_identical(modelDecl$stoch, TRUE)
    expect_equal(
        modelDecl$declRule$originalIndexingRule$apply(
                  varRangeClass$new(list(
                                    newIndexRange(quote(3:6))))),
        varRangeClass$new(list(
                          newIndexRange(quote(2:4))), varName = 'y')
    )
})

test_that("nodeRule creation and application works", {
    ## Apply nodeRule to varRange
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 3:5){}))
    context_i <- modelContextClass$new(list(singleContext1))
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))

    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)

    expect_identical(LHSrule$numExternalIndexRules, 1L)
    expect_identical(LHSrule$numInternalIndexRules, 0L)
    expect_identical(LHSrule$indexSlotToSet, 1L)
    expect_identical(is(LHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'), TRUE)
                     
    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(3)), varName = 'mu'))
    expect_identical(result$boolExternalIndexRanges, FALSE)
    expect_identical(result$numExternalIndexRanges, 0L)
    expect_identical(result$indexSlotToRange, 1)
    expect_equal(result$indexRanges[[1]], newIndexRange(3))

    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(6:11))),varName = 'mu'))
    expect_identical(result$boolExternalIndexRanges, TRUE)
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexSlotToRange, 1)
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(6:9)))

    LHS <- quote(mu[1:3,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)

    expect_identical(LHSrule$numExternalIndexRules, 1L)
    expect_identical(LHSrule$numInternalIndexRules, 1L)
    expect_identical(LHSrule$indexSlotToSet, c(0L,1L))
    expect_identical(is(LHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'), TRUE)
    expect_identical(is(LHSrule$internalRule$indexRules[[1]], 'indexRuleConstantClass'), TRUE)

    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                   newIndexRange(quote(4:9))), varName = 'mu'))

    expect_identical(result$boolExternalIndexRanges, c(TRUE, FALSE))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexSlotToRange, c(2L, 1L))
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(4:8)))
    expect_equal(result$indexRanges[[2]], newIndexRange(quote(1:3)))

    ## input of part of the internal block resolves to full internal block
    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:2)),
                                                   newIndexRange(quote(4:9))), varName = 'mu'))
    expect_identical(result$boolExternalIndexRanges, c(TRUE, FALSE))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexSlotToRange, c(2L, 1L))
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(4:8)))
    expect_equal(result$indexRanges[[2]], newIndexRange(quote(1:3)))

    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)

    expect_identical(LHSrule$numExternalIndexRules, 2L)
    expect_identical(LHSrule$numInternalIndexRules, 2L)
    expect_identical(LHSrule$indexSlotToSet, c(0L,2L,1L,0L))
    expect_identical(is(LHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'), TRUE)
    expect_identical(is(LHSrule$externalRule$indexRules[[2]], 'indexRuleBlockClass'), TRUE)
    expect_identical(is(LHSrule$internalRule$indexRules[[1]], 'indexRuleConstantClass'), TRUE)
    expect_identical(is(LHSrule$internalRule$indexRules[[2]], 'indexRuleConstantClass'), TRUE)

    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:2)),
                                                      newIndexRange(quote(4:6)),
                                                      newIndexRange(matrix(c(1,9,4,6,5))),
                                                      newIndexRange(2)), varName = 'mu'))

    expect_identical(result$boolExternalIndexRanges, c(TRUE,TRUE,FALSE,FALSE))
    expect_identical(result$numExternalIndexRanges, 2L)
    expect_identical(result$indexSlotToRange, c(3L,1L,2L,4L))
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(4:5)))
    expect_equal(result$indexRanges[[2]], newIndexRange(quote(4:6)))
    expect_equal(result$indexRanges[[3]], newIndexRange(quote(1:3)))
    expect_equal(result$indexRanges[[4]], newIndexRange(2))

    ## invalid external range
    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:2)),
                                                      newIndexRange(35),
                                                      newIndexRange(matrix(c(1,9,4,6,5))),
                                                   newIndexRange(2)), varName = 'mu'))
    expect_identical(result, NULL)
    
    ## invalid internal range 
    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:2)),
                                                      newIndexRange(quote(4:6)),
                                                      newIndexRange(matrix(c(1,9,4,6,5))),
                                                   newIndexRange(4)), varName = 'mu'))
    expect_identical(result, NULL)

    ## with varRange matrix covering two columns that go across internal and external

    ## some pairs of input indexRange invalid
    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:2)),
                                                   newIndexRange(quote(4:6)),
                                                   newIndexRange(matrix(c(5,3,12,12,6,2,4,2,4,2),
                                                                     ncol = 2))), varName = 'mu'))

    expect_identical(result$boolExternalIndexRanges, c(TRUE,TRUE,FALSE,FALSE))
    expect_identical(result$numExternalIndexRanges, 2L)
    expect_identical(result$indexSlotToRange, c(3L,1L,2L,4L))
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(4:5)))
    expect_equal(result$indexRanges[[2]], newIndexRange(quote(5:6)))
    expect_equal(result$indexRanges[[3]], newIndexRange(quote(1:3)))
    expect_equal(result$indexRanges[[4]], newIndexRange(2))
    
    ## all pairs of input indexRange invalid
    result <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:2)),
                                                   newIndexRange(4:6),
                                                   newIndexRange(matrix(c(3,12,4,2), ncol = 2))),
                                              varName = 'mu'))
    expect_identical(result, NULL)
    
    ## Apply nodeRule to nodeRange (modified so that output is not same as input)
    LHS <- quote(mu[1:3,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    nodeRange <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                   newIndexRange(quote(4:9))), varName = 'mu'))
    result <- LHSrule$apply(nodeRange)
    expect_equal(result, nodeRange)

    nodeRange$indexRanges[[1]]$end <- 35  # have nodeRange extend beyond true extent
    result <- LHSrule$apply(nodeRange)
    expect_identical(result$boolExternalIndexRanges, c(TRUE, FALSE))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexSlotToRange, c(2L, 1L))
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(4:8)))
    expect_equal(result$indexRanges[[2]], newIndexRange(quote(1:3)))

    ## no indexing case
    context_0 <- modelContextClass$new()
    LHSrule <- nodeRuleClass$new(quote(mu), 1, context_0)
    
    result <- LHSrule$apply('mu')
    expect_true(result$isNone())

    result <- LHSrule$apply(varRangeClass$new(list(), varName = 'mu'))
    expect_true(result$isNone())

    expect_error(LHSrule$apply(varRangeClass$new(list(newIndexRange(3)), varName = 'mu')),
                 "incorrect number of input indices")
   
})

test_that("rhsRule creation and application works", {
    ## Basic case. More (implicitly) in `exclude` testing.

    context_0 <- modelContextClass$new()
    RHSrule <- rhsRuleClass$new(quote(sigma), 1, context_0)

    result <- RHSrule$apply('sigma')
    expect_true(result$isNone())

    expect_error(RHSrule$apply(varRangeClass$new(list(newIndexRange(3)), varName = 'sigma')),
                 "incorrect number of input indices")
                            
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)

    expect_identical(RHSrule$numExternalIndexRules, 1L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_identical(RHSrule$indexSlotToSet, 1L)
    expect_identical(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'), TRUE)

    result <- RHSrule$apply(varRangeClass$new(list(newIndexRange(quote(5:12))), varName = 'mu'))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexSlotToRange, 1)
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(5:9)))

    expect_error(RHSrule$apply(varRangeClass$new(list(newIndexRange(3), newIndexRange(5)), varName = 'mu')),
                 "incorrect number of input indices")

    
    RHS <- quote(mu[i+1, 1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)

    expect_identical(RHSrule$numExternalIndexRules, 2L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_identical(RHSrule$indexSlotToSet, c(1L,2L))
    expect_identical(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'), TRUE)
    expect_identical(is(RHSrule$externalRule$indexRules[[2]], 'indexRuleBlockClass'), TRUE)

    result <- RHSrule$apply(varRangeClass$new(list(
                                              newIndexRange(quote(5:12)),
                                              newIndexRange(3)), varName = 'mu'))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexSlotToRange, c(1L, 2L))
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(5:9)))
    expect_equal(result$indexRanges[[2]], newIndexRange(3))

    result <- RHSrule$apply(varRangeClass$new(list(
                                              newIndexRange(quote(5:12)),
                                              newIndexRange(quote(2:4))), varName = 'mu'))
    expect_identical(result$numExternalIndexRanges, 2L)
    expect_identical(result$indexSlotToRange, c(1L, 2L))
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(5:9)))
    expect_equal(result$indexRanges[[2]], newIndexRange(quote(2:3)))
    
})

test_that("rhsRule creation when RHS missing an index and relevant singleContexts use that index", {
    ## Test case with duplicate RHS node creation and dependent indexing, e.g.,
    ## `for(i in 1:4) for(t in 1:seasons[i]) y[i,t] <- alpha[t]`.
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:4){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in n1[i]:n2[i]){}))
    
    context <- modelContextClass$new(list(singleContext1,singleContext2))

    RHS <- quote(alpha[j])
    RHSrule <- rhsRuleClass$new(RHS, 1, context, constants = list(n1 = c(2,3,5,2), n2 = c(2,8,6,3)))
    expect_identical(RHSrule$numExternalIndexRules, 1L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_true(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'))
    expect_identical(RHSrule$externalRule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 2, fromMax = 8))

    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:4){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in n1[i]:n2[i]){}))
    singleContext3 <-
        singleContextClass$new(forCode = quote(for(k in 1:n3[i]){}))
    
    context <- modelContextClass$new(list(singleContext1, singleContext2, singleContext3))

    RHS <- quote(alpha[j,k])
    RHSrule <- rhsRuleClass$new(RHS, 1, context, constants = list(n1 = c(2,3,5,2), n2 = c(2,8,6,3), n3 = c(5,9,1,2)))
    expect_identical(RHSrule$numExternalIndexRules, 2L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_true(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'))
    expect_true(is(RHSrule$externalRule$indexRules[[2]], 'indexRuleBlockClass'))
    expect_identical(RHSrule$externalRule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 2, fromMax = 8))
    expect_identical(RHSrule$externalRule$indexRules[[2]]$setupResults,
                     list(offset = 0, fromMin = 1, fromMax = 9))

    
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:4){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in n1[i]:n2[i]){}))
    singleContext3 <-
        singleContextClass$new(forCode = quote(for(k in n3[j]:n4[i]){}))  # uses both 'j' and 'i'
    
    context <- modelContextClass$new(list(singleContext1, singleContext2, singleContext3))

    RHS <- quote(alpha[j,k])
    RHSrule <- rhsRuleClass$new(RHS, 1, context, constants = list(n1 = c(2,3,5,2), n2 = c(2,8,6,3),
                                                                  n3 = c(5,9,4,3,3,3,3,4), n4 = c(11,10,4,8,4,4,4,4)))
    expect_identical(RHSrule$numExternalIndexRules, 2L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_true(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'))
    expect_true(is(RHSrule$externalRule$indexRules[[2]], 'indexRuleBlockClass'))
    expect_identical(RHSrule$externalRule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 2, fromMax = 8))
    expect_identical(RHSrule$externalRule$indexRules[[2]]$setupResults,
                     list(offset = 0, fromMin = 3, fromMax = 11))

    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:4){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 1:3){}))
    singleContext3 <-
        singleContextClass$new(forCode = quote(for(k in 1:n1[i]){}))
    
    context <- modelContextClass$new(list(singleContext1, singleContext2, singleContext3))

    RHS <- quote(alpha[j,k])
    RHSrule <- rhsRuleClass$new(RHS, 1, context, constants = list(n1 = c(2,3,6,2)))
    expect_identical(RHSrule$numExternalIndexRules, 2L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_true(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleBlockClass'))
    expect_true(is(RHSrule$externalRule$indexRules[[2]], 'indexRuleBlockClass'))
    expect_identical(RHSrule$externalRule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 1, fromMax = 3))
    expect_identical(RHSrule$externalRule$indexRules[[2]]$setupResults,
                     list(offset = 0, fromMin = 1, fromMax = 6))

    
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:4){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in n1[i]:n2[i]){}))
        
    context <- modelContextClass$new(list(singleContext1, singleContext2))

    RHS <- quote(alpha[idx[j]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context, constants = list(n1 = c(3,4,6,3), n2 = c(4,7,7,3), idx = c(11,2,3,2,4,9,3,15)))
    expect_identical(RHSrule$numExternalIndexRules, 1L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_true(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleArbitraryClass'))
    expect_identical(RHSrule$fullRange$indexRanges[[1]]$values,
                     matrix(c(2,3,4,9)))  # only the idx values within j=3:7

    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:4){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in c(3,7,4)){}))
    context <- modelContextClass$new(list(singleContext1, singleContext2))

    RHS <- quote(alpha[j])
    RHSrule <- rhsRuleClass$new(RHS, 1, context)
    expect_identical(RHSrule$numExternalIndexRules, 1L)
    expect_identical(RHSrule$numInternalIndexRules, 0L)
    expect_true(is(RHSrule$externalRule$indexRules[[1]], 'indexRuleArbitraryClass'))
    expect_identical(RHSrule$fullRange$indexRanges[[1]]$values,
                     matrix(c(3,4,7)))
        
})


test_that("calcRanges are generated correctly", {  
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 3:5){}))
    singleContext3 <-
        singleContextClass$new(forCode = quote(for(k in 1:4){}))
    context_i <- modelContextClass$new(list(singleContext1))
    context_ijk <- modelContextClass$new(list(singleContext1, singleContext2, singleContext3))
    context_0 <- modelContextClass$new()

    modelDecl <- modelDeclClass$new(quote(y ~ dnorm(mu, sigma)), context_0, 1)
    modelDecl$processDecl(NULL, list(), .GlobalEnv)
    declRule <- declRuleClass$new(modelDecl, 1, context_0)
    calcRule <- calcRuleClass$new(declRule, NULL, NULL, context_0)
    result <- calcRule$apply(varRangeClass$new(list(), varName = 'y'))
    expect_identical(result$numExternalIndexRanges, 0L)
    expect_true(result$isNone())
    
    modelDecl <- modelDeclClass$new(quote(y[i+1] ~ dnorm(0,1)), context_i, 1)
    modelDecl$processDecl(NULL, list(), .GlobalEnv)
    declRule_i <- declRuleClass$new(modelDecl, 1, context_i)
    
    calcRule <- calcRuleClass$new(declRule_i, NULL, NULL, context_i)

    ## Basic nodeRule style apply
    result <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(3:5))), varName = 'y'))
    expect_identical(result$boolExternalIndexRanges, TRUE)
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexSlotToRange, 1)
    expect_equal(result$indexRanges[[1]], newIndexRange(quote(3:5)))

    ## Generation of calcRules
   
    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(quote(2:4))), varName = 'y'))

    ## Mismatched varNames
    expect_error(calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(3:5))),
                                                               varName = 'foo')),
                 "does not match")
    
    ## No overlap
    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(50:53)))))
    expect_equal(calcRange, NULL)

    ## No input range
    calcRange <- calcRule$makeCalcRange()
    expect_equal(calcRange, NULL)

    ## Multiple indices
    modelDecl <- modelDeclClass$new(quote(y[7:9, i+1] ~ dmnorm(z[1:3],pr[1:3,1:3])), context_i, 1)
    modelDecl$processDecl(NULL, list(), .GlobalEnv) 
    declRule_i <- declRuleClass$new(modelDecl, 1, context_i)
    calcRule <- calcRuleClass$new(declRule_i, NULL, NULL, context_i)
 
    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(2:4)), newIndexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange, NULL)

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(7:9)), newIndexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(quote(2:4))), varName = 'y'))

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(2)), newIndexRange(quote(3:5)))))
    expect_equal(calcRange, NULL)

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(7)), newIndexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(quote(2:4))), varName = 'y'))
    
    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(5:8)), newIndexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(quote(2:4))), varName = 'y'))

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(newIndexRange(quote(6)), newIndexRange(quote(3:5)))))
    expect_equal(calcRange, NULL)

    ## Using a nodeRange (modified so that output is not same as input)
    LHS <- quote(y[7:9, i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    nodeRange <- LHSrule$apply(varRangeClass$new(list(newIndexRange(quote(7:9)),
                                                   newIndexRange(quote(5:9))), varName = 'y'))
    calcRange <- calcRule$makeCalcRange(nodeRange)
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(quote(4:8))), varName = 'y'))

    nodeRange$indexRanges[[1]]$end <- 35  # have nodeRange extend beyond true extent
    calcRange <- calcRule$makeCalcRange(nodeRange)
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(quote(4:8))), varName = 'y'))

    ## Multiple loops
    modelDecl <- modelDeclClass$new(quote(y[j, i+1, k, 2] ~ dnorm(0,1)), context_ijk, 1)
    modelDecl$processDecl(NULL, list(), .GlobalEnv) 
    declRule_ijk <- declRuleClass$new(modelDecl, 1, context_ijk)
    calcRule <- calcRuleClass$new(declRule_ijk, NULL, NULL, context_ijk)
    
    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(
                                                  newIndexRange(quote(3:5)),
                                                  newIndexRange(matrix(c(3,12,8))),
                                                  newIndexRange(quote(1:6)),
                                                  newIndexRange(2))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(matrix(c(2,7))),
                                        newIndexRange(quote(3:5)),
                                        newIndexRange(quote(1:4))), varName = 'y'))
    

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(
                                                  newIndexRange(quote(3:5)),
                                                  newIndexRange(matrix(c(3,12,8))),
                                                  newIndexRange(quote(1:6)),
                                                  newIndexRange(3))))
    expect_equal(calcRange, NULL)

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(
                                                  newIndexRange(quote(3:5)),
                                                  newIndexRange(matrix(c(3,9,3,11,11,1,2,5,2,7), ncol = 2)),
                                                  newIndexRange(2))
                                              ))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(matrix(c(2,8,1,2), ncol = 2)),
                                        newIndexRange(quote(3:5))), rangeToIndexSlot = list(c(1,3), 2), varName = 'y'))

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(
                                                  newIndexRange(quote(3:5)),
                                                  newIndexRange(matrix(c(3,12,5,7,2,2,3,2), ncol = 2)),
                                                  newIndexRange(quote(1:5))
                                              ), rangeToIndexSlot = list(1,c(2,4), 3)))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(matrix(c(2,6))),
                                        newIndexRange(quote(3:5)),
                                        newIndexRange(quote(1:4))), varName = 'y'))

    ## j in 1:n[i] type case
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:3){}))
    singleContext2ni <-
        singleContextClass$new(indexVarExpr = quote(j),
                           indexRangeExpr = quote(1:n[i]),
                           )
    context_ijni <- modelContextClass$new(list(singleContext1,
                                               singleContext2ni))
    n <- c(1,3,2)
    modelDecl <- modelDeclClass$new(quote(y[j, i+1] ~ dnorm(0,1)), context_ijni, 1)
    modelDecl$processDecl(NULL, constants = list(n = n), .GlobalEnv) 
    declRule_ijni <- declRuleClass$new(modelDecl, 1, context_ijni, constants = list(n = n))
    calcRule <- calcRuleClass$new(declRule_ijni, NULL, NULL, context_ijni, constants = list(n = n))

    calcRange <- calcRule$makeCalcRange(varRangeClass$new(list(
                                                               newIndexRange(quote(1:4)),
                                                               newIndexRange(quote(1:3))
                                                           )))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(newIndexRange(matrix(c(1,2,2,2,1,1,2,3), ncol = 2))),
                                   varName = 'y'))                                        
})



## Hopefully comprehensive testing of fracture()
test_that("calcRule fracturing works", {
    ## For simplicity, these tests use generic `nodeRule` inputs rather than `calcRule` that would be used in real work.
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 1:4){}))
    
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))
    context_j <- modelContextClass$new(list(singleContext2))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    
    LHS <- quote(mu)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_0)
    ## fracture with mu itself
    fracRange <- calcRule$apply(varRangeClass$new(list(), varName = 'mu'))
    result <- fracture(calcRule, fracRange)
    expect_identical(result, NULL)
    
    ## scalar overlap at end
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_i)
    ## fracture with mu[3]
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(3)), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)

    expect_identical(length(result), 2L)
    expr <- quote(mu[i])
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 3:3){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 4:9){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## seq overlap at end
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_i)
    ## fracture with mu[3:4]
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(3:4))), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)

    expect_identical(length(result), 2L)
    expr <- quote(mu[i])
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 3:4){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 5:9){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    
    ## seq overlap in middle
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_i)
    ## fracture with mu[4:5]
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(4:5))), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)

    expect_identical(length(result), 3L)
    expr <- quote(mu[i])
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 4:5){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 3:3){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 6:9){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[3]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## full overlap
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_i)
    ## fracture with mu[4:5]
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(3:11))), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)
    expect_identical(result, NULL)
    

    ## seq and matrix
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_i)
    ## fracture with matrix
    idx <- c(3,6,9)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(matrix(idx))), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)

    expect_identical(length(result), 2L)
    expr <- quote(mu[idx[i]])
    idx2 <- c(4,5,7,8)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:3){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:4){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx2))
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)

    ## matrix and matrix
    constants <- list(k = c(1,3,5,7,9,11,15,13))
    LHS <- quote(mu[k[i]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i, constants = constants)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_i, constants = constants)

    idx <- c(3,7,8,9)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(matrix(idx))), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)
    
    expect_identical(length(result), 2L)
    expr <- quote(mu[idx[i]])
    idx1 <- c(3,7,9)
    idx2 <- c(5,11,13,15)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:3){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx1))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:4){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx2))
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)

    ## full overlap
    idx <- c(7,3,5,8,9,11,13,15,23)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(matrix(idx))), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)
    expect_identical(result, NULL)

    ## 2-d matrix and matrix
    constants <- list(k1 = c(2,3,4,5), k2 = c(3,3,5,5))
    LHS <- quote(mu[k1[j],k2[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = constants)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_j, constants = constants)

    idx <- matrix(c(2,4,3,5), nrow = 2)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(idx)), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)
    expect_identical(length(result), 2L)
    expr <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(2,4)
    idx2 <- c(3,5)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:2){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)
    idx1 <- c(3,5)
    idx2 <- c(3,5)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:2){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)

    ## full overlap
    constants <- list(k1 = c(5,2,3,4), k2 = c(5,3,3,6))
    LHS <- quote(mu[k1[j],k2[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = constants)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_j, constants = constants)

    idx <- matrix(c(2,4,3,5,6,3,6,3,5,7), nrow = 5)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(idx)), varName = 'mu'))
    result <- fracture(calcRule, fracRange)
    expect_identical(result, NULL)
    
    ## basic case with one external, one internal: mu[1:3, i]
    LHS <- quote(mu[1:3,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_i)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                      newIndexRange(quote(2:3))), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)

    expect_identical(length(result), 2L)
    expect_equal(result[[1]]$internalRule$indexRules[[1]]$setupResults,
                     LHSrule$internalRule$indexRules[[1]]$setupResults)
    expect_equal(result[[2]]$internalRule$indexRules[[1]]$setupResults,
                     LHSrule$internalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:3){}))))
    expected <- nodeRuleClass$new(LHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 4:8){}))))
    expected <- nodeRuleClass$new(LHS, 1, context_tmp)
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## two external indices, one fractured, two constant internal rules
    LHS <- quote(mu[1:3,j,i,2:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_ij)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                      newIndexRange(quote(1:4)),
                                                      newIndexRange(matrix(c(2,4))),
                                                      newIndexRange(2)), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)
    
    expect_identical(length(result), 2L)
    expect_identical(LHSrule$indexSlotToSet, c(0L,2L,1L,0L))
    for(k in 1:2) {
        for(kk in 1:2)
            expect_equal(result[[k]]$internalRule$indexRules[[kk]]$setupResults,
                             LHSrule$internalRule$indexRules[[kk]]$setupResults)
        expect_equal(result[[k]]$externalRule$indexRules[[1]]$setupResults,
                         LHSrule$externalRule$indexRules[[2]]$setupResults)
        expect_identical(result[[k]]$indexSlotToSet, c(0L,1L,2L,0L))
    }
    
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:2){}))))
    idx <- as.integer(c(2,4))
    expr <- quote(mu[idx[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:5){}))))
    idx <- as.integer(c(3,5,6,7,8))
    expr <- quote(mu[idx[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[2]]$externalRule$indexRules[[2]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)

    ## two external indices, one fractured, one constant internal rule and additional external from scalar
    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_ij)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                      newIndexRange(quote(1:4)),
                                                      newIndexRange(matrix(c(2,4))),
                                                      newIndexRange(2)), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)

    ## Just check stuff related to the scalar constant index, given similarity to above test.
    expect_identical(length(result), 2L)
    expect_identical(LHSrule$indexSlotToSet, c(0L,2L,1L,0L))
    for(k in 1:2) {
        expect_equal(result[[k]]$internalRule$indexRules[[1]]$setupResults,
                         LHSrule$internalRule$indexRules[[1]]$setupResults)
        expect_equal(result[[k]]$externalRule$indexRules[[1]]$setupResults,
                         LHSrule$externalRule$indexRules[[2]]$setupResults)
        expect_identical(result[[k]]$indexSlotToSet, c(0L,1L,2L,0L))
    }

    
    ## two external indices, both fractured: mu[1:3, j ,2, i]
    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_ij)
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                      newIndexRange(quote(2:3)),
                                                      newIndexRange(quote(2:3)),
                                                      newIndexRange(2)), varName = 'mu'))
    
    result <- fracture(calcRule, fracRange)

    expect_identical(length(result), 2L)
    expect_identical(LHSrule$indexSlotToSet, c(0L,2L,1L,0L))
    for(k in 1:2) {
        expect_equal(result[[k]]$internalRule$indexRules[[1]]$setupResults,
                         LHSrule$internalRule$indexRules[[1]]$setupResults)
        expect_identical(result[[k]]$indexSlotToSet, c(0L,1L,1L,0L))
    }

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:4){}))))
    idx1 <- as.integer(c(2,2,3,3))
    idx2 <- as.integer(c(2,3,2,3))
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:24){}))))
    idx1 <- as.integer(c(1,4,1,4,rep(1:4, 5)))
    idx2 <- as.integer(c(2,2,3,3,rep(4:8, each = 4)))
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)


    ## two external indices fractured: mu[1:3, j ,2, i] , based on 2-d matrix
    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    calcRule <- calcRuleClass$new(LHSrule, NULL, NULL, context_ij)
    
    fracRange <- calcRule$apply(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                      newIndexRange(matrix(c(2,3,3,7), ncol = 2)),
                                                      newIndexRange(2)), varName = 'mu'))

    result <- fracture(calcRule, fracRange)

    expect_identical(length(result), 2L)
    expect_identical(LHSrule$indexSlotToSet, c(0L,2L,1L,0L))
    for(k in 1:2) {
        expect_equal(result[[k]]$internalRule$indexRules[[1]]$setupResults,
                         LHSrule$internalRule$indexRules[[1]]$setupResults)
        expect_identical(result[[k]]$indexSlotToSet, c(0L,1L,1L,0L))
    }

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:2){}))))
    idx1 <- as.integer(c(2,3))
    idx2 <- as.integer(c(3,7))
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:26){}))))
    idx1 <- as.integer(rep(1:4, 7))
    idx2 <- as.integer(rep(2:8, each = 4))
    wh <- (idx1 == 2 & idx2 == 3) | (idx1 == 3 & idx2 == 7)
    idx1 <- idx1[!wh]
    idx2 <- idx2[!wh]
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                 expected$externalRule$indexRules[[1]]$setupResults)

})

## Hopefully comprehensive testing of exclude()
test_that("RHS exclusion works", {
    ## Note that this uses a generic `nodeRule` as the excluding input,
    ## though in real work, it would be a `rhsRule` or a `declRule`.

    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 1:4){}))
    
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))
    context_j <- modelContextClass$new(list(singleContext2))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    RHS <- quote(mu)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_0)
    LHS <- quote(mu)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    result <- exclude(RHSrule, LHSrule)[[1]]
    expect_identical(result, NULL)

    ## scalar/seq overlap at end
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)[[1]]

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 3:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result$externalRule$indexRules[[1]]$setupResults,
                    expected$externalRule$indexRules[[1]]$setupResults)

    ## scalar/seq overlap no overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[33])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)

    result <- exclude(RHSrule, LHSrule)
    expect_identical(result[[1]], RHSrule)

    ## scalar/seq overlap in middle
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[4])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:2){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                    expected$externalRule$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 4:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                    expected$externalRule$indexRules[[1]]$setupResults)


    ## seq/seq partial overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:5){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 5:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                    expected$externalRule$indexRules[[1]]$setupResults)


    ## seq/seq full overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:9){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)
    expect_identical(result, NULL)

    ## matrix in LHS
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[idx[i]])
    idx <- c(2,5,4)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp, constants = list(idx = idx))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:2){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)

    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 5:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)

    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)


    ## matrix in RHS
    idx <- c(14,4,2,9,1,3,7,11)
    RHS <- quote(mu[idx[i]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx = idx))
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:4){}))))
    idx <- as.integer(c(4,7,9,11))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx = idx))

    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## two-d arbitrary case, extracting block elements from RHS matrix
    RHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx1 = idx1, idx2 = idx2))
    LHS <- quote(mu[i,j])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:5){}))))
    idx1 <- c(11,11,12,13,5)
    idx2 <- c(2,5,6,7,13)
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    

    ## two-d arbitrary case, extracting matrix elements from RHS block
    RHS <- quote(mu[i,j])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij)
    LHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i, constants = list(idx1 = idx1, idx2 = idx2))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:26){}))))
    idx1 <- c(2,3,5,6,7,8,2,4:8,rep(2:8, 2))
    idx2 <- c(rep(1,6),rep(2,6),rep(3,7), rep(4,7))
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    expected <- rhsRuleClass$new(LHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## basic mv node case with constant
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 6:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                    RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[2]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## basic mv node case, all excluded
    RHS <- quote(mu[5,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_0)
    LHS <- quote(mu[i, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    
    result <- exclude(RHSrule, LHSrule)
    expect_identical(result, NULL)

    ## basic mv node case with seq-seq partial overlap
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 4:5){}))))
    LHS <- quote(mu[i+1, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 7:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                    RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[2]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## basic mv node case with shared matrix index
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1,2)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx = idx))
    LHS <- quote(mu[5, idx[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = list(idx = idx))
    
    result <- exclude(RHSrule, LHSrule, constants = list(idx = idx))

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:4){})),
                                              singleContextClass$new(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 6:8){})),
                                              singleContextClass$new(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[2]]$externalRule$indexRules[[1]]$setupResults,
                    RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[2]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    
    ## Awkward intersections

    ## Partial overlap in some rows; for now this is simply unrolled.
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:17){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## LHS element inside RHS; for now this is simply unrolled
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[3, 3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:20){}))))
    idx1 <- c(2:8,2:8,2,4:8)
    idx2 <- c(rep(1,7), rep(2,7), rep(3, 6))
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)


    ## LHS fully overlaps RHS block constant in additional dimension; this is handled nicely.
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:4])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:6){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[2]]$setupResults)

    ## basic 3-d case - two identical indices
    RHS <- quote(mu[1:3, i, 1:2])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[1:3, i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
 
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:6){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     RHSrule$externalRule$indexRules[[3]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[3]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$indexSlotToSet, c(1L,3L,2L))

    ## 3-d case with partial overlap in some rows but with additional identical index (j)
    RHS <- quote(mu[i, j, 1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 7:9){})),
                                              singleContextClass$new(forCode = quote(for(j in 1:4){}))))
    LHS <- quote(mu[i, j, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 1:17){})),
                                              singleContextClass$new(forCode = quote(for(j in 1:4){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    expr <- quote(mu[idx1[i], j, idx2[i]])
    expected <- rhsRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[2]]$setupResults)

    ## 3-d case with multi-column shared matrix indexRange
    idx1 <- c(4,7,1,2)
    idx2 <- c(1,9,3,2)
    ord <- order(idx1, idx2)
    idx1 <- idx1[ord]
    idx2 <- idx2[ord]
    
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    LHS <- quote(mu[idx1[j], 2, idx2[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = list(idx1 = idx1, idx2 = idx2))
    
    result <- exclude(RHSrule, LHSrule, constants = list(idx1 = idx1, idx2 = idx2))

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 3:8){})),
                                              singleContextClass$new(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[1]]$setupResults)

    ## 3-d case with multi-column unshared matrix indexRange
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    idx3 <- c(4,7,2,2)
    idx4 <- c(1,10,3,2)
    LHS <- quote(mu[idx3[j], i, idx4[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij, constants = list(idx3 = idx3, idx4 = idx4))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(singleContextClass$new(forCode = quote(for(i in 2:8){})),
                                              singleContextClass$new(forCode = quote(for(j in 1:2){}))))
    idx5 <- c(1,7)
    idx6 <- c(3,9)
    expr <- quote(mu[idx5[j], i, idx6[j]])
    expected <- rhsRuleClass$new(expr, 1, context_tmp, constants = list(idx5 = idx5, idx6 = idx6))
    expect_equal(result[[1]]$externalRule$indexRules[[1]]$setupResults,
                     RHSrule$externalRule$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRule$indexRules[[2]]$setupResults,
                     expected$externalRule$indexRules[[2]]$setupResults)
    
    ## Use of index and constant in wrong way. Error message could be more informative.
    RHS <- quote(mu[i[idx]])
    idx <- c(4,7,1)
    expect_error(RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx = idx)),
                 "Missing values found")
    
    ## Incorrect length of constant. 
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1)
    expect_error(RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx = idx)),
                 "Constants may be incorrect size")
})

test_that("getFullRange works correctly", {
    context_0 <- modelContextClass$new()
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    context_i <- modelContextClass$new(list(singleContext1))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 1:4){}))
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))

    LHS <- quote(mu)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$fullRange,
                     varRangeClass$new(list(), varName = 'mu'))

    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(2:8))), varName = 'mu'))
    
    
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$fullRange,
                     varRangeClass$new(list(newIndexRange(5), newIndexRange(quote(1:3))), varName = 'mu'))

    LHS <- quote(mu[4:5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(4:5)), newIndexRange(quote(1:3))), varName = 'mu'))
    
    LHS <- quote(mu[4:5, i, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(4:5)), newIndexRange(quote(2:8)), newIndexRange(quote(1:3))), varName = 'mu'))
    
    expr <- quote(mu[4:5, j, i, 3])
    LHSrule <- nodeRuleClass$new(expr, 1, context_ij)
    expect_equal(LHSrule$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(4:5)), newIndexRange(quote(1:4)),
                                        newIndexRange(quote(2:8)), newIndexRange(quote(3))), varName = 'mu'))
    
    LHS <- quote(mu[4:5, i, i, 3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(4:5)), newIndexRange(matrix(rep(2:8, 2), ncol = 2)), newIndexRange(3)), varName = 'mu'))

    LHS <- quote(mu[4:5, i, 3, i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(4:5)), newIndexRange(matrix(rep(2:8, 2), ncol = 2)), newIndexRange(3)),
                                   rangeToIndexSlot = list(1, c(2,4), 3),
                                   varName = 'mu'))

})


test_that("nodeRange::print works correctly", {
    ## This tests result of `toChar` as there is no obvious way
    ## to capture and check result of print method.
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:8){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 3:5){}))
    context_i <- modelContextClass$new(list(singleContext1))
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))

    LHS <- quote(mu[1:3,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    
    expect_identical(LHSrule$apply()$toChar(),
                     "`mu[1:3, idx1]`, for idx1 in 2:8")

    LHS <- quote(mu[1:3,j,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    expect_identical(LHSrule$apply()$toChar(),
                     "`mu[1:3, idx1, idx2]`, for idx1 in 3:5, idx2 in 2:8")

    LHS <- quote(mu[1:3,j,i-1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    expect_identical(LHSrule$apply()$toChar(),
                     "`mu[1:3, idx1, idx2]`, for idx1 in 3:5, idx2 in 1:7")
  
})

test_that("nodeRange::toNodeChars works correctly", {
    code <- quote({
        theta ~ dnorm(0, 1)
        for(i in 1:4)
            for(j in 1:2)
                y[i,3,j,i,2:4]~ dmnorm(z[1:3],pr[1:3,1:3])
        for(i in 1:3) {
            w[i] ~ dnorm(0,1)
            v[i,i] ~ dnorm(0,1) # fix bug
        }
    })
    
    model <- nimbleModel(code)
    nodeRanges <- model$getNodes()
    expect_identical(nodeRanges[[1]]$toNodeChars(), "theta")
    expect_identical(nodeRanges[[2]]$toNodeChars(),
                     "lifted_chol_oPpr_oB1to3_comma_1to3_cB_cP[1:3, 1:3]")
    expect_identical(nodeRanges[[3]]$toNodeChars(),
        c("y[1, 3, 1, 1, 2:4]", "y[1, 3, 2, 1, 2:4]", "y[2, 3, 1, 2, 2:4]", 
          "y[2, 3, 2, 2, 2:4]", "y[3, 3, 1, 3, 2:4]", "y[3, 3, 2, 3, 2:4]",
          "y[4, 3, 1, 4, 2:4]", "y[4, 3, 2, 4, 2:4]"))
    expect_identical(nodeRanges[[4]]$toNodeChars(), c("w[1]", "w[2]", "w[3]"))
    expect_identical(nodeRanges[[5]]$toNodeChars(),
                     c("v[1, 1]", "v[2, 2]", "v[3, 3]"))

    expect_identical(nodeRanges[[5]]$toNodeChars(2),
                     "v[2, 2]")

    
})

test_that("removal of indexing for scalar elements of nodeRanges work", {
    code <- quote({
        for(i in 1:10) 
            for(j in 1:10)
                for(k in 1:10)
                    for(l in 1:10)
                        y[5,l,k,j,3, k,i] ~ dnorm(mu[i,j,k,l], 1)
        
    })
    m <- nimbleModel(code)
    deps <- m$getDependencies('mu[1,1:2,4,3]')[[1]]
    nr <- m$getNodes(deps)[[1]]
    expect_identical(nr$indexSlotToRange, as.integer(c(3, 5, 1, 2, 4, 1, 6)))
    expect_equal(nr$indexRanges[[3]], newIndexRange(5))
    expect_equal(nr$indexRanges[[5]], newIndexRange(3))
    expect_equal(nr$indexRanges[[1]], newIndexRange(matrix(c(4,4),nrow=1)))
    expect_equal(nr$indexRanges[[2]], newIndexRange(quote(1:2)))
    expect_equal(nr$indexRanges[[4]], newIndexRange(3))
    expect_equal(nr$indexRanges[[6]], newIndexRange(1))

    code <- quote({
        for(j in 1:3) { 
            for(i in 1:4) {
                y[i,6,j,3] ~ dnorm(mu[j],1)
            }
            mu[j] ~ dnorm(mu0,1)
        }    
    })
    m <- modelClass$new(code,data=list(y=array(rnorm(3*4*6*3), c(4,6,3,3))))
    nr <- getNodes(m, 'y[1:4,6,1,3]')[[1]]
    expect_identical(nr$indexSlotToRange, as.integer(c(1,2,4,3)))
    expect_equal(nr$indexRanges[[1]], newIndexRange(quote(1:4)))
    expect_equal(nr$indexRanges[[2]], newIndexRange(6))
    expect_equal(nr$indexRanges[[4]], newIndexRange(1))
    expect_equal(nr$indexRanges[[3]], newIndexRange(3))

    nr <- getNodes(m,'y[2,6,1:2,3]')[[1]]
    expect_identical(nr$indexSlotToRange, as.integer(c(4,2,1,3)))
    expect_equal(nr$indexRanges[[4]], newIndexRange(2))
    expect_equal(nr$indexRanges[[2]], newIndexRange(6))
    expect_equal(nr$indexRanges[[1]], newIndexRange(quote(1:2)))
    expect_equal(nr$indexRanges[[3]], newIndexRange(3))

})
    
