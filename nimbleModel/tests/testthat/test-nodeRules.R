test_that("declRules are generated correctly", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    context_i <- modelContextClass$new(list(singleContext1))

    rule <- declRuleClass$new(quote(y[i+2] ~ dnorm(0,1)), 1, context_i)

    expect_identical(rule$stoch, TRUE)
    expect_equal(
        rule$originalIndexingRule$apply(
                  varRangeClass$new(list(
                                    indexRange(quote(3:6))))),
        varRangeClass$new(list(
                          indexRange(quote(2:4))), varName = 'y')
    )
})

test_that("nodeRule creation and application works", {
    ## Apply nodeRule to varRange
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 3:5){}))
    context_i <- modelContextClass$new(list(singleContext1))
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))

    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)

    expect_identical(LHSrule$numExternalRules, 1L)
    expect_identical(LHSrule$numInternalRules, 0L)
    expect_identical(LHSrule$index2setID, 1)
    expect_identical(is(LHSrule$externalRules$indexRules[[1]], 'indexRuleClass_block'), TRUE)
                     
    result <- LHSrule$apply(varRangeClass$new(list(indexRange(3))))
    expect_identical(result$boolExternalIndexRanges, TRUE)
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexID_2_rangeID, 1L)
    expect_identical(result$indexRanges[[1]], indexRange(3))

    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(6:11)))))
    expect_identical(result$boolExternalIndexRanges, TRUE)
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexID_2_rangeID, 1L)
    expect_identical(result$indexRanges[[1]], indexRange(quote(6:9)))

    LHS <- quote(mu[1:3,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)

    expect_identical(LHSrule$numExternalRules, 1L)
    expect_identical(LHSrule$numInternalRules, 1L)
    expect_identical(LHSrule$index2setID, c(0,1))
    expect_identical(is(LHSrule$externalRules$indexRules[[1]], 'indexRuleClass_block'), TRUE)
    expect_identical(is(LHSrule$internalRules$indexRules[[1]], 'indexRuleClass_constant'), TRUE)

    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                   indexRange(quote(4:9)))))

    expect_identical(result$boolExternalIndexRanges, c(TRUE, FALSE))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexID_2_rangeID, c(2L, 1L))
    expect_identical(result$indexRanges[[1]], indexRange(quote(4:8)))
    expect_identical(result$indexRanges[[2]], indexRange(quote(1:3)))

    ## input of part of the internal block resolves to full internal block
    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:2)),
                                                   indexRange(quote(4:9)))))
    expect_identical(result$boolExternalIndexRanges, c(TRUE, FALSE))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexID_2_rangeID, c(2L, 1L))
    expect_identical(result$indexRanges[[1]], indexRange(quote(4:8)))
    expect_identical(result$indexRanges[[2]], indexRange(quote(1:3)))

    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)

    expect_identical(LHSrule$numExternalRules, 2L)
    expect_identical(LHSrule$numInternalRules, 2L)
    expect_identical(LHSrule$index2setID, c(0,2,1,0))
    expect_identical(is(LHSrule$externalRules$indexRules[[1]], 'indexRuleClass_block'), TRUE)
    expect_identical(is(LHSrule$externalRules$indexRules[[2]], 'indexRuleClass_block'), TRUE)
    expect_identical(is(LHSrule$internalRules$indexRules[[1]], 'indexRuleClass_constant'), TRUE)
    expect_identical(is(LHSrule$internalRules$indexRules[[2]], 'indexRuleClass_constant'), TRUE)

    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:2)),
                                                      indexRange(quote(4:6)),
                                                      indexRange(matrix(c(1,9,4,6,5))),
                                                      indexRange(2))))

    expect_identical(result$boolExternalIndexRanges, c(TRUE,TRUE,FALSE,FALSE))
    expect_identical(result$numExternalIndexRanges, 2L)
    expect_identical(result$indexID_2_rangeID, c(3L,1L,2L,4L))
    expect_identical(result$indexRanges[[1]], indexRange(quote(4:5)))
    expect_identical(result$indexRanges[[2]], indexRange(quote(4:6)))
    expect_identical(result$indexRanges[[3]], indexRange(quote(1:3)))
    expect_identical(result$indexRanges[[4]], indexRange(2))

    ## invalid external range
    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:2)),
                                                      indexRange(35),
                                                      indexRange(matrix(c(1,9,4,6,5))),
                                                   indexRange(2))))
    expect_identical(result, NULL)
    
    ## invalid internal range 
    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:2)),
                                                      indexRange(quote(4:6)),
                                                      indexRange(matrix(c(1,9,4,6,5))),
                                                   indexRange(4))))
    expect_identical(result, NULL)

    ## with varRange matrix covering two columns that go across internal and external

    ## some pairs of input indexRange invalid
    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:2)),
                                                   indexRange(quote(4:6)),
                                                   indexRange(matrix(c(5,3,12,12,6,2,4,2,4,2),
                                                                     ncol = 2)))))

    expect_identical(result$boolExternalIndexRanges, c(TRUE,TRUE,FALSE,FALSE))
    expect_identical(result$numExternalIndexRanges, 2L)
    expect_identical(result$indexID_2_rangeID, c(3L,1L,2L,4L))
    expect_identical(result$indexRanges[[1]], indexRange(quote(4:5)))
    expect_identical(result$indexRanges[[2]], indexRange(quote(5:6)))
    expect_identical(result$indexRanges[[3]], indexRange(quote(1:3)))
    expect_identical(result$indexRanges[[4]], indexRange(2))
    
    ## all pairs of input indexRange invalid
    result <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:2)),
                                                   indexRange(4:6),
                                                   indexRange(matrix(c(3,12,4,2), ncol = 2)))))
    expect_identical(result, NULL)
    
    ## Apply nodeRule to nodeRange (modified so that output is not same as input)
    LHS <- quote(mu[1:3,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    nodeRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                   indexRange(quote(4:9)))))
    result <- LHSrule$apply(nodeRange)
    expect_equal(result, nodeRange)

    nodeRange$indexRanges[[1]][[1]][[2]] <- 35  # have nodeRange extend beyond true extent
    result <- LHSrule$apply(nodeRange)
    expect_identical(result$boolExternalIndexRanges, c(TRUE, FALSE))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexID_2_rangeID, c(2L, 1L))
    expect_identical(result$indexRanges[[1]], indexRange(quote(4:8)))
    expect_identical(result$indexRanges[[2]], indexRange(quote(1:3)))

    ## no indexing case
    context_0 <- modelContextClass$new()
    LHSrule <- nodeRuleClass$new(quote(mu), 1, context_0)
    
    vrNone <- varRangeClass$new(list(nimbleModel:::indexRange_none()))

    result <- LHSrule$apply(vrNone)
    expect_identical(result$indexRanges[[1]], nimbleModel:::indexRange_none())

    expect_error(LHSrule$apply(varRangeClass$new(list(indexRange(3)))),
                 "incorrect number of input indices")
    
})

test_that("rhsRule creation and application works", {
    ## Basic case. More (implicitly) in exclude testing.

    context_0 <- modelContextClass$new()
    RHSrule <- rhsRuleClass$new(quote(sigma), 1, context_0)

    vrNone <- varRangeClass$new(list(nimbleModel:::indexRange_none()))

    result <- RHSrule$apply(vrNone)
    expect_identical(result$indexRanges[[1]], nimbleModel:::indexRange_none())

    expect_error(RHSrule$apply(varRangeClass$new(list(indexRange(3)))),
                 "incorrect number of input indices")
                            

    ## TODO: test cases with extra single contexts:
    ## mu[i] <- tau
    ## mu[i,j] <- mu0[i]
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)

    expect_identical(RHSrule$numExternalRules, 1L)
    expect_identical(RHSrule$numInternalRules, 0L)
    expect_identical(RHSrule$index2setID, 1)
    expect_identical(is(RHSrule$externalRules$indexRules[[1]], 'indexRuleClass_block'), TRUE)

    result <- RHSrule$apply(varRangeClass$new(list(indexRange(quote(5:12)))))
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexID_2_rangeID, 1L)
    expect_equal(result$indexRanges[[1]], indexRange(quote(5:9)))
    
    RHS <- quote(mu[i+1, 1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)

    expect_identical(RHSrule$numExternalRules, 2L)
    expect_identical(RHSrule$numInternalRules, 0L)
    expect_identical(RHSrule$index2setID, c(1,2))
    expect_identical(is(RHSrule$externalRules$indexRules[[1]], 'indexRuleClass_block'), TRUE)
    expect_identical(is(RHSrule$externalRules$indexRules[[2]], 'indexRuleClass_block'), TRUE)

    result <- RHSrule$apply(varRangeClass$new(list(
                                              indexRange(quote(5:12)),
                                              indexRange(3))))
    expect_identical(result$numExternalIndexRanges, 2L)
    expect_identical(result$indexID_2_rangeID, c(1L, 2L))
    expect_equal(result$indexRanges[[1]], indexRange(quote(5:9)))
    expect_equal(result$indexRanges[[2]], indexRange(3))

    result <- RHSrule$apply(varRangeClass$new(list(
                                              indexRange(quote(5:12)),
                                              indexRange(quote(2:4)))))
    expect_identical(result$numExternalIndexRanges, 2L)
    expect_identical(result$indexID_2_rangeID, c(1L, 2L))
    expect_equal(result$indexRanges[[1]], indexRange(quote(5:9)))
    expect_equal(result$indexRanges[[2]], indexRange(quote(2:3)))
    
})


test_that("calcRanges are generated correctly", {  
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 3:5){}))
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 1:4){}))
    context_i <- modelContextClass$new(list(singleContext1))
    context_ijk <- modelContextClass$new(list(singleContext1, singleContext2, singleContext3))
    context_0 <- modelContextClass$new()

    declRule <- declRuleClass$new(quote(y ~ dnorm(mu, sigma)), 1, context_0)
    calcRule <- calcRuleClass$new(declRule, NULL, NULL, context_0)
    result <- calcRule$apply(varRangeClass$new(list(nimbleModel:::indexRange_none())))
    expect_identical(result$numExternalIndexRanges, 0L)
    expect_identical(result$indexRanges[[1]], nimbleModel:::indexRange_none())
    
    declRule_i <- declRuleClass$new(quote(y[i+1] ~ dnorm(0,1)), 1, context_i)
    
    calcRule <- calcRuleClass$new(declRule_i, NULL, NULL, context_i)

    ## Basic nodeRule style apply
    result <- calcRule$apply(varRangeClass$new(list(indexRange(quote(3:5)))))
    expect_identical(result$boolExternalIndexRanges, TRUE)
    expect_identical(result$numExternalIndexRanges, 1L)
    expect_identical(result$indexID_2_rangeID, 1L)
    expect_identical(result$indexRanges[[1]], indexRange(quote(3:5)))

    ## Generation of calcRules
   
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(2:4))), varName = 'y'))

    ## Mismatched varNames
    expect_error(calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(3:5))),
                                                               varName = 'foo')),
                 "does not match")
    
    ## No overlap
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(50:53)))))
    expect_equal(calcRange, NULL)

    ## No input range
    calcRange <- calcRule$generate_calcRange()
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(2:8))), varName = 'y'))

    ## Multiple indices
    declRule_i <- declRuleClass$new(quote(y[7:9, i+1] ~ dmnorm(z[1:3],pr[1:3,1:3])), 1, context_i)
    calcRule <- calcRuleClass$new(declRule_i, NULL, NULL, context_i)
 
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(2:4)), indexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange, NULL)

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(7:9)), indexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(2:4))), varName = 'y'))

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(2)), indexRange(quote(3:5)))))
    expect_equal(calcRange, NULL)

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(7)), indexRange(quote(3:5)))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(2:4))), varName = 'y'))
    
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(6)), indexRange(quote(3:5)))))
    expect_equal(calcRange, NULL)

    ## Using a nodeRange (modified so that output is not same as input)
    LHS <- quote(y[7:9, i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    nodeRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(7:9)),
                                                   indexRange(quote(5:9)))))
    calcRange <- calcRule$generate_calcRange(nodeRange)
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(4:8))), varName = 'y'))

    nodeRange$indexRanges[[1]][[1]][[2]] <- 35  # have nodeRange extend beyond true extent
    calcRange <- calcRule$generate_calcRange(nodeRange)
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(4:8))), varName = 'y'))

    ## Multiple loops
    declRule_ijk <- declRuleClass$new(quote(y[j, i+1, k, 2] ~ dnorm(0,1)), 1, context_ijk)
    calcRule <- calcRuleClass$new(declRule_ijk, NULL, NULL, context_ijk)
    
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(
                                                  indexRange(quote(3:5)),
                                                  indexRange(matrix(c(3,12,8))),
                                                  indexRange(quote(1:6)),
                                                  indexRange(2))))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(matrix(c(2,7))),
                                        indexRange(quote(3:5)),
                                        indexRange(quote(1:4))), varName = 'y'))
    

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(
                                                  indexRange(quote(3:5)),
                                                  indexRange(matrix(c(3,12,8))),
                                                  indexRange(quote(1:6)),
                                                  indexRange(3))))
    expect_equal(calcRange, NULL)

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(
                                                  indexRange(quote(3:5)),
                                                  indexRange(matrix(c(3,9,3,11,11,1,2,5,2,7), ncol = 2)),
                                                  indexRange(2))
                                              ))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(matrix(c(2,8,1,2), ncol = 2)),
                                        indexRange(quote(3:5))), indexOrders = list(c(1,3), 2), varName = 'y'))

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(
                                                  indexRange(quote(3:5)),
                                                  indexRange(matrix(c(3,12,5,7,2,2,3,2), ncol = 2)),
                                                  indexRange(quote(1:5))
                                              ), indexOrders = list(1,c(2,4), 3)))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(matrix(c(2,6))),
                                        indexRange(quote(3:5)),
                                        indexRange(quote(1:4))), varName = 'y'))

    ## j in 1:n[i] type case
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    singleContext2ni <-
        modelSingleContext(indexVarExpr = quote(j),
                           indexRangeExpr = quote(1:n[i]),
                           )
    context_ijni <- modelContextClass$new(list(singleContext1,
                                               singleContext2ni))
    n <- c(1,3,2)
    declRule_ijni <- declRuleClass$new(quote(y[j, i+1] ~ dnorm(0,1)), 1, context_ijni, constants = list(n = n))
    calcRule <- calcRuleClass$new(declRule_ijni, NULL, NULL, context_ijni, constants = list(n = n))

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(
                                                               indexRange(quote(1:4)),
                                                               indexRange(quote(1:3))
                                                           )))
    expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(matrix(c(1,2,2,2,1,1,2,3), ncol = 2))),
                                   varName = 'y'))                                        
})



## Hopefully comprehensive testing of exclude()
test_that("calcRule fracturing works", {
    ## I think nodeRuleClass can stay as a generic nodeRule rather than needed to be a calcRule,
    ## even though in real work, input would be a calcRule.
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:4){}))
    
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))
    context_j <- modelContextClass$new(list(singleContext2))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    
    LHS <- quote(mu)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    ## fracture with mu itself
    fracRange <- LHSrule$apply(varRangeClass$new(list(nimbleModel:::indexRange_none())))
    result <- fracture(LHSrule, fracRange)
    expect_identical(result, NULL)
    
    ## scalar overlap at end
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    ## fracture with mu[3]
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(3))))
    
    result <- fracture(LHSrule, fracRange)

    expect_identical(length(result), 2L)
    expr <- quote(mu[i])
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:3){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:9){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## seq overlap at end
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    ## fracture with mu[3:4]
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(3:4)))))
    
    result <- fracture(LHSrule, fracRange)

    expect_identical(length(result), 2L)
    expr <- quote(mu[i])
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:4){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 5:9){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    ## seq overlap in middle
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    ## fracture with mu[4:5]
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(4:5)))))
    
    result <- fracture(LHSrule, fracRange)

    expect_identical(length(result), 3L)
    expr <- quote(mu[i])
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:5){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:3){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:9){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp)
    expect_identical(result[[3]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    

    ## seq and matrix
    LHS <- quote(mu[i+1])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    ## fracture with matrix
    idx <- c(3,6,9)
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(matrix(idx)))))
    
    result <- fracture(LHSrule, fracRange)

    expect_identical(length(result), 2L)
    expr <- quote(mu[idx[i]])
    idx2 <- c(4,5,7,8)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:4){}))))
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx2))
    expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)

    ## basic case with one external, one internal: mu[1:3, i]
    LHS <- quote(mu[1:3,i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                      indexRange(quote(2:3)))))
    
    result <- fracture(LHSrule, fracRange)

    expect_identical(length(result), 2L)
    expect_identical(result[[1]]$internalRules$indexRules[[1]]$setupResults,
                     LHSrule$internalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[2]]$internalRules$indexRules[[1]]$setupResults,
                     LHSrule$internalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:3){}))))
    expected <- nodeRuleClass$new(LHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:8){}))))
    expected <- nodeRuleClass$new(LHS, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## two external indices, one fractured, two constant internal rules
    LHS <- quote(mu[1:3,j,i,2:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                      indexRange(quote(1:4)),
                                                      indexRange(matrix(c(2,4))),
                                                      indexRange(2))))
    
    result <- fracture(LHSrule, fracRange)
    
    expect_identical(length(result), 2L)
    expect_identical(LHSrule$index2setID, c(0,2,1,0))
    for(k in 1:2) {
        for(kk in 1:2)
            expect_identical(result[[k]]$internalRules$indexRules[[kk]]$setupResults,
                             LHSrule$internalRules$indexRules[[kk]]$setupResults)
        expect_identical(result[[k]]$externalRules$indexRules[[1]]$setupResults,
                         LHSrule$externalRules$indexRules[[2]]$setupResults)
        expect_identical(result[[k]]$index2setID, c(0,1,2,0))
    }
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:2){}))))
    idx <- as.integer(c(2,4))
    expr <- quote(mu[idx[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    idx <- as.integer(c(3,5,6,7,8))
    expr <- quote(mu[idx[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)

    ## two external indices, one fractured, one constant internal rule and additional external from scalar
    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                      indexRange(quote(1:4)),
                                                      indexRange(matrix(c(2,4))),
                                                      indexRange(2))))
    
    result <- fracture(LHSrule, fracRange)

    ## Just check stuff related to the scalar constant index, given similarity to above test.
    expect_identical(length(result), 2L)
    expect_identical(LHSrule$index2setID, c(0,2,1,0))
    for(k in 1:2) {
        expect_identical(result[[k]]$internalRules$indexRules[[1]]$setupResults,
                         LHSrule$internalRules$indexRules[[1]]$setupResults)
        expect_identical(result[[k]]$externalRules$indexRules[[1]]$setupResults,
                         LHSrule$externalRules$indexRules[[2]]$setupResults)
        expect_identical(result[[k]]$index2setID, c(0,1,2,0))
    }

    
    ## two external indices, both fractured: mu[1:3, j ,2, i]
    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                      indexRange(quote(2:3)),
                                                      indexRange(quote(2:3)),
                                                      indexRange(2))))
    
    result <- fracture(LHSrule, fracRange)

    expect_identical(length(result), 2L)
    expect_identical(LHSrule$index2setID, c(0,2,1,0))
    for(k in 1:2) {
        expect_identical(result[[k]]$internalRules$indexRules[[1]]$setupResults,
                         LHSrule$internalRules$indexRules[[1]]$setupResults)
        expect_identical(result[[k]]$index2setID, c(0,1,1,0))
    }

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:4){}))))
    idx1 <- as.integer(c(2,3,2,3))
    idx2 <- as.integer(c(2,2,3,3))
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:24){}))))
    idx1 <- as.integer(c(1,4,1,4,rep(1:4, 5)))
    idx2 <- as.integer(c(2,2,3,3,rep(4:8, each = 4)))
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)


    ## two external indices fractured: mu[1:3, j ,2, i] , based on 2-d matrix
    LHS <- quote(mu[1:3,j,i,2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)
    
    fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                      indexRange(matrix(c(2,3,3,7), ncol = 2)),
                                                      indexRange(2))))

    result <- fracture(LHSrule, fracRange)

    expect_identical(length(result), 2L)
    expect_identical(LHSrule$index2setID, c(0,2,1,0))
    for(k in 1:2) {
        expect_identical(result[[k]]$internalRules$indexRules[[1]]$setupResults,
                         LHSrule$internalRules$indexRules[[1]]$setupResults)
        expect_identical(result[[k]]$index2setID, c(0,1,1,0))
    }

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:2){}))))
    idx1 <- as.integer(c(2,3))
    idx2 <- as.integer(c(3,7))
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:26){}))))
    idx1 <- as.integer(rep(1:4, 7))
    idx2 <- as.integer(rep(2:8, each = 4))
    wh <- (idx1 == 2 & idx2 == 3) | (idx1 == 3 & idx2 == 7)
    idx1 <- idx1[!wh]
    idx2 <- idx2[!wh]
    expr <- quote(mu[idx1[i],idx2[i]])
    expected <- nodeRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1, idx2 = idx2))
    expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                 expected$externalRules$indexRules[[1]]$setupResults)

})




test_that("RHS exclusion works", {
    ## I think nodeRuleClass can stay as is here, even though in real work, input would be a calcRule

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:4){}))
    
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

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)

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

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:2){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)


    ## seq/seq partial overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 5:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)


    ## seq/seq full overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:9){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)
    expect_identical(result, NULL)

    ## matrix in LHS
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[idx[i]])
    idx <- c(2,5,4)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp, constants = list(idx = idx))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:2){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)

    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 5:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)

    expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)


    ## matrix in RHS
    idx <- c(14,4,2,9,1,3,7,11)
    RHS <- quote(mu[idx[i]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx = idx))
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:4){}))))
    idx <- as.integer(c(4,7,9,11))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx = idx))

    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## two-d arbitrary case, extracting block elements from RHS matrix
    RHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx1 = idx1, idx2 = idx2))
    LHS <- quote(mu[i,j])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    idx1 <- c(11,11,12,13,5)
    idx2 <- c(2,5,6,7,13)
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    

    ## two-d arbitrary case, extracting matrix elements from RHS block
    RHS <- quote(mu[i,j])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij)
    LHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i, constants = list(idx1 = idx1, idx2 = idx2))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:26){}))))
    idx1 <- c(2,3,5,6,7,8,2,4:8,rep(2:8, 2))
    idx2 <- c(rep(1,6),rep(2,6),rep(3,7), rep(4,7))
    expected <- rhsRuleClass$new(LHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case with constant
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

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
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:5){}))))
    LHS <- quote(mu[i+1, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case with shared matrix index
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1,2)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx = idx))
    LHS <- quote(mu[5, idx[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = list(idx = idx))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx = idx))
    expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    
    ## Awkward intersections

    ## Partial overlap in some rows; for now this is simply unrolled.
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:17){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## LHS element inside RHS; for now this is simply unrolled
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[3, 3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:20){}))))
    idx1 <- c(2:8,2:8,2,4:8)
    idx2 <- c(rep(1,7), rep(2,7), rep(3, 6))
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)


    ## LHS fully overlaps RHS block constant in additional dimension; this is handled nicely.
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:4])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:6){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)

    ## basic 3-d case - two identical indices
    RHS <- quote(mu[1:3, i, 1:2])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[1:3, i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
 
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:6){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     RHSrule$externalRules$indexRules[[3]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[3]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$index2setID, c(1,3,2))

    ## 3-d case with partial overlap in some rows but with additional identical index (j)
    RHS <- quote(mu[i, j, 1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    LHS <- quote(mu[i, j, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:17){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    expr <- quote(mu[idx1[i], j, idx2[i]])
    expected <- rhsRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)

    ## 3-d case with multi-column shared matrix indexRange
    idx1 <- c(4,7,1,2)
    idx2 <- c(1,9,3,2)
    
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    LHS <- quote(mu[idx1[j], 2, idx2[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = list(idx1 = idx1, idx2 = idx2))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## 3-d case with multi-column unshared matrix indexRange
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    idx3 <- c(4,7,2,2)
    idx4 <- c(1,10,3,2)
    LHS <- quote(mu[idx3[j], i, idx4[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij, constants = list(idx3 = idx3, idx4 = idx4))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:2){}))))
    idx5 <- c(1,7)
    idx6 <- c(3,9)
    expr <- quote(mu[idx5[j], i, idx6[j]])
    expected <- rhsRuleClass$new(expr, 1, context_tmp, constants = list(idx5 = idx5, idx6 = idx6))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[2]]$setupResults)
    
    ## This is not error-trapped (use of index and constant in wrong way)
    RHS <- quote(mu[i[idx]])
    idx <- c(4,7,1)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx = idx))
    
    ## incorrect length of constant (move this check to test-graphRules, probably).
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1)
    expect_error(RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx = idx)),
                 "Missing values found in setting up arbitrary indexRule")
})

## FAILS for the moment because of logProb initialization currently hacked into $calculate (see next test)
test_that("declaration-specific calculate generated correctly", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:4){}))
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))

    context_0 <- modelContextClass$new()
    rule <- declRuleClass$new(quote(y ~ dnorm(mu, sigma)), 1, context_0)
    expect_identical(body(rule$calculate), quote(logProb_y <- dnorm(y, mu, sigma)))
   
   
    rule <- declRuleClass$new(quote(y[j,i] ~ dnorm(x[i], 1)), 1, context_ij)
    expect_identical(body(rule$calculate), quote(logProb_y[idx[2], idx[1]] <- dnorm(y[idx[2], idx[1]], x[idx[1]], 1)))
    
})


test_that("calculate works correctly", {
    ## NOTE: until nodeFun generation and model building are integrated, this testing relies on
    ## inserting logProb_y as hard-coded initialization in the declRule$calculate. 
    context_0 <- modelContextClass$new()
    y <- rnorm(1)
    assign('y', y, pos = .GlobalEnv)
    logProb_y <- dnorm(y, 0, 1)
    
    declRule <- declRuleClass$new(quote(y ~ dnorm(0, 1)), 1, context_0)
    calcRule <- calcRuleClass$new(declRule, NULL, NULL, context_0)
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(nimbleModel:::indexRange_none())))
    expect_identical(calcRange$calculate(), logProb_y)

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:6){}))
    context_i <- modelContextClass$new(list(singleContext1))
    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(0, 1)), 1, context_i)

    y <- rnorm(6)
    assign('y', y, pos = .GlobalEnv)
    logProb_y <- dnorm(y, 0, 1)
    
    calcRule <- calcRuleClass$new(declRule, NULL, NULL, context_i)
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(quote(3:5)))))
    expect_identical(calcRange$calculate(), logProb_y[3:5])

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(list(indexRange(matrix(c(3,5))))))
    expect_identical(calcRange$calculate(), logProb_y[c(3,5)])

    ## also test with 1e6 dnorms once have indexRange as proper R6 class to see if faster (avoid current list copies)
    ## current nimble 1e5 dnorms is 34 sec. (compare to vec dnorm in R of .02 f0r 1e6 and for loop of 0.88 sec for 1e6)

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:6){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 3:9){}))
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))
    declRule <- declRuleClass$new(quote(y[j,i] ~ dnorm(x[i], 1)), 1, context_ij)

    y <- matrix(rnorm(6*9), 9)
    x <- rep(0, 6)
    assign('y', y, pos = .GlobalEnv)
    assign('x', x, pos = .GlobalEnv)
    logProb_y <- dnorm(y, 0, 1)

    calcRule <- calcRuleClass$new(declRule, NULL, NULL, context_ij)
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(
                        list(indexRange(matrix(c(6,2,5,4,7,5), ncol = 2)))))
    expect_identical(calcRange$calculate(), logProb_y[matrix(c(6,5,4,5), ncol = 2)])

    calcRange <- calcRule$generate_calcRange(varRangeClass$new(
                        list(indexRange(quote(8:9)), indexRange(quote(2:3)))))
                                             
    expect_identical(calcRange$calculate(), c(logProb_y[8:9,2:3]))

    ## internal block and deterministic
    y <- array(0, c(6,3,9))
    x <- matrix(rnorm(6*9), 6)
    assign('y', y, pos = .GlobalEnv)
    assign('x', x, pos = .GlobalEnv)

    declRule <- declRuleClass$new(quote(y[i,2:3,j] <- x[i,j]), 1, context_ij)
    calcRule <- calcRuleClass$new(declRule, NULL, NULL, context_ij)
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(
                        list(indexRange(matrix(c(3,2,4,3,3,4,5,2,5,5,3,5), byrow = TRUE, ncol = 3)))))
    expect_identical(calcRange$calculate(), x[matrix(c(3,4,3,4,5,5,5,5), byrow = TRUE, ncol = 2)])


    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:2){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:3){}))
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 1:4){}))
    context_ijk <- modelContextClass$new(list(singleContext1, singleContext2, singleContext3))

    y <- array(rnorm(2*3*4), c(2,3,4))
    assign('y', y, pos = .GlobalEnv)
    logProb_y  <- dnorm(y, 0, 1)
    declRule <- declRuleClass$new(quote(y[i,j,k] ~ dnorm(0,1)), 1, context_ijk)
    calcRule <- calcRuleClass$new(declRule, NULL, NULL, context_ijk)
    calcRange <- calcRule$generate_calcRange(varRangeClass$new(
                                                               list(indexRange(matrix(c(2,1,3,1), ncol = 2)),
                                                                    indexRange(quote(2:3))),
                                                               indexOrders = list(c(1,3), 2)))
    expect_identical(calcRange$calculate(), logProb_y[matrix(c(2,2,3,2,3,3,1,2,1,1,3,1), ncol = 3, byrow = TRUE)])

    ## TODO: add tests with mv nodes

})


test_that("getFullRange works correctly", {
    context_0 <- modelContextClass$new()
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    context_i <- modelContextClass$new(list(singleContext1))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:4){}))
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))

    LHS <- quote(mu)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$getFullRange(),
                     varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'mu'))

    
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$getFullRange(),
                     varRangeClass$new(list(indexRange(5), indexRange(quote(1:3))), varName = 'mu'))

    LHS <- quote(mu[4:5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$getFullRange(),
                     varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:3))), varName = 'mu'))
    
    LHS <- quote(mu[4:5, i, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(2:8)), indexRange(quote(1:3))), varName = 'mu'))
    
    expr <- quote(mu[4:5, j, i, 3])
    LHSrule <- nodeRuleClass$new(expr, 1, context_ij)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:4)),
                                        indexRange(quote(2:8)), indexRange(quote(3))), varName = 'mu'))
    
    LHS <- quote(mu[4:5, i, i, 3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(matrix(rep(2:8, 2), ncol = 2)), indexRange(3)), varName = 'mu'))

})





