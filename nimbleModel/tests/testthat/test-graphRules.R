context("graphRules")

bruteForceNestedIndexing <- function(indexVector, indexQuery) {
    ## See below for where this is used to brute-force check some tests.
    ##
    ## Trickier use of which and sorting
    ## did not quickly handle all needs.
    ## It was decided that for testing purposes a slow
    ## but simple and readable function would be useful.
    ## Hence the following iterations:
    ans <- integer()
    for(v in indexQuery) {
        thisAns <- which(indexVector == v)
        ans <- append(ans, thisAns)
    }
    ans
}

irNone <- nimbleModel:::indexRangeNoneClass$new()

singleContext1 <-
    singleContextClass$new(forCode = quote(for(i in 1:10){}))

singleContext1_short <-
    singleContextClass$new(forCode = quote(for(i in 1:3){}))

singleContext2 <-
    singleContextClass$new(forCode = quote(for(j in 1:4){}))

singleContext3 <-
    singleContextClass$new(forCode = quote(for(k in 1:3){}))

## alternative way to specific a single context
singleContext2ni <-
    singleContextClass$new(indexVarExpr = quote(j),
                       indexRangeExpr = quote(1:n[i]),
                       )

singleContext3ni <-
    singleContextClass$new(forCode = quote(for(k in 1:mi[i]){}))                          
singleContext3nj <-
    singleContextClass$new(forCode = quote(for(k in 1:mj[j]){}))                          
singleContext3nij <-
    singleContextClass$new(forCode = quote(for(k in 1:mij[i,j]){}))                          

context_0 <- modelContextClass$new()

context_i <- modelContextClass$new(list(singleContext1))

context_ij <- modelContextClass$new(list(singleContext1,
                                         singleContext2))

context_ijk <- modelContextClass$new(list(singleContext1,
                                          singleContext2,
                                          singleContext3))

context_ijni<- modelContextClass$new(list(singleContext1,
                                          singleContext2ni))


context_i_short <- modelContextClass$new(list(singleContext1_short))

context_ij_short <- modelContextClass$new(list(singleContext1_short,
                                         singleContext2))

context_ijk_short <- modelContextClass$new(list(singleContext1_short,
                                          singleContext2,
                                          singleContext3))

context_ijni_short <- modelContextClass$new(list(singleContext1_short,
                                          singleContext2ni))

context_ijnik_short <- modelContextClass$new(list(singleContext1_short,
                                           singleContext2ni,
                                           singleContext3))
context_ijnikni_short <- modelContextClass$new(list(singleContext1_short,
                                             singleContext2ni,
                                             singleContext3ni))
context_ijniknj_short <- modelContextClass$new(list(singleContext1_short,
                                             singleContext2ni,
                                             singleContext3nj))
context_ijniknij_short <- modelContextClass$new(list(singleContext1_short,
                                              singleContext2ni,
                                              singleContext3nij))
 

test_that("makeSeparableIndexSets works", {
    
    expect_identical(makeSeparableIndexSets(quote(y[i]),
                                            quote(x),
                                            context_i)$indexVarNameSets,
                     list(c(i = "i")))

    expect_identical(makeSeparableIndexSets(quote(y),
                                            quote(x[i]),
                                            context_i)$indexVarNameSets,
                     list(c(i = "i")))

    expect_identical(makeSeparableIndexSets(quote(y),
                                            quote(x),
                                            context_0)$indexVarNameSets,
                     list())

    expect_identical(makeSeparableIndexSets(quote(y[j, i + j]),
                                            quote(x[i, j]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i", j = "j")))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, 3]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[j + i]),
                                            quote(x[i, 3]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i", j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, 1:3]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[j + i]),
                                            quote(x[,]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i", j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[i, j]),
                                            quote(x[i + j]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i", j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                               quote(x[ ]),
                               context_ij)$indexVarNameSets
       ,
                     list(c(i = "i"), c(j = "j")))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, const]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, j, k]),
                                            quote(x[k, i, j]),
                                            context_ijk)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j"), c(k = "k")))

    ## Tied together by ragged indexing.
    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, j]),
                                            context_ijni)$indexVarNameSets,
                     list(c(i = "i", j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, j]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))

    ## getParents situations
    expect_identical(makeSeparableIndexSets(quote(x[2]),
                                            quote(y[i, j]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))

    expect_identical(makeSeparableIndexSets(quote(x[2]),
                                            quote(y[i+j]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i", j = "j")))


    expect_identical(makeSeparableIndexSets(quote(x[i,j]),
                                            quote(y[i+j,j+k]),
                                            context_ijk)$indexVarNameSets,
                     list(c(i = "i", j = "j", k = "k")))

})

test_that("graphRuleClass works", {
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i+1]),
                               context = context_i)

    expect_error(
        rule$apply(newIndexRange(c(2, 4))),
        "needs to be a `varRangeClass` object"
    )

    expect_equal(
        rule$apply(varRangeClass$new(list(newIndexRange(quote(2:3))), varName = 'x')),
        varRangeClass$new(list(newIndexRange(quote(1:2))), varName = 'y')
    )

    ## full variable
    expect_equal(
        rule$apply('x'),
        varRangeClass$new(list(newIndexRange(quote(1:10))), varName = 'y')
    )
})

test_that("unused indices do not cause problems", {
    rule <- graphRuleClass$new(toExpr = quote(y[2]),
                               fromExpr = quote(x[3]),
                               context = context_i)

    expect_equal(rule$apply('x[3]'), varRangeClass$new('y[2]'))
    expect_identical(rule$apply('x[2]'), NULL)

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i]),
                               context = context_ij_short)

    expect_equal(rule$apply('x[3]'), varRangeClass$new('y[3]'))
    expect_identical(rule$apply('x[22]'), NULL)
})



test_that("error trap incorrect number of input indices", {
    ## Single simple sequence rule
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i]),
                               context = context_i)

    expect_error(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(3),
                              newIndexRange(4))), rule),
        "incorrect number of input indices"
    )

    expect_error(
        applyGraphRule(
            varRangeClass$new(list(irNone), varName = 'x'), rule),
        "incorrect number of input indices"
    )
    
    ## incorrect length of input matrix indexRange
    expect_error(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(3,4), nrow = 1)))), rule),
        "incorrect number of input indices"
    )
    
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[2, 3]),
                               context = context_i)
    
    expect_error(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(3))), rule), 
        "incorrect number of input indices"
    )
    
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x),
                               context = context_i)
    
    expect_error(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(2))), rule),
        "incorrect number of input indices"
    )
    
})

test_that("graphRules works for basic cases lacking indexing", {
    rule <- graphRuleClass$new(toExpr = quote(y),
                               fromExpr = quote(x),
                               context = context_0)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(irNone), varName = 'x'), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y),
                               fromExpr = quote(x[2]),
                               context = context_0)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(2))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(3))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(y[2]),
                                fromExpr = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(irNone), varName = 'x'), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'y')
    )
    
})

test_that("graphRules works for single index cases, wrapping indexRules", {
    ## indexRuleBlock case.
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i+1]),
                               context = context_i)
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:6)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2:5))), varName = 'y')
    )

    ## Checking that NAs produced by invalid input matrix entries are discarded
    ## in graphRules processing.
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(10, 12, 14), ncol = 1)))), rule),
        varRangeClass$new(list(
                          newIndexRange(9)), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(5, 10, 12, 14), ncol = 1)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(4,9)))), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(12, 14), ncol = 1)))), rule),
        NULL
    )

    ## `1+i` instead of usual `i+1`.
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[1+i]),
                               context = context_i)

    expect_true(is(rule$indexRules[[1]], "indexRuleBlockClass"))
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(3:6)))),
            rule),
        varRangeClass$new(list(newIndexRange(quote(2:5))), varName = 'y')
    )

    ## indexRuleAll cases.
    rule <- graphRuleClass$new(toExpr = quote(y[i+1]),
                               fromExpr = quote(x[3]),
                               context = context_i)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(3)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2:11))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[2]),
                               fromExpr = quote(x[3:n]),
                               context = context_0,
                               constants = list(n = 5))
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(4)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2))), varName = 'y'))

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(7)))), rule),
        NULL
    )

    ## more complicated expression than simple index
    rule <- graphRuleClass$new(toExpr = quote(y[3*i]),
                               fromExpr = quote(x[3]),
                               context = context_i)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(3))), rule),
        varRangeClass$new(list(newIndexRange(matrix(seq(3, 30, by = 3), ncol = 1))), varName = 'y')
    )
    
    ## indexRuleConstant case.
    
    rule <- graphRuleClass$new(toExpr = quote(y[2]),
                               fromExpr = quote(x[3]),
                               context = context_0)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2))), varName = 'y'))

    ## indexRuleArbitrary cases.
    
    idx <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 1 and 2 are repeated
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[ idx[i] ]),
                               context = context_i,
                               constants = list(idx = idx))
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:5)))), rule), ## 2 yields multiples; 3 and 4 yield nothing
        varRangeClass$new(list(
                          newIndexRange(matrix(bruteForceNestedIndexing(idx, 2:5)))), varName = 'y')
    )
    
    rule <- graphRuleClass$new(toExpr = quote(y[idx[i]]),
                               fromExpr = quote(x[i]),
                               context = context_i,
                               constants = list(idx = idx))
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(3:7)))), rule), 
        varRangeClass$new(list(
                          newIndexRange(matrix(idx[3:7]))), varName = 'y')
    )

})

test_that("graphRules works for various basic multiple index cases", {

    ## two 'all' rules, crossed (of course)
    rule <- graphRuleClass$new(toExpr = quote(y[i+1,j]),
                               fromExpr = quote(x[2]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(varRangeClass$new(list(newIndexRange(2))), rule),
        varRangeClass$new(list(newIndexRange(quote(2:11)),
                               newIndexRange(quote(1:4))), varName = 'y')
    )
   
    expect_identical(
        applyGraphRule(varRangeClass$new(list(newIndexRange(3))), rule),
        NULL
    )

    ## all plus constant, crossed (of course)
    rule <- graphRuleClass$new(toExpr = quote(y[i,2]),
                               fromExpr = quote(x),
                               context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(irNone), varName = 'x'), rule),
        varRangeClass$new(list(newIndexRange(quote(1:10)),
                               newIndexRange(quote(2))), varName = 'y')
    )

    ## two constant rules, crossed (of course)
    rule <- graphRuleClass$new(toExpr = quote(y[2,3]),
                               fromExpr = quote(x),
                               context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(irNone), varName = 'x'), rule),
        varRangeClass$new(list(newIndexRange(quote(2)),
                               newIndexRange(quote(3))), varName = 'y')
    )

    ## block plus constant, crossed (of course)
    rule <- graphRuleClass$new(toExpr = quote(y[2,i+1]),
                               fromExpr = quote(x[i]),
                               context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(1:3)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2)),
                               newIndexRange(quote(2:4))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(22:24)))), rule),
        NULL
    )

    ## block plus all, crossed (of course)
    rule <- graphRuleClass$new(toExpr = quote(y[j,i]),
                               fromExpr = quote(x[i]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(newIndexRange(quote(1:4)),
                               newIndexRange(quote(2:3))), varName = 'y')
    )

    ## two block rules, crossed
    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[i, j]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:4)),
                              newIndexRange(quote(3:6)))), rule),
        varRangeClass$new(list(
               newIndexRange(quote(2:4)),
               newIndexRange(quote(3:4))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:4)),
                              newIndexRange(quote(5:9)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:4)),
                              newIndexRange(matrix(c(1,3,5), ncol = 1)))), rule),
        varRangeClass$new(list(
               newIndexRange(quote(2:4)),
               newIndexRange(matrix(c(1,3), ncol = 1))), varName = 'y')
    )
})


test_that("graphRules works for constraints tangled up with rule results because of multi-index indexRange", {
    
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i, 2]),
                               context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(3,2,2,3,4,3,10,2), ncol = 2, byrow = TRUE)))), rule),
        varRangeClass$new(list(newIndexRange(3)), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(13,2,2,3,4,3,10,2), ncol = 2, byrow = TRUE)))), rule),
        NULL
    )
        
    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[i, j, 2]),
                               context = context_ij_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(3,2,2,2,2,3,4,2,3,10,2,2,3,4,2), ncol = 3, byrow = TRUE)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(3,3,2,4), ncol = 2))), varName = 'y')
    )

    
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[k[i], 2]),
                               context = context_i_short,
                               constants = list(k = c(2,2,3)))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(1,2,2,1,2,2,3,2), ncol = 2, byrow = TRUE)))), rule),
        varRangeClass$new(list(
                          newIndexRange(quote(1:3))), varName = 'y')
    )    
})


test_that("graphRules works for arbitrary rules of multiple contexts entangled with other indices", {

    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                                fromExpr = quote(x[i+3*j,j]),
                                context = context_ij)
    expect_identical(length(rule$indexRules), 1L)
    expect_true(is(rule$indexRules[[1]], "indexRuleArbitraryClass"))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(1:7)),
                              newIndexRange(quote(1:3)))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(1,1,2,1,3,1,4,1,1,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(1,2,2,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(4,2), ncol = 2)))), rule),
        NULL
    )

    ## version using k[i,j]
    k <- matrix(outer(1:10, 1:4, function(i,j) i+3*j), nrow = 10, ncol = 4)
    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                                fromExpr = quote(x[k[i,j],j]),
                                context = context_ij,
                                constants = list(k = k))
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(1:7)),
                              newIndexRange(quote(1:3)))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(1,1,2,1,3,1,4,1,1,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(1,2,2,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    ## getParents version
    rule <- graphRuleClass$new(toExpr = quote(y[i+3*j, j]),
                                fromExpr = quote(x[i,j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:3)),
                              newIndexRange(quote(1:2)))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(5,1,6,1,8,2,9,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(2,1,3,1,3,2,11,3), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(5,1,6,1,9,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )


    ## Additional LHS index not used on RHS, 'all' scenario embedded within arbitrary rule.
    rule <- graphRuleClass$new(toExpr = quote(y[i+3*j, j]),
                                fromExpr = quote(x[i]),
                                context = context_ij)
    expect_identical(length(rule$indexRules), 1L)
    expect_true(is(rule$indexRules[[1]], "indexRuleArbitraryClass"))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(1:2)))), rule), 
        varRangeClass$new(list(
               newIndexRange(matrix(c(4,1,7,2,10,3,13,4,5,1,8,2,11,3,14,4), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    ## More complicated such case where first i,j need to be crossed.
    rule <- graphRuleClass$new(toExpr = quote(y[i+j, j+k]),
                                 fromExpr = quote(x[i,j]),
                                 context = context_ijk)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:3)),
                              newIndexRange(matrix(c(2,4,6))))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(4,3,4,4,4,5,5,3,5,4,5,5,6,5,6,6,6,7,7,5,7,6,7,7), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(1)),
                              newIndexRange(quote(6)))), rule),
        NULL
    )

    ## getParents version
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i+3*j, j]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               newIndexRange(quote(1:2))), varName = 'y')
    )
    

   ## include an additional index tied in in some cases via the input varRange
   rule <- graphRuleClass$new(toExpr = quote(y[j+3*i,j,k]),
                              fromExpr = quote(x[i, k, j]),
                              context = context_ijk)

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(matrix(c(2,3), ncol = 1)),
                             newIndexRange(matrix(c(1,3,1,2), nrow = 2)))), rule),
       varRangeClass$new(list(newIndexRange(matrix(c(7,10,8,11,1,1,2,2,1,1,3,3), ncol = 3))), varName = 'y'))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(matrix(c(2,3,1,2,1,3), nrow = 2)))), rule),
       varRangeClass$new(list(newIndexRange(matrix(c(7,1,1,12,3,2), byrow = TRUE, ncol = 3))), varName = 'y'))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(matrix(c(2,3), ncol = 1)),
                             newIndexRange(matrix(c(1,2), ncol = 1)),
                             newIndexRange(matrix(c(1,3), ncol = 1)))), rule),
       varRangeClass$new(list(newIndexRange(matrix(c(7,10,9,12,1,1,3,3), ncol = 2)),
                              newIndexRange(quote(1:2))), varName = 'y'))


   rule <- graphRuleClass$new(toExpr = quote(x[j]),
                              fromExpr = quote(y[k[i], j, 3*i]),
                              context = context_ij_short,
                              constants = list(k = c(2,4,5)))

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(matrix(c(2,9,4,2,4,9,6,2,5,4),ncol = 2,byrow = TRUE)),
                             newIndexRange(quote(6:7)))
                             ), rule),
       varRangeClass$new(list(newIndexRange(2)), varName = 'x')
   )

   
})


test_that("graphRules works for arbitrary rule with ragged blocks combined with another rule", {

    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[i, 1:j]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:4)),
                              newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(
                          newIndexRange(quote(2:4)),
                          newIndexRange(matrix(c(2,3,4,3,4), ncol = 1))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(2, 3, 1, 2, 99, 3), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(2,3,2,4,1,2,1,3,1,4), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    ## getParents versions
    
    rule <- graphRuleClass$new(toExpr = quote(y[i, 1:j]),
                               fromExpr = quote(x[i, j]),
                               context = context_ij)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:4)),
                              newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(
                          newIndexRange(quote(2:4)),
                          newIndexRange(matrix(c(1,2,1,2,3), ncol = 1))), varName = 'y')
    )
    

    rule <- graphRuleClass$new(toExpr = quote(y[i, 1:i, j]),
                               fromExpr = quote(x[i, j]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:3)),
                              newIndexRange(quote(2:4)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(2,1,2,2,3,1,3,2,3,3), byrow = TRUE, ncol = 2)),
                          newIndexRange(quote(2:4))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(2, 3, 1, 4, 99, 3), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(2,1,3,2,2,3,1,1,4), byrow = TRUE, ncol = 3))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[i, 1:i, j]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(2,1,2,2,1,1), nrow = 3, byrow = TRUE)),
                              newIndexRange(quote(2:4)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(2,2,1), ncol = 1)),
                          newIndexRange(quote(2:4))), varName = 'y')
    )
})

test_that("graphRules works for two rules that use a single indexRange matrix", {
    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[i, j]),
                               context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(7,3,4,2), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(7,3,4,2), ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(11,3,4,2), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(3,2), ncol = 2))), varName = 'y')
    )

    ## single empty indexRange because results of the two rules get combined back together
    ## as we need to check by row if combined result is valid
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(11,3,4,5), ncol = 2)))), rule),
        NULL
    )

    idx1 <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 2 is repeated and 3,4 absent
    idx2 <- c(1, 1, 2, 1) 

    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[ idx1[i] , idx2[j]]),
                               context = context_ij,
                               constants = list(idx1 = idx1, idx2 = idx2))
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(5,5,5,3,3,3,2,2,2,1,4,2,1,4,2,1,4,2), ncol = 2)))), rule), 
        varRangeClass$new(list(newIndexRange(matrix(c(9,9,9,9,3,7,3,7,3,7,3,7,1,2,4,3,1,1,2,2,4,4,3,3), ncol = 2))),
                          varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[ idx1[i] , j]),
                               context = context_ij,
                               constants = list(idx1 = idx1))

    ## Apply rule to a sequence indexRange
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(5,5,3,2,2,2,5,2,5,4), ncol = 2)))), rule), 
        varRangeClass$new(list(newIndexRange(matrix(c(9,3,7,2,4,4), ncol = 2))), varName = 'y')
    )
    
    ## effect of having a RHS constraint, with indexRange matrix inputs
    rule <- graphRuleClass$new(toExpr = quote(y[i, j]),
                               fromExpr = quote(x[3, i, j]),
                               context = context_ij)

    ## constraint results in two empty ranges instead of one, despite input indexRange matrix
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(4), 
                              newIndexRange(matrix(c(1,1,2,2), ncol = 2)))), rule),
        NULL
    )
    
    ## constraint results in two empty ranges instead of one, despite input indexRange matrix
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(4,7,1,1,2,2), ncol = 3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(4,3,1,1,2,2), ncol = 3)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(1,2), ncol = 2))), varName = 'y')
    )

})

test_that("graphRules works for all rule that is function of two indexes", {
    rule <- graphRuleClass$new(toExpr = quote(y[j+3*i]),
                               fromExpr = quote(x[2]),
                               context = context_ij_short)

    ## Note that there are duplicate results.
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(4:7,7:10,10:13)))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[i+1,i]),
                               fromExpr = quote(x[2]),
                               context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(2,3,4,1,2,3), ncol = 2))), varName = 'y')
    )
    
})

test_that("graphRules works for fixed RHS indices", {
    ## Start by just using the simplest LHS given focus here is on RHS constraints.

    rule <- graphRuleClass$new(toExpr = quote(y[2]),
                               fromExpr = quote(x),
                               context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(irNone), varName = 'x'), rule),
        varRangeClass$new(list(newIndexRange(quote(2))), varName = 'y'))

    
    rule <- graphRuleClass$new(toExpr = quote(y),
                               fromExpr = quote(x[2]),
                               context = context_0)

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(3))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(3:4, ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(2:3, ncol = 1)))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    ## TODO: figure out if dealing with blank index case.
    if(FALSE) {
        rule <- graphRuleClass$new(toExpr = quote(y),
                                   fromExpr = quote(x[]),
                                   context = context_0)
        
        expect_equal(
            applyGraphRule(
                varRangeClass$new(list(newIndexRange(3))), rule),
            varRangeClass$new(list(irNone), varName = 'y')
        )
        
        expect_equal(
            applyGraphRule(
                varRangeClass$new(list(newIndexRange(3:20))), rule),
            varRangeClass$new(list(irNone), varName = 'y')
        )
        
        expect_equal(
            applyGraphRule(
                varRangeClass$new(list(newIndexRange(matrix(c(3,20), ncol = 1)))), rule),
            varRangeClass$new(list(irNone), varName = 'y')
        )
        
        expect_error(  ## Not yet checking input variable bounds in blank index case
            expect_identical(
                applyGraphRule(
                    varRangeClass$new(list(
                                      newIndexRange(quote(33)))), rule),
                NULL)
        )
        
        expect_error(  ## Not yet checking input variable bounds in blank index case
            expect_identical(
                applyGraphRule(
                    varRangeClass$new(list(newIndexRange(matrix(c(20,55), ncol = 1)))), rule),
                NULL)
        )
    }
    
    rule <- graphRuleClass$new(toExpr = quote(y),
                               fromExpr = quote(x[2:3]),
                               context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(3))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(4))), rule),
        NULL
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(4,6), ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,5), ncol = 1)))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(4:5)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )
    
    rule <- graphRuleClass$new(toExpr = quote(y),
                               fromExpr = quote(x[2,3]),
                               context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(2), newIndexRange(3))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(2), newIndexRange(4))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(1), newIndexRange(4))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,4), ncol = 1)),
                                   newIndexRange(matrix(c(1,3), ncol = 1)))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,5), ncol = 1)),
                                   newIndexRange(matrix(c(3,5), ncol = 1)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,5), ncol = 1)),
                                   newIndexRange(matrix(c(4,6), ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)),
                                   newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)),
                                   newIndexRange(quote(4:5)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)),
                                   newIndexRange(quote(4:5)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,4,4,3), ncol = 2)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,2,4,3), ncol = 2)))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    ## Various cases where the index of the constraint is or is not tangled up in an indexRange
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[2,3]),
                               context = context_i_short)
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,3,4,5), ncol = 2)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,2,4,3), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[2,3,i]),
                               context = context_i)

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,3,4,5,1,2), ncol = 3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,2,4,3,2,4), ncol = 3)))), rule),
        varRangeClass$new(list(newIndexRange(4)), varName = 'y')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,3,4,5), ncol = 2)),
                                   newIndexRange(quote(2:3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,2,4,3), ncol = 2)),
                                   newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2:3))), varName = 'y')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:5)),
                                   newIndexRange(matrix(c(3,4,2,2), ncol = 2)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)),
                                   newIndexRange(matrix(c(1,4,2,2), ncol = 2)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)),
                                   newIndexRange(matrix(c(3,4,2,2), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:5)),
                                   newIndexRange(quote(3:4)),
                                   newIndexRange(quote(2:3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)),
                                   newIndexRange(quote(3:4)),
                                   newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2:3))), varName = 'y')
    )

    if(FALSE) {  ## TODO: empty block case
        rule <- graphRuleClass$new(toExpr = quote(y[i+1]),
                                   fromExpr = quote(x[]),
                                   context = context_i)
        expect_identical(rule$constraints[[1]]$constraint, c(1, Inf))
    }
    
})

test_that("graphRules works for reordered columns", {
    rule <- graphRuleClass$new(toExpr = quote(y[j, i, k]),
                               fromExpr = quote(x[k, i, j]),
                               context = context_ijk_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(1:2)),
                              newIndexRange(quote(2:3)),
                              newIndexRange(quote(1:4)))), rule),
        varRangeClass$new(list(
                          newIndexRange(quote(1:4)),
                          newIndexRange(quote(2:3)),
                          newIndexRange(quote(1:2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(1,2,2,3), nrow = 2)),
                              newIndexRange(quote(1:4)))), rule),
        varRangeClass$new(list(
               newIndexRange(quote(1:4)),
               newIndexRange(matrix(c(2,3,1,2), nrow = 2))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[k, i, j]),
                                fromExpr = quote(x[i, j, k]),
                                context = context_ijk_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:3)),
                              newIndexRange(quote(1:4)),
                              newIndexRange(quote(1:2)))), rule),
        varRangeClass$new(list(
               newIndexRange(quote(1:2)),
               newIndexRange(quote(2:3)),
               newIndexRange(quote(1:4))), varName = 'y')
    )

    ## reordering with an indexRange matrix covering noncontiguous indices
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:3)),
                              newIndexRange(matrix(c(1,2,2,3), ncol = 2)))), rule),
        varRangeClass$new(list(
               newIndexRange(matrix(c(2,3,1,2), ncol = 2)),
               newIndexRange(quote(2:3))), rangeToIndexSlot = list(c(1,3), 2), varName = 'y')
                                           
    )
})

test_that("graphRules correctly handles ragged indexing", {

   n <- c(1,3,2)
   mi <- c(2,1,3)
   mj <- c(3,2,1)
   mij <- matrix(c(1,3,2,3,1,3,2,2,1), ncol = 3)

   ## Ragged indexing induces arbitrary rule for the index and its 'parent'
   
   rule <- graphRuleClass$new(toExpr = quote(y[k,j,i]),
                              fromExpr = quote(x[k,j,i]),
                              context = context_ijnik_short,
                              constants = list(n = n))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    c('indexRuleArbitraryClass', 'indexRuleBlockClass'))
   expect_identical(rule$indexSets$toIndexSlotToSet, c(2, 1, 1))

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(quote(1:2)),
                             newIndexRange(quote(1:2)),
                             newIndexRange(quote(1:2)))), rule),
       varRangeClass$new(list(newIndexRange(quote(1:2)),
                              newIndexRange(matrix(c(1,1,2,1,2,2), nrow = 3))), varName = 'y')
   )

   rule <- graphRuleClass$new(toExpr = quote(y[k,j,i]),
                              fromExpr = quote(x[k,j,i]),
                              context = context_ijnikni_short,
                              constants = list(n = n, mi = mi))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    'indexRuleArbitraryClass')
   expect_identical(rule$indexSets$toIndexSlotToSet, rep(1,3))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(quote(1:2)),
                             newIndexRange(quote(1:2)),
                             newIndexRange(quote(1:2)))), rule),
       varRangeClass$new(list(newIndexRange(matrix(c(1,2,1,1,1,1,1,2,1,1,2,2), nrow = 4))), varName = 'y')
   )

   rule <- graphRuleClass$new(toExpr = quote(y[k,j,i]),
                              fromExpr = quote(x[k,j,i]),
                              context = context_ijniknj_short,
                              constants = list(n = n, mj = mj))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    'indexRuleArbitraryClass')
   expect_identical(rule$indexSets$toIndexSlotToSet, rep(1,3))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(quote(2:3)),
                             newIndexRange(quote(1:3)),
                             newIndexRange(quote(2:3)))), rule),
       varRangeClass$new(list(newIndexRange(matrix(c(2,3,2,2,3,2,1,1,2,1,1,2,2,2,2,3,3,3), ncol = 3))), varName = 'y')
   )

   rule <- graphRuleClass$new(toExpr = quote(y[k,j,i]),
                              fromExpr = quote(x[k,j,i]),
                              context = context_ijniknij_short,
                              constants = list(n = n, mij = mij))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    'indexRuleArbitraryClass')
   expect_identical(rule$indexSets$toIndexSlotToSet, rep(1,3))

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             newIndexRange(quote(2:3)),
                             newIndexRange(quote(1:3)),
                             newIndexRange(quote(1:2)))), rule),
       varRangeClass$new(list(newIndexRange(matrix(c(2,3,2,1,1,3,2,2,2), ncol = 3))), varName = 'y')
   )

   ## This invokes complicated crossing case
   rule <- graphRuleClass$new(toExpr = quote(x[j]),
                              fromExpr = quote(y[i,j,3]),
                              context = context_ijni_short,
                              constants = list(n = n))
   expect_identical(length(rule$indexRules), 1L)
   expect_true(is(rule$indexRules[[1]], "indexRuleArbitraryClass"))

   ## This case has redundant results.
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(newIndexRange(quote(2:3)),
                                  newIndexRange(matrix(c(3,2,2,3,3,4), ncol = 2)))), rule),
       varRangeClass$new(list(newIndexRange(matrix(c(3,2,2), ncol = 1))), varName = 'x')
   )

})

test_that("graphRules works for non-contiguous indices in an indexRange", {
    ## Case of y[i,j,k] where i,k are in an indexRange together
    rule <- graphRuleClass$new(toExpr = quote(y[i,k,j]),
                               fromExpr = quote(x[i,j,k]),
                               context = context_ijk)
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(1,2,1,2), nrow = 2)),
                              newIndexRange(matrix(c(1,3),ncol = 1)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(1,2,1,2), nrow = 2)),
                               newIndexRange(matrix(c(1,3), ncol = 1))),
                          rangeToIndexSlot = list(c(1,3), 2), varName = 'y'))
})

test_that("graphRules works for getParents by checking 1-to-many case", {
    ## use y[i] -> x[2] to emphasize getParents use case; y[i] is "from"

    rule <- graphRuleClass$new(toExpr = quote(x),
                               fromExpr = quote(y[i]),
                               context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(3))), rule),
        varRangeClass$new(list(irNone), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(4))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[i]),
                               context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(4:5)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,5), ncol = 1)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(4,6), ncol = 1)))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(x[2:4]),
                               fromExpr = quote(y[i]),
                               context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,5), ncol = 1)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2:4))), varName = 'x')
    )

    k <- c(2,5,1)
    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[k[i]]),
                               context = context_i_short,
                               constants = list(k = k))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(6)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(5:6)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)))), rule),
        NULL
    )
 
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(6:7)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,5), ncol = 1)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,4), ncol = 1)))), rule),
        NULL
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(6,9), ncol = 1)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,9), ncol = 1)))), rule),
        NULL
    )

    k1 <- c(2,5,1)
    k2 <- c(3,4,2)
    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[k1[i],k2[i]]),
                               context = context_i_short,
                               constants = list(k1 = k1, k2 = k2))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(rbind(c(5,5),c(5,4))))), rule),
        varRangeClass$new(list(newIndexRange(quote(2))), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,9), ncol = 2)))), rule),
        NULL
    )
    

    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[i,3]),
                               context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)),
                                   newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(4:5)),
                                   newIndexRange(quote(2:3)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:3)),
                                   newIndexRange(quote(4:5)))), rule),
        NULL
    )


    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(2:3, ncol = 1)),
                                   newIndexRange(matrix(2:3, ncol = 1)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(4:5, ncol = 1)),
                                   newIndexRange(matrix(2:3, ncol = 1)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(2:3, ncol = 1)),
                                   newIndexRange(matrix(4:5, ncol = 1)))), rule),
        NULL
    )
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,2,3,4), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,4,5,3), ncol = 2)))), rule),
        NULL
    )    

    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[i,j]),
                               context = context_ij_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(2),
                                   newIndexRange(4))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(4),
                                   newIndexRange(4))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(2),
                                   newIndexRange(5))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)),
                                   newIndexRange(quote(4:5)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )


    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)),
                                   newIndexRange(quote(5:6)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(4:5)),
                                   newIndexRange(quote(5:6)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(1,4), ncol = 1)),
                                   newIndexRange(matrix(c(6,4), ncol = 1)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(4,7), ncol = 1)),
                                   newIndexRange(matrix(c(6,4), ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,2,3,5), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,4,5,3), ncol = 2)))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[i+3*j]),
                               context = context_ij_short)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(6)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(100)))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(x[2,i]),
                               fromExpr = quote(y[i,3,j]),
                               context = context_ij_short)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:5)),
                                   newIndexRange(quote(3:5)),
                                   newIndexRange(quote(4:7)))), rule),
        varRangeClass$new(list(newIndexRange(2), newIndexRange(quote(2:3))), varName = 'x')
    )

    ## Case where block rule is 2nd indexRule and 1st is `all` case so NULL.
    rule <- graphRuleClass$new(toExpr = quote(x[2,j]),
                               fromExpr = quote(y[j,3,i]),
                               context = context_ij_short)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:7)),
                                   newIndexRange(quote(3:5)),
                                   newIndexRange(quote(2:5)))), rule),
        varRangeClass$new(list(newIndexRange(2), newIndexRange(quote(3:4))), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:7)),
                                   newIndexRange(quote(3:5)),
                                   newIndexRange(quote(5:7)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(3,4,1,1,5,3,3,3,4,3,2,1,4,1,3), ncol = 3)))), rule),
        varRangeClass$new(list(newIndexRange(2), newIndexRange(quote(3:4))), varName = 'x')
    )
    
    n <- c(1,3,2)
    
    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[i,j]),
                               context = context_ijni_short,
                               constants = list(n = n))
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(2),
                                   newIndexRange(2))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(1),
                                   newIndexRange(3))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(1:2),
                                   newIndexRange(1:3))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(1),
                                   newIndexRange(2:3))), rule),
        NULL
    )
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(1,1,1,3), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(1,3,3,3), ncol = 2)))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(y[j]),
                               fromExpr = quote(x[j,i+3*j]),
                               context = context_ij)
    expect_identical(length(rule$indexRules), 1L)
    expect_true(is(rule$indexRules[[1]], "indexRuleArbitraryClass"))


    rule <- graphRuleClass$new(toExpr = quote(x),
                               fromExpr = quote(y[i+j,j]),
                               context = context_ij)
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(1,1,1,1),2)))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[i+j,j]),
                               context = context_ij_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(5,5,3,1), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(5,5,7,1), ncol = 2)))), rule),
        NULL
    )

    n <- c(1,3,2)
    rule <- graphRuleClass$new(toExpr = quote(x[2]),
                               fromExpr = quote(y[i,j]),
                               context = context_ijni_short,
                               constants = list(n = n))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(2,3,2,3), ncol = 2)))), rule),
        varRangeClass$new(list(newIndexRange(2)), varName = 'x')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(matrix(c(1,3,2,3), ncol = 2)))), rule),
        NULL
    )
    
})

test_that("Single index variable used multiple times", {

    rule <- graphRuleClass$new(toExpr = quote(y[i, i+1]),
                               fromExpr = quote(x[i]),
                               context = context_i)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(3:5)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(3,4,5,4,5,6), ncol = 2))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i,i+1]),
                               context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(3,4,5,4,5,6), ncol = 2)))), rule),
        varRangeClass$new(list(
                          newIndexRange(quote(3:5))), varName = 'y')
    )
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(3,5,5,4,5,6), ncol = 2)))), rule),
        varRangeClass$new(list(
                          newIndexRange(matrix(c(3,5)))), varName = 'y')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(3,5,4,5,5,6), ncol = 2)))), rule),
        NULL
    )
    
    rule <- graphRuleClass$new(toExpr = quote(y[i,i]),
                               fromExpr = quote(x[i,i]),
                               context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(2,2,3,2,3,3), nrow=3)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(2,3,2,3), ncol=2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(2),
                              newIndexRange(2))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(2,2), nrow = 1))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2:4)),
                              newIndexRange(quote(2:3)))), rule),
        varRangeClass$new(list(newIndexRange(matrix(c(2,3,2,3), ncol = 2))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[i+1,i]),
                               fromExpr = quote(x[2]),
                               context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(quote(2)))), rule),
        varRangeClass$new(list(newIndexRange(cbind(2:11, 1:10))), varName = 'y')
    )

    rule <- graphRuleClass$new(toExpr = quote(y[k1[i+1],k2[i]]),
                               fromExpr = quote(x[i]),
                               context = context_i_short,
                               constants = list(k1 = c(2,5,9,12),
                                                k2 = c(8,1,2)))
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              newIndexRange(matrix(c(3,4,1))))), rule),
        varRangeClass$new(list(newIndexRange(rbind(c(12,2), c(5,8)))), varName = 'y')
    )
    
    
})


test_that("indexRange matrix converted to sequence if appropriate", {
    
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 2:5){}))
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(j in 1:3){}))
    
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))
    
    k <- c(2,3,4,5)
    rule <- graphRuleClass$new(toExpr = quote(y[i+1,j]),
                                 fromExpr = quote(x[k[i-1],j]),
                                 context = context_ij,
                          constants = list(k = k))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:5)),
                                   newIndexRange(2))),rule),
        varRangeClass$new(list(newIndexRange(quote(3:6)), newIndexRange(2)), varName = 'y')
    )

    k <- c(2,4,3,5)
    rule <- graphRuleClass$new(toExpr = quote(y[i+1,j]),
                                 fromExpr = quote(x[k[i-1],j]),
                                 context = context_ij,
                          constants = list(k = k))

    ## This gives 3,5,4,6; but `indexRangeMatrix$toSequence` does not sort
    ## (to avoid usually unneeded computation), hence result is still a matrix.
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(2:5)),
                                   newIndexRange(2))),rule),
        varRangeClass$new(list(newIndexRange(c(3,5,4,6)), newIndexRange(2)), varName = 'y')
    )

    
})

## TODO: This works in terms of graphRules; will need to consider if we want to allow use of it.
test_that("non-sequential indexing in for loop or indexRule", {
    singleContextNonseq <-
    singleContextClass$new(forCode = quote(for(i in c(2,3,5)){}))
    context_i_nonseq <- modelContextClass$new(list(singleContextNonseq))

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i]),
                               context = context_i_nonseq)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(1:4)))), rule),
        varRangeClass$new(list(newIndexRange(quote(2:3))), varName = 'y')
    )
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:5)))), rule),
                varRangeClass$new(list(newIndexRange(matrix(c(3,5)))), varName = 'y')
    )        

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(7:9)))), rule),
        NULL
    )

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[c(2,3,5)]),
                               context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)))), rule),
                varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y')
    )        

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(7:9)))), rule),
        NULL
    )
    
    rule <- graphRuleClass$new(toExpr = quote(y[c(2,3,5)]),
                               fromExpr = quote(x[1:3]),
                               context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(newIndexRange(quote(3:4)))), rule),
                varRangeClass$new(list(newIndexRange(matrix(c(2,3,5)))),, varName = 'y')
    )        

})

test_that("getFromRange", {
    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[i+2]),
                               context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(newIndexRange(quote(1:12))),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[2:3]),
                               context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(newIndexRange(quote(1:3))),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x),
                               context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(irNone),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(toExpr = quote(y[i]),
                               fromExpr = quote(x[k[i]+2]),
                               context = context_i_short,
                               constants = list(k = c(4,2,7)))
    expect_equal(rule$getFromRange(), varRangeClass$new(list(newIndexRange(quote(1:9))),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(toExpr = quote(y[i,j]),
                               fromExpr = quote(x[k[i]+2,j,i]),
                               context = context_ij_short,
                               constants = list(k = c(4,2,7)))
    expect_equal(rule$getFromRange(), varRangeClass$new(list(newIndexRange(quote(1:9)),
                                                             newIndexRange(quote(1:4)),
                                                             newIndexRange(quote(1:3))),
                                                            varName = 'x'))

    ## getParents, all rule cases
    
    rule <- graphRuleClass$new(toExpr = quote(x[3]),
                               fromExpr = quote(y[i+2]),
                               context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(newIndexRange(quote(1:12))),
                                                        varName = 'y'))

    rule <- graphRuleClass$new(toExpr = quote(x[2,j]),
                               fromExpr = quote(y[j,5,i]),
                               context = context_ij_short)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(
                                                        newIndexRange(quote(1:4)),
                                                        newIndexRange(quote(1:5)),
                                                        newIndexRange(quote(1:3))),
                                                        varName = 'y'))


    rule <- graphRuleClass$new(toExpr = quote(x[3]),
                               fromExpr = quote(y[k[i]+2]),
                               context = context_i_short,
                               constants = list(k = c(4,2,7)))
    
    expect_equal(rule$getFromRange(), varRangeClass$new(list(newIndexRange(quote(1:9))),
                                                            varName = 'y'))

 
    rule <- graphRuleClass$new(toExpr = quote(x[3]),
                               fromExpr = quote(y[k1[i],k2[i]]),
                               context = context_i_short,
                               constants = list(k1 = c(4,2,7), k2 = c(2,1,4)))
    expect_equal(rule$getFromRange(), varRangeClass$new(list(newIndexRange(quote(1:7)),
                                                             newIndexRange(quote(1:4))),
                                                            varName = 'y'))

})

test_that("application of graphRule to varName and character string (rather than varRange)", {
       rule <- graphRuleClass$new(toExpr = quote(y),
                                  fromExpr = quote(x[3]),
                                  context = context_0)
       expect_equal(rule$apply('x'),
                    varRangeClass$new(list(irNone), varName = 'y'))

       rule <- graphRuleClass$new(toExpr = quote(y[i+2]),
                                  fromExpr = quote(x[i]),
                                  context = context_i)
       expect_equal(rule$apply('x'),
                    varRangeClass$new(list(newIndexRange(quote(3:12))), varName = 'y'))
                                                       

       rule <- graphRuleClass$new(toExpr = quote(y[i+2]),
                                  fromExpr = quote(x[i]),
                                  context = context_i)
       expect_equal(rule$apply('x[5:12]'),
                    varRangeClass$new(list(newIndexRange(quote(7:12))), varName = 'y'))

       rule <- graphRuleClass$new(toExpr = quote(y[i+2]),
                                  fromExpr = quote(x[i]),
                                  context = context_i)
       expect_identical(rule$apply('z'), NULL)


       rule <- graphRuleClass$new(toExpr = quote(y[i+2]),
                                  fromExpr = quote(x[i]),
                                  context = context_i)
       expect_identical(rule$apply('z[5:12]'), NULL)

       rule <- graphRuleClass$new(toExpr = quote(y[i+2,j]),
                                  fromExpr = quote(x[i]),
                                  context = context_ij)
       expect_equal(rule$apply('x'),
                    varRangeClass$new(list(newIndexRange(quote(3:12)),
                                           newIndexRange(quote(1:4))),
                                      varName = 'y'))
       
       rule <- graphRuleClass$new(toExpr = quote(y[k[i]+2]),
                                  fromExpr = quote(x[i]),
                                  context = context_i_short,
                                  constants = list(k = c(4,2,7)))
       expect_equal(rule$apply('x'),
                    varRangeClass$new(list(newIndexRange(matrix(c(6,4,9)))),
                                      varName = 'y'))


       rule <- graphRuleClass$new(toExpr = quote(x[3]),
                                  fromExpr = quote(y[i+2]),
                                  context = context_i_short)
       expect_equal(rule$apply('y'),
                    varRangeClass$new(list(newIndexRange(3)),
                                      varName = 'x'))

       rule <- graphRuleClass$new(toExpr = quote(x[3]),
                                  fromExpr = quote(y[i+2]),
                                  context = context_i_short)
       expect_equal(rule$apply('y[4]'),
                    varRangeClass$new(list(newIndexRange(3)),
                                      varName = 'x'))
       expect_identical(rule$apply('y[14]'), NULL)

       rule <- graphRuleClass$new(toExpr = quote(x[3]),
                                  fromExpr = quote(y[k[i]+2]),
                                  context = context_i_short,
                                  constants = list(k=c(7,4,2)))
       expect_equal(rule$apply('y[6]'),
                    varRangeClass$new(list(newIndexRange(3)),
                                      varName = 'x'))
  
})

