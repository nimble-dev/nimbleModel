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

## This is not allowed in current nimble: y[i] <- sum(x[c(1,3,5)])

irEmpty <- nimbleModel:::indexRange_empty()
## vrEmpty <- varRangeClass$new(list(irEmpty))
## vrEmpty_mat2 <- varRangeClass$new(list(irEmpty), list(1:2))  ## single indexRange, two indices
## vrEmpty2 <- varRangeClass$new(list(irEmpty, irEmpty))
irNone <- nimbleModel:::indexRange_none()
vrNone <- varRangeClass$new(list(irNone))

singleContext1 <-
    modelSingleContext(forCode = quote(for(i in 1:10){}))

singleContext1_short <-
    modelSingleContext(forCode = quote(for(i in 1:3){}))

## used to be j in 1:5
singleContext2 <-
    modelSingleContext(forCode = quote(for(j in 1:4){}))

singleContext3 <-
    modelSingleContext(forCode = quote(for(k in 1:3){}))

## alternative way to specific a single context
singleContext2ni <-
    modelSingleContext(indexVarExpr = quote(j),
                       indexRangeExpr = quote(1:n[i]),
                       )

singleContext3ni <-
    modelSingleContext(forCode = quote(for(k in 1:mi[i]){}))                          
singleContext3nj <-
    modelSingleContext(forCode = quote(for(k in 1:mj[j]){}))                          
singleContext3nij <-
    modelSingleContext(forCode = quote(for(k in 1:mij[i,j]){}))                          

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
    rule <- graphRuleClass$new(LHS = quote(y[i]),
                               RHS = quote(x[i+1]),
                               context = context_i)

    expect_error(
        rule$apply(indexRange(c(2, 4))),
        "needs to be a varRange"
    )
    expect_error(
        rule$apply(varRangeClass$new(indexRange(matrix(c(2, 4), nrow = 1)))),
        "list of indexRanges"
    )

    expect_equal(
        rule$apply(varRangeClass$new(list(indexRange(quote(2:3))))),
        varRangeClass$new(list(indexRange(quote(1:2))), varName = 'y')
    )
})

test_that("error trap incorrect number of input indices", {
    ## Single simple sequence rule
    rule <- makeGraphRule(LHS = quote(y[i]),
                                 RHS = quote(x[i]),
                                 context = context_i)

    ## incorrect number of input indexRanges
    expect_error(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(3),
                              indexRange(4))), rule),
        "incorrect number of input indices"
    )

    expect_error(
        applyGraphRule(
            vrNone, rule),
        "incorrect number of input indices"
    )

    ## incorrect length of input matrix indexRange
    expect_error(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4), nrow = 1)))), rule),
        "incorrect number of input indices"
    )

    rule <- makeGraphRule(LHS = quote(y[i]),
                                 RHS = quote(x[2, 3]),
                                 context = context_i)

    expect_error(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(3))), rule), 
        "incorrect number of input indices"
    )

    rule <- makeGraphRule(LHS = quote(y[i]),
                                RHS = quote(x),
                                context = context_i)

    expect_error(
        applyGraphRule(
            varRangeClass$new(list(indexRange(2))), rule),
        "incorrect number of input indices"
    )

})

test_that("handle incorrect indexing", {
    ## Do we want to throw an error?
    rule <- makeGraphRule(LHS = quote(y[i]),
                                RHS = quote(x[i]),
                                context = context_i)

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(-3))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(0))), rule),
        NULL
    )

})

test_that("graphRules works for basic cases lacking LHS indexing", {
    rule <- makeGraphRule(LHS = quote(y),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphRule(
            vrNone, rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y),
                                RHS = quote(x[2]),
                                context = context_0)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(2))), rule),
        varRangeClass$new(list(irNone), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(3))), rule),
        NULL
    )
})

test_that("graphRules works for single index cases, wrapping indexRules", {
    ## indexRule_block
    
    rule <- makeGraphRule(LHS = quote(y[i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))), varName = 'y')
    )

   ## Checking that NAs produced by invalid input matrix entries are discarded
   ## in graphRules processing.

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(10, 12, 14), ncol = 1)))), rule),
        varRangeClass$new(list(
                          indexRange(quote(9:9))), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(12, 14), ncol = 1)))), rule),
        NULL
    )

    ## indexRule_all

    rule <- makeGraphRule(LHS = quote(y[i+1]),
                                 RHS = quote(x[]),
                                 context = context_i)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11))), varName = 'y'))


    ## more complicated expression than simple index
    rule <- makeGraphRule(LHS = quote(y[3*i]),
                                RHS = quote(x[]),
                                context = context_i)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(3))), rule),
        varRangeClass$new(list(indexRange(matrix(seq(3, 30, by = 3), ncol = 1))), varName = 'y')
    )

    ## indexRule_constant

    rule <- makeGraphRule(LHS = quote(y[2]),
                                RHS = quote(x[]),
                                context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2))), varName = 'y'))

    idx <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 1 and 2 are repeated
    rule <- makeGraphRule(LHS = quote(y[i]),
                                 RHS = quote(x[ idx[i] ]),
                                 context = context_i,
                                 constants = list(idx = idx))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:5)))), rule), ## 2 yields multiples; 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(matrix(bruteForceNestedIndexing(idx, 2:5)))), varName = 'y')
    )
    
    rule <- makeGraphRule(LHS = quote(y[idx[i]]),
                                 RHS = quote(x[i]),
                                 context = context_i,
                                 constants = list(idx = idx))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(3:7)))), rule), 
        varRangeClass$new(list(
                          indexRange(matrix(idx[3:7]))), varName = 'y')
    )

})

test_that("graphRules works for various basic multiple index cases", {

    ## two all rules, crossed (of course)
    rule <- makeGraphRule(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2]),
                                 context = context_ij)

    expect_equal(
        applyGraphRule(varRangeClass$new(list(indexRange(2))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:4))), varName = 'y')
    )
   
    expect_identical(
        applyGraphRule(varRangeClass$new(list(indexRange(3))), rule),
        NULL
    )

    ## all plus constant, crossed (of course)
    rule <- makeGraphRule(LHS = quote(y[i,2]),
                                 RHS = quote(x),
                                 context = context_i)

    expect_equal(
        applyGraphRule(
            vrNone, rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))), varName = 'y')
    )

    ## two constant rules, crossed (of course)
    rule <- makeGraphRule(LHS = quote(y[2,3]),
                                 RHS = quote(x),
                                 context = context_0)

    expect_equal(
        applyGraphRule(
            vrNone, rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))), varName = 'y')
    )

    ## block plus constant, crossed (of course)
    rule <- makeGraphRule(LHS = quote(y[2,i+1]),
                                 RHS = quote(x[i]),
                                 context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(1:3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))), varName = 'y')
    )

    ## This gives mixed result with one indexRange empty and one not.
    ## I think that makes sense where there is a direct from->to mapping,
    ## but later code will need to determine that this result is an empty
    ## overall varRange even though at least one indexRange is not empty.
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(22:24)))), rule),
        NULL
    )

    ## block plus all, crossed (of course)
    rule <- makeGraphRule(LHS = quote(y[j,i]),
                                 RHS = quote(x[i]),
                                 context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(indexRange(quote(1:4)),
                               indexRange(quote(2:3))), varName = 'y')
    )

    ## two block rules, crossed
    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                 RHS = quote(x[i, j]),
                                 context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(3:4))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(5:9)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(matrix(c(1,3,5), ncol = 1)))), rule),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(matrix(c(1,3), ncol = 1))), varName = 'y')
    )
})


test_that("graphRules works for arbitrary rules of multiple contexts entangled with other indices", {

    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                RHS = quote(x[i+3*j,j]),
                                context = context_ij)
    expect_identical(length(rule$indexRules), 1L)
    expect_true(is(rule$indexRules[[1]], "indexRuleClass_arbitrary"))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(1:7)),
                              indexRange(quote(1:3)))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(1,1,2,1,3,1,4,1,1,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(1,2,2,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(4,2), ncol = 2)))), rule),
        NULL
    )

    ## version using k[i,j]
    k <- matrix(outer(1:10, 1:4, function(i,j) i+3*j), nrow = 10, ncol = 4)
    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                RHS = quote(x[k[i,j],j]),
                                context = context_ij,
                                constants = list(k = k))
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(1:7)),
                              indexRange(quote(1:3)))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(1,1,2,1,3,1,4,1,1,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(1,2,2,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    ## getParents version
    rule <- makeGraphRule(LHS = quote(y[i+3*j, j]),
                                RHS = quote(x[i,j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(1:2)))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(5,1,6,1,8,2,9,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,1,3,1,3,2,11,3), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(5,1,6,1,9,2), byrow = TRUE, ncol = 2))), varName = 'y')
    )


    ## Additional LHS index not used on RHS, 'all' scenario embedded within arbitrary rule.
    rule <- makeGraphRule(LHS = quote(y[i+3*j, j]),
                                RHS = quote(x[i]),
                                context = context_ij)
    expect_identical(length(rule$indexRules), 1L)
    expect_true(is(rule$indexRules[[1]], "indexRuleClass_arbitrary"))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(1:2)))), rule), 
        varRangeClass$new(list(
               indexRange(matrix(c(4,1,7,2,10,3,13,4,5,1,8,2,11,3,14,4), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    ## More complicated such case where first i,j need to be crossed.
    rule <- makeGraphRule(LHS = quote(y[i+j, j+k]),
                                 RHS = quote(x[i,j]),
                                 context = context_ijk)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(matrix(c(2,4,6))))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(4,3,4,4,4,5,5,3,5,4,5,5,6,5,6,6,6,7,7,5,7,6,7,7), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(1)),
                              indexRange(quote(6)))), rule),
        NULL
    )

    ## getParents version
    rule <- makeGraphRule(LHS = quote(y[i]),
                                RHS = quote(x[i+3*j, j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
               indexRange(quote(1:2))), varName = 'y')
    )
    

   ## include an additional index tied in in some cases via the input varRange
   rule <- makeGraphRule(LHS = quote(y[j+3*i,j,k]),
                                 RHS = quote(x[i, k, j]),
                                 context = context_ijk)

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(matrix(c(2,3), ncol = 1)),
                             indexRange(matrix(c(1,3,1,2), nrow = 2)))), rule),
       varRangeClass$new(list(indexRange(matrix(c(7,10,8,11,1,1,2,2,1,1,3,3), ncol = 3))), varName = 'y'))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(matrix(c(2,3,1,2,1,3), nrow = 2)))), rule),
       varRangeClass$new(list(indexRange(matrix(c(7,1,1,12,3,2), byrow = TRUE, ncol = 3))), varName = 'y'))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(matrix(c(2,3), ncol = 1)),
                             indexRange(matrix(c(1,2), ncol = 1)),
                             indexRange(matrix(c(1,3), ncol = 1)))), rule),
       varRangeClass$new(list(indexRange(matrix(c(7,10,9,12,1,1,3,3), ncol = 2)),
                              indexRange(quote(1:2))), varName = 'y'))

})


test_that("graphRules works for arbitrary rule with ragged blocks combined with another rule", {

    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                RHS = quote(x[i, 1:j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(
                          indexRange(quote(2:4)),
                          indexRange(matrix(c(2,3,4,3,4), ncol = 1))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(2, 3, 1, 2, 99, 3), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,3,2,4,1,2,1,3,1,4), byrow = TRUE, ncol = 2))), varName = 'y')
    )

    ## getParents version
    rule <- makeGraphRule(LHS = quote(y[i, 1:j]),
                                RHS = quote(x[i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(
                          indexRange(quote(2:4)),
                          indexRange(matrix(c(1,2,1,2,3), ncol = 1))), varName = 'y')
    )
    

    rule <- makeGraphRule(LHS = quote(y[i, 1:i, j]),
                                RHS = quote(x[i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(2:4)))), rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,1,2,2,3,1,3,2,3,3), byrow = TRUE, ncol = 2)),
                          indexRange(quote(2:4))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(2, 3, 1, 4, 99, 3), byrow = TRUE, ncol = 2)))), rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,1,3,2,2,3,1,1,4), byrow = TRUE, ncol = 3))), varName = 'y')
    )

    ## getParents version
    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                RHS = quote(x[i, 1:i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,1,2,2,1,1), nrow = 3, byrow = TRUE)),
                              indexRange(quote(2:4)))), rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,2,1), ncol = 1)),
                          indexRange(quote(2:4))), varName = 'y')
    )
})

test_that("graphRules works for two rules that use a single indexRange matrix", {
    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                RHS = quote(x[i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(7,3,4,2), ncol = 2)))), rule),
        varRangeClass$new(list(indexRange(matrix(c(7,3,4,2), ncol = 2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(11,3,4,2), ncol = 2)))), rule),
        varRangeClass$new(list(indexRange(matrix(c(3,2), ncol = 2))), varName = 'y')
    )

    ## single empty indexRange because results of the two rules get combined back together
    ## as we need to check by row if combined result is valid
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(11,3,4,5), ncol = 2)))), rule),
        NULL
    )

    idx1 <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 2 is repeated and 3,4 absent
    idx2 <- c(1, 1, 2, 1) 

    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                 RHS = quote(x[ idx1[i] , idx2[j]]),
                                 context = context_ij,
                                 constants = list(idx1 = idx1, idx2 = idx2))

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(5,5,5,3,3,3,2,2,2,1,4,2,1,4,2,1,4,2), ncol = 2)))), rule), 
        varRangeClass$new(list(indexRange(matrix(c(9,9,9,9,3,7,3,7,3,7,3,7,1,2,4,3,1,1,2,2,4,4,3,3), ncol = 2))),
                          varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                 RHS = quote(x[ idx1[i] , j]),
                                 context = context_ij,
                                 constants = list(idx1 = idx1))

    ## Apply rule to a sequence indexRange
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(5,5,3,2,2,2,5,2,5,4), ncol = 2)))), rule), 
        varRangeClass$new(list(indexRange(matrix(c(9,3,7,2,4,4), ncol = 2))), varName = 'y')
    )
    
    ## effect of having a RHS constraint, with indexRange matrix inputs
    rule <- makeGraphRule(LHS = quote(y[i, j]),
                                 RHS = quote(x[3, i, j]),
                                 context = context_ij)

    ## constraint results in two empty ranges instead of one, despite input indexRange matrix
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(4), 
                              indexRange(matrix(c(1,1,2,2), ncol = 2)))), rule),
        NULL
    )
    
    ## constraint results in two empty ranges instead of one, despite input indexRange matrix
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(4,7,1,1,2,2), ncol = 3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(4,3,1,1,2,2), ncol = 3)))), rule),
        varRangeClass$new(list(indexRange(matrix(c(1,2), ncol = 2))), varName = 'y')
    )

})

test_that("graphRules works for all rule that is function of two indexes", {
    rule <- makeGraphRule(LHS = quote(y[j+3*i]),
                                 RHS = quote(x[2]),
                                 context = context_ij_short)

    ## Note that there are duplicate results.
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(matrix(c(4:7,7:10,10:13)))), varName = 'y')
    )

})


test_that("graphRules works for fixed RHS indices", {
    ## Start by just using the simplest LHS given focus here is on RHS constraints.

    rule <- makeGraphRule(LHS = quote(y[2]),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphRule(
            vrNone, rule),
        varRangeClass$new(list(indexRange(quote(2))), varName = 'y'))

    
    rule <- makeGraphRule(LHS = quote(y),
                                RHS = quote(x[2]),
                                context = context_0)

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(3))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(3:4, ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(2:3, ncol = 1)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:4)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y),
                                RHS = quote(x[]),
                                context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(3))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(3:20))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,20), ncol = 1)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_error(  ## Not yet checking input variable bounds in blank index case
        expect_identical(
            applyGraphRule(
                varRangeClass$new(list(
                                  indexRange(quote(33)))), rule),
            NULL)
    )

    expect_error(  ## Not yet checking input variable bounds in blank index case
        expect_identical(
            applyGraphRule(
                varRangeClass$new(list(indexRange(matrix(c(20,55), ncol = 1)))), rule),
            NULL)
    )

    rule <- makeGraphRule(LHS = quote(y),
                                RHS = quote(x[2:3]),
                                context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(3))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(4))), rule),
        NULL
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(4,6), ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(4:5)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:4)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )
    
    rule <- makeGraphRule(LHS = quote(y),
                                RHS = quote(x[2,3]),
                                context = context_0)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(2), indexRange(3))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(2), indexRange(4))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(1), indexRange(4))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,4), ncol = 1)),
                                   indexRange(matrix(c(1,3), ncol = 1)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)),
                                   indexRange(matrix(c(3,5), ncol = 1)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)),
                                   indexRange(matrix(c(4,6), ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(4:5)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:4)),
                                   indexRange(quote(4:5)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,4,4,3), ncol = 2)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3), ncol = 2)))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'y')
    )

    ## Various cases where the index of the constraint is or is not tangled up in an indexRule
    rule <- makeGraphRule(LHS = quote(y[i]),
                                RHS = quote(x[2,3]),
                                context = context_i_short)
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,5), ncol = 2)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3), ncol = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(1:3))), varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y[i]),
                                RHS = quote(x[2,3,i]),
                                context = context_i)

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,5,1,2), ncol = 3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3,2,4), ncol = 3)))), rule),
        varRangeClass$new(list(indexRange(quote(4:4))), varName = 'y')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,5), ncol = 2)),
                                   indexRange(quote(2:3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3), ncol = 2)),
                                   indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:3))), varName = 'y')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:5)),
                                   indexRange(matrix(c(3,4,2,2), ncol = 2)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(matrix(c(1,4,2,2), ncol = 2)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(matrix(c(3,4,2,2), ncol = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:2))), varName = 'y')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:5)),
                                   indexRange(quote(3:4)),
                                   indexRange(quote(2:3)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(3:4)),
                                   indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:3))), varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y[i+1]),
                                 RHS = quote(x[]),
                                 context = context_i)
    expect_identical(rule$constraints[[1]]$constraint, c(1, Inf))

    
})



test_that("graphRules works for reordered columns", {
    rule <- makeGraphRule(LHS = quote(y[j, i, k]),
                                RHS = quote(x[k, i, j]),
                                context = context_ijk_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(1:2)),
                              indexRange(quote(2:3)),
                              indexRange(quote(1:4)))), rule),
        varRangeClass$new(list(
                          indexRange(quote(1:4)),
                          indexRange(quote(2:3)),
                          indexRange(quote(1:2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(1,2,2,3), nrow = 2)),
                              indexRange(quote(1:4)))), rule),
        varRangeClass$new(list(
               indexRange(quote(1:4)),
               indexRange(matrix(c(2,3,1,2), nrow = 2))), varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y[k, i, j]),
                                RHS = quote(x[i, j, k]),
                                context = context_ijk_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(1:4)),
                              indexRange(quote(1:2)))), rule),
        varRangeClass$new(list(
               indexRange(quote(1:2)),
               indexRange(quote(2:3)),
               indexRange(quote(1:4))), varName = 'y')
    )

    ## reordering with an indexRange matrix covering noncontiguous indices
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(matrix(c(1,2,2,3), ncol = 2)))), rule),
        varRangeClass$new(list(
               indexRange(matrix(c(2,3,1,2), ncol = 2)),
               indexRange(quote(2:3))), indexOrders = list(c(1,3), 2), varName = 'y')
                                           
    )
})

test_that("graphRules correctly handles ragged indexing", {

   n <- c(1,3,2)
   mi <- c(2,1,3)
   mj <- c(3,2,1)
   mij <- matrix(c(1,3,2,3,1,3,2,2,1), ncol = 3)

   ## Ragged indexing induces arbitrary rule for the index and its 'parent'
   
   rule <- makeGraphRule(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijnik_short,
                             constants = list(n = n))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    c('indexRuleClass_arbitrary', 'indexRuleClass_block'))
   expect_identical(rule$indexSets$LHSindex2setID, c(2, 1, 1))

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)))), rule),
       varRangeClass$new(list(indexRange(quote(1:2)),
                              indexRange(matrix(c(1,1,2,1,2,2), nrow = 3))), varName = 'y')
   )

   rule <- makeGraphRule(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijnikni_short,
                             constants = list(n = n, mi = mi))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    'indexRuleClass_arbitrary')
   expect_identical(rule$indexSets$LHSindex2setID, rep(1,3))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)))), rule),
       varRangeClass$new(list(indexRange(matrix(c(1,2,1,1,1,1,1,2,1,1,2,2), nrow = 4))), varName = 'y')
   )

   rule <- makeGraphRule(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijniknj_short,
                             constants = list(n = n, mj = mj))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    'indexRuleClass_arbitrary')
   expect_identical(rule$indexSets$LHSindex2setID, rep(1,3))
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(quote(2:3)),
                             indexRange(quote(1:3)),
                             indexRange(quote(2:3)))), rule),
       varRangeClass$new(list(indexRange(matrix(c(2,3,2,2,3,2,1,1,2,1,1,2,2,2,2,3,3,3), ncol = 3))), varName = 'y')
   )

   rule <- makeGraphRule(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijniknij_short,
                             constants = list(n = n, mij = mij))
   expect_identical(sapply(rule$indexRules, function(ir) class(ir)[1]),
                    'indexRuleClass_arbitrary')
   expect_identical(rule$indexSets$LHSindex2setID, rep(1,3))

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(quote(2:3)),
                             indexRange(quote(1:3)),
                             indexRange(quote(1:2)))), rule),
       varRangeClass$new(list(indexRange(matrix(c(2,3,2,1,1,3,2,2,2), ncol = 3))), varName = 'y')
   )

   ## This invokes complicated crossing case
   rule <- makeGraphRule(LHS = quote(x[j]),
                                 RHS = quote(y[i,j,3]),
                                context = context_ijni_short,
                                constants = list(n = n))
   expect_identical(length(rule$indexRules), 1L)
   expect_true(is(rule$indexRules[[1]], "indexRuleClass_arbitrary"))

   ## NOTE: this case has redundant results.
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(indexRange(quote(2:3)),
                                  indexRange(matrix(c(3,2,2,3,3,4), ncol = 2)))), rule),
       varRangeClass$new(list(indexRange(matrix(c(3,2,2), ncol = 1))), varName = 'x')
   )

})

test_that("graphRules works for non-contiguous indices in an indexRange", {
    ## Case of y[i,j,k] where i,k are in an indexRange together
    rule <- makeGraphRule(LHS = quote(y[i,k,j]),
                                 RHS = quote(x[i,j,k]),
                             context = context_ijk)
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(1,2,1,2), nrow = 2)),
                              indexRange(matrix(c(1,3),ncol = 1)))), rule),
        varRangeClass$new(list(indexRange(matrix(c(1,2,1,2), nrow = 2)),
                               indexRange(matrix(c(1,3), ncol = 1))),
                          indexOrders = list(c(1,3), 2), varName = 'y'))
})

test_that("graphRules works for getParents by checking 1-to-many case", {
    ## use y[i] -> x[2] to emphasize getParents use case; y[i] is "RHS"

    ## should some of these tests be moved to test-indexRules_any.R ?
    ## perhaps any that don't involve weird crossing
    ## use that to test simple cases that input scalar, seq, 1-col matrix and 2-col matrix handled properly

    rule <- makeGraphRule(LHS = quote(x),
                                RHS = quote(y[i]),
                                context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(3))), rule),
        varRangeClass$new(list(nimbleModel:::indexRange_none()), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(4))), rule),
        NULL
    )

    rule <- makeGraphRule(LHS = quote(x[2]),
                                RHS = quote(y[i]),
                                context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:4)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(4:5)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(4,6), ncol = 1)))), rule),
        NULL
    )

    k <- c(2,5,1)
    rule <- makeGraphRule(LHS = quote(x[2]),
                                 RHS = quote(y[k[i]]),
                                 context = context_i_short,
                                 constants = list(k = k))


    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(6)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(5:6)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:4)))), rule),
        NULL
    )
 
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(6:7)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,4), ncol = 1)))), rule),
        NULL
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(6,9), ncol = 1)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,9), ncol = 1)))), rule),
        NULL
    )

    rule <- makeGraphRule(LHS = quote(x[2]),
                                RHS = quote(y[i,3]),
                                context = context_i_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(4:5)),
                                   indexRange(quote(2:3)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(4:5)))), rule),
        NULL
    )


    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(2:3, ncol = 1)),
                                   indexRange(matrix(2:3, ncol = 1)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(4:5, ncol = 1)),
                                   indexRange(matrix(2:3, ncol = 1)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(2:3, ncol = 1)),
                                   indexRange(matrix(4:5, ncol = 1)))), rule),
        NULL
    )
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,4), ncol = 2)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,4,5,3), ncol = 2)))), rule),
        NULL
    )    

    rule <- makeGraphRule(LHS = quote(x[2]),
                                RHS = quote(y[i,j]),
                                context = context_ij_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(2),
                                   indexRange(4))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(4),
                                   indexRange(4))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(2),
                                   indexRange(5))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:4)),
                                   indexRange(quote(4:5)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )


    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3:4)),
                                   indexRange(quote(5:6)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(4:5)),
                                   indexRange(quote(5:6)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(1,4), ncol = 1)),
                                   indexRange(matrix(c(6,4), ncol = 1)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(4,7), ncol = 1)),
                                   indexRange(matrix(c(6,4), ncol = 1)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,5), ncol = 2)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(2,4,5,3), ncol = 2)))), rule),
        NULL
    )

    rule <- makeGraphRule(LHS = quote(x[2]),
                                 RHS = quote(y[i+3*j]),
                                 context = context_ij_short)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(6)))), rule),
        varRangeClass$new(list(indexRange(2)), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(3)))), rule),
        NULL
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(100)))), rule),
        NULL
    )

    rule <- makeGraphRule(LHS = quote(x[2,i]),
                                RHS = quote(y[i,3,j]),
                                context = context_ij_short)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:5)),
                                   indexRange(quote(3:5)),
                                   indexRange(quote(4:7)))), rule),
        varRangeClass$new(list(indexRange(2), indexRange(quote(2:3))), varName = 'x')
    )

    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:5)),
                                   indexRange(quote(3:5)),
                                   indexRange(quote(5:7)))), rule),
        NULL
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(matrix(c(3,4,1,1,1,3,3,3,4,3,2,1,4,1,5), ncol = 3)))), rule),
        varRangeClass$new(list(indexRange(2), indexRange(matrix(c(3,1), ncol = 1))), varName = 'x')
    )
    
   n <- c(1,3,2)

   rule <- makeGraphRule(LHS = quote(x[2]),
                                 RHS = quote(y[i,j]),
                                context = context_ijni_short,
                                constants = list(n = n))

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(indexRange(2),
                                  indexRange(2))), rule),
       varRangeClass$new(list(indexRange(2)), varName = 'x')
   )
   
   expect_identical(
       applyGraphRule(
           varRangeClass$new(list(indexRange(1),
                                  indexRange(3))), rule),
       NULL
   )

   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(indexRange(1:2),
                                  indexRange(1:3))), rule),
       varRangeClass$new(list(indexRange(2)), varName = 'x')
   )

   expect_identical(
       applyGraphRule(
           varRangeClass$new(list(indexRange(1),
                                  indexRange(2:3))), rule),
       NULL
   )
   
   expect_equal(
       applyGraphRule(
           varRangeClass$new(list(indexRange(matrix(c(1,1,1,3), ncol = 2)))), rule),
       varRangeClass$new(list(indexRange(2)), varName = 'x')
   )

   expect_identical(
       applyGraphRule(
           varRangeClass$new(list(indexRange(matrix(c(1,3,3,3), ncol = 2)))), rule),
       NULL
   )

   rule <- makeGraphRule(LHS = quote(y[j]),
                                 RHS = quote(x[j,i+3*j]),
                                context = context_ij)
   expect_identical(length(rule$indexRules), 1L)
   expect_true(is(rule$indexRules[[1]], "indexRuleClass_arbitrary"))


   rule <- makeGraphRule(LHS = quote(x),
                                RHS = quote(y[i+j,j]),
                                context = context_ij)
   expect_identical(
       applyGraphRule(
           varRangeClass$new(list(
                             indexRange(matrix(c(1,1,1,1),2)))), rule),
       NULL
   )

})

test_that("Single index variable used multiple times", {

    rule <- makeGraphRule(LHS = quote(y[i, i+1]),
                                 RHS = quote(x[i]),
                                 context = context_i)
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(3:5)))), rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(3,4,5,4,5,6), ncol = 2))), varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y[i]),
                                 RHS = quote(x[i,i+1]),
                                 context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4,5,4,5,6), ncol = 2)))), rule),
        varRangeClass$new(list(
                          indexRange(quote(3:5))), varName = 'y')
    )
    
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,5,5,4,5,6), ncol = 2)))), rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(3,5)))), varName = 'y')
    )
    
    expect_identical(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,5,4,5,5,6), ncol = 2)))), rule),
        NULL
    )
    
    rule <- makeGraphRule(LHS = quote(y[i,i]),
                                 RHS = quote(x[i,i]),
                                 context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,2,3,2,3,3), nrow=3)))), rule),
        varRangeClass$new(list(indexRange(matrix(c(2,3,2,3), ncol=2))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(2),
                              indexRange(2))), rule),
        varRangeClass$new(list(indexRange(matrix(c(2,2), nrow = 1))), varName = 'y')
    )

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(indexRange(matrix(c(2,3,2,3), ncol = 2))), varName = 'y')
    )

    rule <- makeGraphRule(LHS = quote(y[i+1,i]),
                                RHS = quote(x[2]),
                                context = context_i)

    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(cbind(2:11, 1:10))), varName = 'y')
    )
})


test_that("Correct indexRule is used for various cases", {
    ## This produces uses arbitrary rule, because
    ## we only detect i+constant as setting up a sequence rule,
    ## but matrix is converted to sequence in post-processing.
    rule <- makeGraphRule(LHS = quote(y[i]),
                                 RHS = quote(x[1+i]),
                                 context = context_i)

    expect_true(is(rule$indexRules[[1]],"indexRuleClass_arbitrary"))
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(
                              indexRange(quote(3:6)))),
            rule),
        varRangeClass$new(list(indexRange(quote(2:5))), varName = 'y')
    )
})


test_that("indexRange matrix converted to sequence if appropriate", {
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:5){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:3){}))
    
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))
    
    k <- c(2,4,3,5)
    rule <- makeGraphRule(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[k[i-1],j]),
                                 context = context_ij,
                                 constants = list(k = k))
    expect_equal(
        applyGraphRule(
            varRangeClass$new(list(indexRange(quote(2:5)),
                                   indexRange(2))),rule),
        varRangeClass$new(list(indexRange(quote(3:6)), indexRange(2)), varName = 'y')
    )
})


test_that("getFromRange", {
    rule <- graphRuleClass$new(LHS = quote(y[i]),
                          RHS = quote(x[i+2]),
                          context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(indexRange(quote(1:12))),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(LHS = quote(y[i]),
                          RHS = quote(x[2:3]),
                          context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(indexRange(quote(1:3))),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(LHS = quote(y[i]),
                          RHS = quote(x),
                          context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(nimbleModel:::indexRange_none()),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(LHS = quote(y[i]),
                          RHS = quote(x[k[i]+2]),
                          context = context_i_short,
                          constants = list(k = c(4,2,7)))
    expect_equal(rule$getFromRange(), varRangeClass$new(list(indexRange(quote(1:9))),
                                                            varName = 'x'))

    rule <- graphRuleClass$new(LHS = quote(y[i,j]),
                          RHS = quote(x[k[i]+2,j,i]),
                          context = context_ij_short,
                          constants = list(k = c(4,2,7)))
    expect_equal(rule$getFromRange(), varRangeClass$new(list(indexRange(quote(1:9)),
                                                             indexRange(quote(1:4)),
                                                             indexRange(quote(1:3))),
                                                            varName = 'x'))

    ## getParents, all rule cases
    
    rule <- graphRuleClass$new(LHS = quote(x[3]),
                          RHS = quote(y[i+2]),
                          context = context_i)
    expect_equal(rule$getFromRange(), varRangeClass$new(list(indexRange(quote(1:12))),
                                                            varName = 'y'))

    ## 'fullRange' here is just an example from the RHS as that is all that is needed
    rule <- graphRuleClass$new(LHS = quote(x[3]),
                          RHS = quote(y[k[i]+2]),
                          context = context_i_short,
                          constants = list(k = c(4,2,7)))
    
    expect_equal(rule$getFromRange(), varRangeClass$new(list(indexRange(quote(1:4))),
                                                            varName = 'y'))

 
    rule <- graphRuleClass$new(LHS = quote(x[3]),
                          RHS = quote(y[k1[i],k2[i]]),
                          context = context_i_short,
                          constants = list(k1 = c(4,2,7), k2 = c(9,1,4)))
    expect_equal(rule$getFromRange(), varRangeClass$new(list(indexRange(quote(1:2)),
                                                             indexRange(quote(1:1))),
                                                            varName = 'y'))

    
    
 

})
    
