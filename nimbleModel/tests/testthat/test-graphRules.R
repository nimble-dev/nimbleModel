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
vrEmpty <- varRangeClass$new(list(irEmpty))
vrEmpty_mat2 <- varRangeClass$new(list(irEmpty), list(1:2))  ## single indexRange, two indices
vrEmpty2 <- varRangeClass$new(list(irEmpty, irEmpty))
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
        varRangeClass$new(list(indexRange(quote(1:2))))
    )
})

test_that("error trap incorrect number of input indices", {
    ## Single simple sequence rule
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i]),
                                 context = context_i)

    ## incorrect number of input indexRanges
    expect_error(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3),
                              indexRange(4))), rules),
        "incorrect number of input indices"
    )

    expect_error(
        applyGraphIndexRules(
            vrNone, rules),
        "incorrect number of input indices"
    )

    ## incorrect length of input matrix indexRange
    expect_error(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4), nrow = 1)))), rules),
        "incorrect number of input indices"
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[2, 3]),
                                 context = context_i)

    expect_error(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3))), rules), 
        "incorrect number of input indices"
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x),
                                context = context_i)

    expect_error(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rules),
        "incorrect number of input indices"
    )

})

test_that("handle incorrect indexing", {
    ## Do we want to throw an error?
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x[i]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(-3))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(0))), rules),
        vrEmpty
    )

})

test_that("graphRules works for basic cases lacking LHS indexing", {
    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            vrNone, rules),
        vrNone
    )

    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(3))), rules),
        vrEmpty
    )
})

test_that("graphRules works for single index cases, wrapping indexRules", {
    ## indexRule_block
    
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

   ## Checking that NAs produced by invalid input matrix entries are discarded
   ## in graphRules processing.

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(10, 12, 14), ncol = 1)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(9))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(12, 14), ncol = 1)))), rules),
        vrEmpty
    )

    ## indexRule_all

    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[]),
                                 context = context_i)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))


    ## more complicated expression than simple index
    rules <- makeGraphIndexRules(LHS = quote(y[3*i]),
                                RHS = quote(x[]),
                                context = context_i)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(3))), rules),
        varRangeClass$new(list(indexRange(matrix(seq(3, 30, by = 3), ncol = 1))))
    )

    ## indexRule_constant

    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    idx <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 1 and 2 are repeated
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[ idx[i] ]),
                                 context = context_i,
                                 constants = list(idx = idx))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:5)))), rules), ## 2 yields multiples; 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(matrix(bruteForceNestedIndexing(idx, 2:5)))))
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y[idx[i]]),
                                 RHS = quote(x[i]),
                                 context = context_i,
                                 constants = list(idx = idx))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:7)))), rules), 
        varRangeClass$new(list(
                          indexRange(matrix(idx[3:7]))))
    )

})

test_that("graphRules works for various basic multiple index cases", {

    ## two all rules, crossed (of course)
    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(varRangeClass$new(list(indexRange(2))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:4))))
    )
   
    expect_equal(
        applyGraphIndexRules(varRangeClass$new(list(indexRange(3))), rules),
        vrEmpty2
    )

    ## all plus constant, crossed (of course)
    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            vrNone, rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    ## two constant rules, crossed (of course)
    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            vrNone, rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    ## block plus constant, crossed (of course)
    rules <- makeGraphIndexRules(LHS = quote(y[2,i+1]),
                                 RHS = quote(x[i]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    ## This gives mixed result with one indexRange empty and one not.
    ## I think that makes sense where there is a direct from->to mapping,
    ## but later code will need to determine that this result is an empty
    ## overall varRange even though at least one indexRange is not empty.
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(22:24)))), rules),
        varRangeClass$new(list(indexRange(quote(2)), irEmpty))
    )

    ## block plus all, crossed (of course)
    rules <- makeGraphIndexRules(LHS = quote(y[j,i]),
                                 RHS = quote(x[i]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(quote(1:4)),
                               indexRange(quote(2:3))))
    )

    ## two block rules, crossed
    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                 RHS = quote(x[i, j]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(3:4))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(5:9)))), rules),
        varRangeClass$new(list(
                          indexRange(quote(2:4)),
                          irEmpty))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(matrix(c(1,3,5), ncol = 1)))), rules),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(matrix(c(1,3), ncol = 1))))
    )
})


test_that("graphRules works for arbitrary rules of multiple contexts entangled with other indices", {

    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                RHS = quote(x[i+3*j,j]),
                                context = context_ij)
    expect_identical(length(rules$indexRules), 1L)
    expect_true(is(rules$indexRules[[1]], "indexRuleClass_arbitrary"))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:7)),
                              indexRange(quote(1:3)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(1,1,2,1,3,1,4,1,1,2), byrow = TRUE, ncol = 2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(1,2,2,2), byrow = TRUE, ncol = 2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(4,2), ncol = 2)))), rules),
        vrEmpty_mat2
    )

    ## version using k[i,j]
    k <- matrix(outer(1:10, 1:4, function(i,j) i+3*j), nrow = 10, ncol = 4)
    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                RHS = quote(x[k[i,j],j]),
                                context = context_ij,
                                constants = list(k = k))
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:7)),
                              indexRange(quote(1:3)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(1,1,2,1,3,1,4,1,1,2), byrow = TRUE, ncol = 2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(1,2,2,2), byrow = TRUE, ncol = 2))))
    )

    ## getParents version
    rules <- makeGraphIndexRules(LHS = quote(y[i+3*j, j]),
                                RHS = quote(x[i,j]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(1:2)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(5,1,6,1,8,2,9,2), byrow = TRUE, ncol = 2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,1,3,1,3,2,11,3), byrow = TRUE, ncol = 2)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(5,1,6,1,9,2), byrow = TRUE, ncol = 2))))
    )


    ## Additional LHS index not used on RHS, 'all' scenario embedded within arbitrary rule.
    rules <- makeGraphIndexRules(LHS = quote(y[i+3*j, j]),
                                RHS = quote(x[i]),
                                context = context_ij)
    expect_identical(length(rules$indexRules), 1L)
    expect_true(is(rules$indexRules[[1]], "indexRuleClass_arbitrary"))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:2)))), rules), 
        varRangeClass$new(list(
               indexRange(matrix(c(4,1,7,2,10,3,13,4,5,1,8,2,11,3,14,4), byrow = TRUE, ncol = 2))))
    )

    ## More complicated such case where first i,j need to be crossed.
    rules <- makeGraphIndexRules(LHS = quote(y[i+j, j+k]),
                                 RHS = quote(x[i,j]),
                                 context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(matrix(c(2,4,6))))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(4,3,4,4,4,5,5,3,5,4,5,5,6,5,6,6,6,7,7,5,7,6,7,7), byrow = TRUE, ncol = 2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1)),
                              indexRange(quote(6)))), rules),
        vrEmpty_mat2
    )

    ## getParents version
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x[i+3*j, j]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(99,2,4,12,4,2,7,2,8,2), byrow = TRUE, ncol = 2)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(1,2), ncol = 1))))
    )
    

   ## include an additional index tied in in some cases via the input varRange
   rules <- makeGraphIndexRules(LHS = quote(y[j+3*i,j,k]),
                                 RHS = quote(x[i, k, j]),
                                 context = context_ijk)

   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(2,3), ncol = 1)),
                             indexRange(matrix(c(1,3,1,2), nrow = 2)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(7,10,8,11,1,1,2,2,1,1,3,3), ncol = 3)))))
   
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(2,3,1,2,1,3), nrow = 2)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(7,1,1,12,3,2), byrow = TRUE, ncol = 3)))))
   
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(2,3), ncol = 1)),
                             indexRange(matrix(c(1,2), ncol = 1)),
                             indexRange(matrix(c(1,3), ncol = 1)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(7,10,9,12,1,1,3,3), ncol = 2)),
                              indexRange(matrix(c(1,2), ncol = 1)))))

})


test_that("graphRules works for arbitrary rule with ragged blocks combined with another rule", {

    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                RHS = quote(x[i, 1:j]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(
                          indexRange(quote(2:4)),
                          indexRange(matrix(c(2,3,4,3,4), ncol = 1))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2, 3, 1, 2, 99, 3), byrow = TRUE, ncol = 2)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,3,2,4,1,2,1,3,1,4), byrow = TRUE, ncol = 2))))
    )

    ## getParents version
    rules <- makeGraphIndexRules(LHS = quote(y[i, 1:j]),
                                RHS = quote(x[i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(
                          indexRange(quote(2:4)),
                          indexRange(matrix(c(1,2,1,2,3), ncol = 1))))
    )
    

    rules <- makeGraphIndexRules(LHS = quote(y[i, 1:i, j]),
                                RHS = quote(x[i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(2:4)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,1,2,2,3,1,3,2,3,3), byrow = TRUE, ncol = 2)),
                          indexRange(quote(2:4))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2, 3, 1, 4, 99, 3), byrow = TRUE, ncol = 2)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,1,3,2,2,3,1,1,4), byrow = TRUE, ncol = 3))))
    )

    ## getParents version
    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                RHS = quote(x[i, 1:i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,1,2,2,1,1), nrow = 3, byrow = TRUE)),
                              indexRange(quote(2:4)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(2,2,1), ncol = 1)),
                          indexRange(quote(2:4))))
    )
})

test_that("graphRules works for two rules that use a single indexRange matrix", {
    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                RHS = quote(x[i, j]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(7,3,4,2), ncol = 2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(7,3,4,2), ncol = 2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(11,3,4,2), ncol = 2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(3,2), ncol = 2))))
    )

    ## single empty indexRange because results of the two rules get combined back together
    ## as we need to check by row if combined result is valid
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(11,3,4,5), ncol = 2)))), rules),
        vrEmpty_mat2
    )

    idx1 <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 2 is repeated and 3,4 absent
    idx2 <- c(1, 1, 2, 1) 

    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                 RHS = quote(x[ idx1[i] , idx2[j]]),
                                 context = context_ij,
                                 constants = list(idx1 = idx1, idx2 = idx2))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(5,5,5,3,3,3,2,2,2,1,4,2,1,4,2,1,4,2), ncol = 2)))), rules), 
        varRangeClass$new(list(indexRange(matrix(c(9,9,9,9,3,7,3,7,3,7,3,7,1,2,4,3,1,1,2,2,4,4,3,3), ncol = 2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                 RHS = quote(x[ idx1[i] , j]),
                                 context = context_ij,
                                 constants = list(idx1 = idx1))

    ## Apply rule to a sequence indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(5,5,3,2,2,2,5,2,5,4), ncol = 2)))), rules), 
        varRangeClass$new(list(indexRange(matrix(c(9,3,7,2,4,4), ncol = 2))))
    )
    
    ## effect of having a RHS constraint, with indexRange matrix inputs
    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                 RHS = quote(x[3, i, j]),
                                 context = context_ij)

    ## constraint results in two empty ranges instead of one, despite input indexRange matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(4), 
                              indexRange(matrix(c(1,1,2,2), ncol = 2)))), rules),
        vrEmpty2
    )
    
    ## constraint results in two empty ranges instead of one, despite input indexRange matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(4,7,1,1,2,2), ncol = 3)))), rules),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(4,3,1,1,2,2), ncol = 3)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(1,2), ncol = 2))))
    )

})

test_that("graphRules works for all rule that is function of two indexes", {
    rules <- makeGraphIndexRules(LHS = quote(y[j+3*i]),
                                 RHS = quote(x[2]),
                                 context = context_ij_short)

    ## Note that there are duplicate results.
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(4:7,7:10,10:13)))))
    )

})


test_that("graphRules works for fixed RHS indices", {
    ## Start by just using the simplest LHS given focus here is on RHS constraints.

    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            vrNone, rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    
    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(3))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(3:4, ncol = 1)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2:3, ncol = 1)))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)))), rules),
        vrNone
    )

    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(3))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(3:20))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,20), ncol = 1)))), rules),
        vrNone
    )

    expect_error(  ## Not yet checking input variable bounds in blank index case
        expect_equal(
            applyGraphIndexRules(
                varRangeClass$new(list(
                                  indexRange(quote(33)))), rules),
            vrEmpty)
    )

    expect_error(  ## Not yet checking input variable bounds in blank index case
        expect_equal(
            applyGraphIndexRules(
                varRangeClass$new(list(indexRange(matrix(c(20,55), ncol = 1)))), rules),
            vrEmpty)
    )

    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2:3]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(3))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(4))), rules),
        vrEmpty
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(4,6), ncol = 1)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(4:5)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)))), rules),
        vrNone
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2,3]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2), indexRange(3))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2), indexRange(4))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(1), indexRange(4))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,4), ncol = 1)),
                                   indexRange(matrix(c(1,3), ncol = 1)))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)),
                                   indexRange(matrix(c(3,5), ncol = 1)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)),
                                   indexRange(matrix(c(4,6), ncol = 1)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(2:3)))), rules),
        vrNone
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(4:5)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)),
                                   indexRange(quote(4:5)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,4,4,3), ncol = 2)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3), ncol = 2)))), rules),
        vrNone
    )

    ## Various cases where the index of the constraint is or is not tangled up in an indexRule
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x[2,3]),
                                context = context_i_short)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,5), ncol = 2)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3), ncol = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(1:3))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x[2,3,i]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,5,1,2), ncol = 3)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3,2,4), ncol = 3)))), rules),
        varRangeClass$new(list(indexRange(matrix(4))))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,5), ncol = 2)),
                                   indexRange(quote(2:3)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,3), ncol = 2)),
                                   indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:3))))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:5)),
                                   indexRange(matrix(c(3,4,2,2), ncol = 2)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(matrix(c(1,4,2,2), ncol = 2)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(matrix(c(3,4,2,2), ncol = 2)))), rules),
        varRangeClass$new(list(indexRange(matrix(2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:5)),
                                   indexRange(quote(3:4)),
                                   indexRange(quote(2:3)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(3:4)),
                                   indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:3))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[]),
                                 context = context_i)
    expect_identical(rules$constraints[[1]]$constraint, c(1, Inf))

    
})



test_that("graphRules works for reordered columns", {
    rules <- makeGraphIndexRules(LHS = quote(y[j, i, k]),
                                RHS = quote(x[k, i, j]),
                                context = context_ijk_short)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:2)),
                              indexRange(quote(2:3)),
                              indexRange(quote(1:4)))), rules),
        varRangeClass$new(list(
                          indexRange(quote(1:4)),
                          indexRange(quote(2:3)),
                          indexRange(quote(1:2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(1,2,2,3), nrow = 2)),
                              indexRange(quote(1:4)))), rules),
        varRangeClass$new(list(
               indexRange(quote(1:4)),
               indexRange(matrix(c(2,3,1,2), nrow = 2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[k, i, j]),
                                RHS = quote(x[i, j, k]),
                                context = context_ijk_short)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(1:4)),
                              indexRange(quote(1:2)))), rules),
        varRangeClass$new(list(
               indexRange(quote(1:2)),
               indexRange(quote(2:3)),
               indexRange(quote(1:4))))
    )

    ## reordering with an indexRange matrix covering noncontiguous indices
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(matrix(c(1,2,2,3), ncol = 2)))), rules),
        varRangeClass$new(list(
               indexRange(matrix(c(2,3,1,2), ncol = 2)),
               indexRange(quote(2:3))), indexOrders = list(c(1,3), 2))
                                           
    )
})

test_that("graphRules correctly handles ragged indexing", {

   n <- c(1,3,2)
   mi <- c(2,1,3)
   mj <- c(3,2,1)
   mij <- matrix(c(1,3,2,3,1,3,2,2,1), ncol = 3)

   ## Ragged indexing induces arbitrary rule for the index and its 'parent'
   
   rules <- makeGraphIndexRules(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijnik_short,
                             constants = list(n = n))
   expect_identical(sapply(rules$indexRules, function(ir) class(ir)[1]),
                    c('indexRuleClass_arbitrary', 'indexRuleClass_block'))
   expect_identical(rules$indexSets$LHSindex2setID, c(2, 1, 1))

   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)))), rules),
       varRangeClass$new(list(indexRange(quote(1:2)),
                              indexRange(matrix(c(1,1,2,1,2,2), nrow = 3))))
   )

   rules <- makeGraphIndexRules(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijnikni_short,
                             constants = list(n = n, mi = mi))
   expect_identical(sapply(rules$indexRules, function(ir) class(ir)[1]),
                    'indexRuleClass_arbitrary')
   expect_identical(rules$indexSets$LHSindex2setID, rep(1,3))
   
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)),
                             indexRange(quote(1:2)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(1,2,1,1,1,1,1,2,1,1,2,2), nrow = 4))))
   )

   rules <- makeGraphIndexRules(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijniknj_short,
                             constants = list(n = n, mj = mj))
   expect_identical(sapply(rules$indexRules, function(ir) class(ir)[1]),
                    'indexRuleClass_arbitrary')
   expect_identical(rules$indexSets$LHSindex2setID, rep(1,3))
   
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(quote(2:3)),
                             indexRange(quote(1:3)),
                             indexRange(quote(2:3)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(2,3,2,2,3,2,1,1,2,1,1,2,2,2,2,3,3,3), ncol = 3))))
   )

   rules <- makeGraphIndexRules(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijniknij_short,
                             constants = list(n = n, mij = mij))
   expect_identical(sapply(rules$indexRules, function(ir) class(ir)[1]),
                    'indexRuleClass_arbitrary')
   expect_identical(rules$indexSets$LHSindex2setID, rep(1,3))

   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(quote(2:3)),
                             indexRange(quote(1:3)),
                             indexRange(quote(1:2)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(2,3,2,1,1,3,2,2,2), ncol = 3))))
   )

   ## This invokes complicated crossing case
   rules <- makeGraphIndexRules(LHS = quote(x[j]),
                                 RHS = quote(y[i,j,3]),
                                context = context_ijni_short,
                                constants = list(n = n))
   expect_identical(length(rules$indexRules), 1L)
   expect_true(is(rules$indexRules[[1]], "indexRuleClass_arbitrary"))

   ## NOTE: this case has redundant results.
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(quote(2:3)),
                                  indexRange(matrix(c(3,2,2,3,3,4), ncol = 2)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(3,2,2), ncol = 1))))
   )

})

test_that("graphRules works for non-contiguous indices in an indexRange", {
    ## Case of y[i,j,k] where i,k are in an indexRange together
    rules <- makeGraphIndexRules(LHS = quote(y[i,k,j]),
                                 RHS = quote(x[i,j,k]),
                             context = context_ijk)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(1,2,1,2), nrow = 2)),
                              indexRange(matrix(c(1,3),ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(1,2,1,2), nrow = 2)),
                               indexRange(matrix(c(1,3), ncol = 1))),
                          indexOrders = list(c(1,3), 2)))
})

test_that("graphRules works for getParents by checking 1-to-many case", {
    ## use y[i] -> x[2] to emphasize getParents use case; y[i] is "RHS"

    ## should some of these tests be moved to test-indexRules_any.R ?
    ## perhaps any that don't involve weird crossing
    ## use that to test simple cases that input scalar, seq, 1-col matrix and 2-col matrix handled properly

    rules <- makeGraphIndexRules(LHS = quote(x),
                                RHS = quote(y[i]),
                                context = context_i_short)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(3))), rules),
        varRangeClass$new(list(irNone))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(4))), rules),
        vrEmpty
    )

    rules <- makeGraphIndexRules(LHS = quote(x[2]),
                                RHS = quote(y[i]),
                                context = context_i_short)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(4:5)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(4,6), ncol = 1)))), rules),
        vrEmpty
    )

    k <- c(2,5,1)
    rules <- makeGraphIndexRules(LHS = quote(x[2]),
                                 RHS = quote(y[k[i]]),
                                 context = context_i_short,
                                 constants = list(k = k))


    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(6)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(5:6)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)))), rules),
        vrEmpty
    )
 
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(6:7)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,5), ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,4), ncol = 1)))), rules),
        vrEmpty
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(6,9), ncol = 1)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,9), ncol = 1)))), rules),
        vrEmpty
    )

    rules <- makeGraphIndexRules(LHS = quote(x[2]),
                                RHS = quote(y[i,3]),
                                context = context_i_short)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(4:5)),
                                   indexRange(quote(2:3)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)),
                                   indexRange(quote(4:5)))), rules),
        vrEmpty
    )


    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2:3, ncol = 1)),
                                   indexRange(matrix(2:3, ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(4:5, ncol = 1)),
                                   indexRange(matrix(2:3, ncol = 1)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2:3, ncol = 1)),
                                   indexRange(matrix(4:5, ncol = 1)))), rules),
        vrEmpty
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,4), ncol = 2)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,4,5,3), ncol = 2)))), rules),
        vrEmpty
    )    

    rules <- makeGraphIndexRules(LHS = quote(x[2]),
                                RHS = quote(y[i,j]),
                                context = context_ij_short)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2),
                                   indexRange(4))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(4),
                                   indexRange(4))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2),
                                   indexRange(5))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)),
                                   indexRange(quote(4:5)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )


    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)),
                                   indexRange(quote(5:6)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(4:5)),
                                   indexRange(quote(5:6)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(1,4), ncol = 1)),
                                   indexRange(matrix(c(6,4), ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(4,7), ncol = 1)),
                                   indexRange(matrix(c(6,4), ncol = 1)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,5), ncol = 2)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,4,5,3), ncol = 2)))), rules),
        vrEmpty
    )

    rules <- makeGraphIndexRules(LHS = quote(x[2]),
                                 RHS = quote(y[i+3*j]),
                                 context = context_ij_short)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(6)))), rules),
        varRangeClass$new(list(indexRange(2)))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3)))), rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(100)))), rules),
        vrEmpty
    )

    rules <- makeGraphIndexRules(LHS = quote(x[2,i]),
                                RHS = quote(y[i,3,j]),
                                context = context_ij_short)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:5)),
                                   indexRange(quote(3:5)),
                                   indexRange(quote(4:7)))), rules),
        varRangeClass$new(list(indexRange(2), indexRange(quote(2:3))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:5)),
                                   indexRange(quote(3:5)),
                                   indexRange(quote(5:7)))), rules),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,4,1,1,1,3,3,3,4,3,2,1,4,1,5), ncol = 3)))), rules),
        varRangeClass$new(list(indexRange(2), indexRange(matrix(c(3,1), ncol = 1))))
    )
    
   n <- c(1,3,2)

   rules <- makeGraphIndexRules(LHS = quote(x[2]),
                                 RHS = quote(y[i,j]),
                                context = context_ijni_short,
                                constants = list(n = n))

   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(2),
                                  indexRange(2))), rules),
       varRangeClass$new(list(indexRange(2)))
   )
   
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(1),
                                  indexRange(3))), rules),
       vrEmpty
   )

   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(1:2),
                                  indexRange(1:3))), rules),
       varRangeClass$new(list(indexRange(2)))
   )

   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(1),
                                  indexRange(2:3))), rules),
       vrEmpty
   )
   
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(matrix(c(1,1,1,3), ncol = 2)))), rules),
       varRangeClass$new(list(indexRange(2)))
   )

   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(matrix(c(1,3,3,3), ncol = 2)))), rules),
       vrEmpty
   )

   rules <- makeGraphIndexRules(LHS = quote(y[j]),
                                 RHS = quote(x[j,i+3*j]),
                                context = context_ij)
   expect_identical(length(rules$indexRules), 1L)
   expect_true(is(rules$indexRules[[1]], "indexRuleClass_arbitrary"))


   rules <- makeGraphIndexRules(LHS = quote(x),
                                RHS = quote(y[i+j,j]),
                                context = context_ij)
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(1,1,1,1),2)))), rules),
       vrEmpty
   )

})

test_that("Single index variable used multiple times", {

    rules <- makeGraphIndexRules(LHS = quote(y[i, i+1]),
                                 RHS = quote(x[i]),
                                 context = context_i)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(3,4,5,4,5,6), ncol = 2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i,i+1]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4,5,4,5,6), ncol = 2)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(3:5))))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,5,5,4,5,6), ncol = 2)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(3,5)))))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,5,4,5,5,6), ncol = 2)))), rules),
        vrEmpty
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y[i,i]),
                                 RHS = quote(x[i,i]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,2,3,2,3,3), nrow=3)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(2,3,2,3), ncol=2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(2),
                              indexRange(2))), rules),
        varRangeClass$new(list(indexRange(matrix(c(2,2), nrow = 1))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)),
                              indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(2,3,2,3), ncol = 2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,i]),
                                RHS = quote(x[2]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(cbind(2:11, 1:10))))
    )
})


test_that("Correct indexRule is used for various cases", {
    ## This produces a matrix and uses arbitrary rule, because
    ## we only detect i+constant as setting up a sequence rule.
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[1+i]),
                                 context = context_i)

    expect_true(is(rules$indexRules[[1]],"indexRuleClass_arbitrary"))
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)))),
            rules),
        varRangeClass$new(list(indexRange(matrix(2:5, ncol = 1))))
    )
})


test_that("indexRange matrix converted to sequence if appropriate", {
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:5){}))
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:3){}))
    
    context_ij <- modelContextClass$new(list(singleContext1, singleContext2))
    
    k <- c(2,4,3,5)
    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[k[i-1],j]),
                                 context = context_ij,
                                 constants = list(k = k))
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:5)),
                                   indexRange(2))),rules),
        varRangeClass$new(list(indexRange(quote(3:6)), indexRange(2)))
    )
})
