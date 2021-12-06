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

test_that("makeSeparableIndexSets works", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 1:5){}))
    
    ## alternative way to specific a single context
    singleContext2ni <-
        modelSingleContext(indexVarExpr = quote(j),
                           indexRangeExpr = quote(1:n[i]),
                           )
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
    
    context_ijk <- modelContextClass$new(list(singleContext1,
                                              singleContext2,
                                              singleContext3))
    
    context_ijni<- modelContextClass$new(list(singleContext1,
                                              singleContext2ni))
    
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

    expect_identical(
        makeSeparableIndexSets(quote(y[j, i]),
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
}
)

## Test various child cases:

## (done) [block] y[i] <- x[i], x[i,2], x[i,2:3], x[i,]
## (done) [all] y[i] <- x, x[2], x[2:3], x[], x[2,3], x[2,]
## (done) [constant] y[2] or y[2:3] <- x, x[2], x[2:3], x[], x[2,3], x[2,], x[2,i]
## (done) [constant] y <- x, x[2], x[2:3], x[], x[2,3], x[2,]
## (done) [all,all] y[i,j] <- x, x[2], x[2:3], x[2,4], x[], x[,2], x[2,3,4]
## [all,block] y[j,i] <- x[i], x[i,2], x[i,2:3], x[i,], x[i,2,4]  ## handle reordering as part of tests to avoid additional tests
## [constant,block] y[2,i] <- x[i], x[i,2], x[i,2:3], x[i,], x[i,2,3] 
## (done) [constant,all] y[2,i] <- x, x[2], x[2:3], x[2,4], x[,2], x[], x[2,3,4]
## [constant,constant] y[2,2:3] <- x, x[2], x[2:3], x[2,4], x[,2], x[,2], x[2,3,4]
## [block,block] y[i,j] <- x[i,j], x[i,j,2]
## [block,block,block] y[i,j,k] <- x[k,i,j], x[2,k,i,j]  ## presumably sufficient for other reordering cases
## non-separable cases

## Test parent cases (should we just test all cases above in reverse?)
## Carefully consider cases where where have LHS indexes not appearing on RHS
## since this pattern of having every input index produce same output index (many to one)
## doesn't occur for determining children.
## y[i] -> x, x[2], x[2:3], x[], x[2,3], x[2,]
## That case may be sufficient or we may also need:
## y[2,i] -> x[i], x[i,2], x[i,2:3], x[i,], x[i,2,3]
## y[2,i] <- x, x[2], x[2:3], x[2,4], x[,2], x[,2], x[i,2,3]
## y[i,j] -> x, x[2], x[2:3], x[2,4], x[,2], x[,2], x[2,3,4]
## y[j,i] -> x[i], x[i,2], x[i,2:3], x[i,], x[i,2,4]

## need to consider arbitrary indexes in the various cases above, e.g., y[i] <- x[k[i]]
## also y[k[i]] <- x[i] or y[k[i]] <- x[j[i]]

## multiple mapping cases
## y[i] <- x[i, i+1]
## y[i, i] <- x[i]

irEmpty <- nimbleModel:::indexRange_empty()
vrEmpty <- varRangeClass$new(list(irEmpty))

test_that("graphRules works for 1D sequence rule", {
    ## y[i] from x[i], etc.

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    ## Single simple sequence rule
    rule <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)

    ## block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    ## Apply rule to a matrix indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(
                              matrix(c(2, 4), nrow = 2)
                          ))), rule),
        varRangeClass$new(list(indexRange(
                          matrix(c(1, 3), nrow = 2)
           )))
    )

    ## Apply rule to a matrix indexRange with only 1 row
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(
                             matrix(c(3), nrow = 1)
                         ))), rule),
       varRangeClass$new(list(indexRange(
                   matrix(c(2), nrow = 1)
           )))
    )

    ## check out of bounds case

    ## Apply to a matrix with no inputs in the right range
    expect_equal(
       applyGraphIndexRules(varRangeClass$new(list(indexRange(
                                              matrix(c(12, 13), nrow = 2)
                    ))), rule),
       vrEmpty)
    
    ## Are the following worth testing these given we have equivalent testing in test-indexRules_block.R?
    
    ## Apply to a matrix with one of two inputs in the right range
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(3, 12), nrow = 2)
                    )))
          , rule)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(c(2), nrow = 1)
           )))
    )


    ## Apply to a block with not all valid inputs
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(8:13)
                    )))
          , rule)
       ,
       test2 <-  varRangeClass$new(
           list(
               indexRange(
                   quote(7:10)
               )))
    )

    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(1:4)
                    )))
          , rule)
       ,
       test2 <-  varRangeClass$new(
           list(
               indexRange(
                   quote(1:3)
               )))
    )

    ## With no valid inputs
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(12:15)
                    )))
          , rule)
       , vrEmpty)

    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        quote(0:1)
                    )))
          , rule)
       , vrEmpty)


    ## Simple sequence rule with additional indexing cases
    
    rule <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[2, i+1]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )


    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(3:6)))), rule),
        vrEmpty)

    rule <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[2:3, i+1]),
                                 context = context_i)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:4)),
                              indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(4:5)),
                              indexRange(quote(3:6)))), rule),
        vrEmpty)

    rule <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x[, i+1]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(3:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

})

test_that("graphRules works for 1D all rule", {
    ## y[i] from x[], x[2], etc.
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    context_i <- modelContextClass$new(list(singleContext1))

    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_error(  ## Not yet checking input variable bounds in blank index case
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(33)))), rule),
        vrEmpty)
    )
    

    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        vrEmpty)

    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2:3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:4)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(5:6)))), rule),
        vrEmpty)
    
    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2,3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)), indexRange(quote(3)))), rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), indexRange(quote(4)))), rule),
            vrEmpty)

    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[,3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), indexRange(quote(4)))), rule),
        vrEmpty)

    expect_error(  ## Not yet checking input variable bounds in blank index case
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(33)), indexRange(quote(3)))), rule),
        vrEmpty)
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x),
                                 context = context_i)

    ## Not sure if indexRange(0) is how we want to handle `x`.
    ## We have NULL as part of constraint if blank indexing, so use of NULL here doesn't work.
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(0))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        vrEmpty)
    
})


test_that("graphRules works for 1D constant rule", {
    ## y[2] from x[3], etc.

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    context_i <- modelContextClass$new(list(singleContext1))

    context_0 <- modelContextClass$new()

    rule <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3)))), rule),
        vrEmpty)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)))), rule),
        vrEmpty)

    rule <- makeGraphIndexRules(LHS = quote(y[2:3]),
                                RHS = quote(x[2]),
                                context = context_0)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:3)))))

    rule <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2:3]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    rule <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2,3]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    rule <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))


    rule <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2,]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))


    rule <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(0))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        vrEmpty)
})

test_that("graphRules works for 1D constant rule, LHS no indexing", {
    ## y from x[2] etc.

    context_0 <- modelContextClass$new()

    rule <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rule),
        varRangeClass$new(list(indexRange(quote(0)))))

    rule <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2:3]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rule),
        varRangeClass$new(list(indexRange(quote(0)))))

    rule <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2,3]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2), indexRange(3))), rule),
        varRangeClass$new(list(indexRange(quote(0)))))
    
    rule <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rule),
        varRangeClass$new(list(indexRange(quote(0)))))

    rule <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2,]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(0)))))
    
    rule <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(0))), rule),
        varRangeClass$new(list(indexRange(quote(0)))))
})

test_that("graphRules works for 2D all rule", {
    ## y[i,j] from x[], x[2], etc.
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    rule <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2:3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2,3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[,3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2,3,4]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)),
                              indexRange(quote(4)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )
})

test_that("graphRules works for 2D all+constant rule", {
    ## y[i,2] from x[], x[2], etc.
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))


    rule <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[2:3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[2, 3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[, 2]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[2, 3, 4]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)),
                              indexRange(quote(4)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )
})


lnm('eir'); library(testthat)
irEmpty <- nimbleModel:::indexRange_empty()
vrEmpty <- varRangeClass$new(list(irEmpty))



## save for later
    ## (ok now?) I think I need to rework makeSeparableIndexSets to handle constants
    ## icnluding y[i,j,2], y[i,2], etc. Think about contexts, particularly for the last of these
    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                RHS = quote(x[2]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3)))))

    
    rule <- makeGraphIndexRules(LHS = quote(y[2,3,i]),
                                RHS = quote(x[2]),
                                context = context_i)



test_that("graphRules works 2D with two sequence rules", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    ## Use a rule that includes a permutation and offsets
    rule2 <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                 RHS = quote(x[i + 2, j + 3]),
                                 context = context_ij)

    ## from 2 indexRange blocks
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(quote(5:7)),
                    indexRange(quote(4:6))
                ))
          , rule2)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(quote(1:3)),
               indexRange(quote(3:5))
           ))
    )

    ## from 2 indexRange blocks that run over ranges
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(quote(5:15)),
                    indexRange(quote(4:6))
                ))
          , rule2)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(quote(1:3)),
               indexRange(quote(3:10))
           ))
    )

    ## from 2 indexRange blocks with one that yields empty result
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(quote(13:15)),
                    indexRange(quote(4:6))
                ))
          , rule2)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(quote(1:3)),
               indexRange_matrix(matrix(data = numeric(), nrow = 0, ncol = 1))
           ))
    )

    ## from a matrix indexRange with 1 row
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(4, 7), nrow = 1))
                ))
          , rule2)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(c(4, 2), nrow = 1))
           ))
    )

    ## from a matrix indexRange with 1 row running over some boundaries
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(2, 7), nrow = 1)
                    )))
          , rule2)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange_matrix(
                   matrix(data = numeric(), nrow = 0, ncol = 2)
           )))
    )

    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(13, 7), nrow = 1)
                    )))
          , rule2)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange_matrix(
                    matrix(data = numeric(), nrow = 0, ncol = 2)
                )))
    )
    
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(5, 3), nrow = 1)
                    )))
          , rule2)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange_matrix(
                    matrix(data = numeric(), nrow = 0, ncol = 2)
                )))
    )
    
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(5, 9), nrow = 1)
                    )))
          , rule2)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange_matrix(
                    matrix(data = numeric(), nrow = 0, ncol = 2)
                )))
    )

    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(1, 9), nrow = 1)
                    )))
          , rule2)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange_matrix(
                    matrix(data = numeric(), nrow = 0, ncol = 2)
                )))
    )
    
    ## from a matrix indexRange with multiple rows    
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(4, 7, 6, 4), byrow = TRUE, nrow = 2)
                    )
                ))
          , rule2)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(c(4, 2, 1, 4), byrow = TRUE, nrow = 2)
               )
           ))
    )

    message('To do: Fill in testing of 2D sequence rule application to matrix with arbitrary elements not in the range.')
    
    ## from 2 single indexRanges (representing crossed indices)
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
               list(
                   indexRange(matrix(4)),
                   indexRange(matrix(7))
               ))
          , rule2)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(matrix(4)),
               indexRange(matrix(2))
           ))
   )
    
    ## from 2 separate arbitrary indexRanges
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(matrix(c(4,6), ncol = 1)),
                    indexRange(matrix(c(7,4), ncol = 1))
                ))
          , rule2)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange(matrix(c(4,1), ncol = 1)),
                indexRange(matrix(c(2,4), ncol = 1))
            ))
    )

    ## from 1 arbitrary and 1 block indexRanges
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(quote(4:6)),
                    indexRange(matrix(c(7,4), ncol = 1))
                ))
          , rule2)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange(matrix(c(4,1), ncol = 1)),
                indexRange(quote(2:4))
            ))
    )

    ## from 1 arbitrary and 1 block indexRanges
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(quote(4:6)),
                    indexRange(matrix(c(7), ncol = 1))
                ))
          , rule2)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange(matrix(c(4), ncol = 1)),
                indexRange(quote(2:4))
            ))
    )
})

test_that("graphRules works for 1D arbitrary rule", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    ## Single simple sequence rule
    k <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 1 and 2 are repeated
    rule1 <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[ k[i] ]),
                                 context = context_i,
                                 constants = list(k = k))

    ## Apply rule1 to a block indexRange
     expect_equal(
        test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(3:6) ## 3 and 4 yield nothing
                    )))
          , rule1)
      ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(bruteForceNestedIndexing(k, 3:6))
           )))
    )

    ## Apply rule1 to a block indexRange with multiples
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(1:6) ## 1 and 2 yield multiples, 3 and 4 yield nothing
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(bruteForceNestedIndexing(k, 1:6))
           )))
    )
    
    ## Apply rule1 to a matrix indexRange
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(2, 4, 6, 1), ncol = 1)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(bruteForceNestedIndexing(k, c(2, 4, 6, 1)))
           )))
    )

    ## Apply rule1 to a matrix indexRange with only 1 row
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(7), nrow = 1)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(bruteForceNestedIndexing(k, 7))
           )))
    )

    ## Apply rule1 to a matrix with an empty result
    options(error = recover)
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(4), nrow = 1)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(bruteForceNestedIndexing(k, 4))
           )))
    )
    
    ## Apply to a block with not all valid inputs
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(8:13)
                    )))
          , rule1)
       ,
       test2 <-  varRangeClass$new(
           list(
               indexRange(
                   matrix(bruteForceNestedIndexing(k, 8:13))
               )))
    )

    ## With no valid inputs
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(12:15)
                    )))
          , rule1)
       ,
       test2 <-  varRangeClass$new(
           list(
               indexRange_matrix(
                   matrix(data = numeric(), nrow = 0, ncol = 1)
               )))
    )
})

## Next cases:
## 2D input managed by 2 1D arbitrary rules
## 2D input managed by 1 1D arbitrary and 1 1D sequence rule
## 2D input managed by 1 2D arbitrary rules
## 3D cases

test_that("graphRuleClass works",
{
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 1:5){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
    
    context_ijk <- modelContextClass$new(list(singleContext1,
                                              singleContext2,
                                              singleContext3))

    rule <- graphRuleClass$new(LHS = quote(y[i, j]),
                               RHS = quote(x[i, j]),
                               context = context_ij)
    ## rule$apply needs to work with a varRange, not an indexRange
    debug(applyGraphIndexRules)
    rule$apply(indexRange(c(2, 4)))
}
)
