context("graphRules")

bruteForceNestedIndexing <- function(indexVector, indexQuery) {
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
    
    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, 1:3]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, ]),
                                            context_ij)$indexVarNameSets,
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

test_that("graphRules works for 1D sequence rule", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    ## Single simple sequence rule
    rule1 <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)

    ## Apply rule1 to a block indexRange
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        quote(3:6)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   quote(2:5)
           )))
    )

    ## Apply rule1 to a matrix indexRange
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(2, 4), nrow = 2)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(c(1, 3), nrow = 2)
           )))
    )

    ## Apply rule1 to a matrix indexRange with only 1 row
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(3), nrow = 1)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(c(2), nrow = 1)
           )))
    )

    ## Apply rule1 to a scalar indexRange.
    ## I AM NOT SURE WHAT BEHAVIOR WE WANT HERE.
    ## CURRENTLY, THE RESULT OF APPLYING RULE TO A SCALAR IS A MATRIX
    message('not sure what behavior is wanted for applying a sequence rule to a scalar')
    ## debugonce(applyGraphIndexRules)
    ## expect_equal(
    ##    test1 <- applyGraphIndexRules(
    ##        varRangeClass$new(
    ##             list(
    ##                 indexRange(
    ##                     3
    ##                 )))
    ##       , rule1)
    ##    ,
    ##    test2 <- varRangeClass$new(
    ##        list(
    ##            indexRange(
    ##                matrix(2, nrow = 1)
    ##        )))
    ## )

    ## Apply to a matrix with one of two inputs in the right range
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(3, 12), nrow = 2)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(
                   matrix(c(2), nrow = 1)
           )))
    )

    ## Apply to a matrix with no inputs in the right range
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(12, 13), nrow = 2)
                    )))
          , rule1)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange_matrix(
                   matrix(nrow = 0, ncol = 1)
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
          , rule1)
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
          , rule1)
       ,
       test2 <-  varRangeClass$new(
           list(
               indexRange_matrix(
                   matrix(nrow = 0, ncol = 1)
               )))
    )

    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        quote(0:1)
                    )))
          , rule1)
       ,
       test2 <-  varRangeClass$new(
           list(
               indexRange_matrix(
                   matrix(nrow = 0, ncol = 1)
               )))
    )
})

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
