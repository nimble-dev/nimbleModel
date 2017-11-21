context("graphRules")

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

test_that("graphRules works", {
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

    ## We can make a comprehensive test function like test_arbitraryIndexRule
    ## For now here are some quick tests.
    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                 RHS = quote(x[i, j]),
                                 context = context_ij)
    ## from a matrix indexRange with 1 row
    debug(applyGraphIndexRules)
    expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(2, 4), nrow = 1))
                ))
          , rules)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(quote(matrix(2))),
               indexRange(quote(matrix(4)))
           ))
    )

    ## from a matrix indexRange with multiple rows
    message('This one is not really correct. The results should be combined into a single result arbitrary index range')
    debug(applyGraphIndexRules)
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(
                        matrix(c(2, 3, 4, 5), byrow = TRUE, nrow = 2)
                    )
                ))
          , rules)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(c(2,4)),
               indexRange(c(3,5))
           ))
    )

    ## from 2 single indexRanges
   expect_equal(
       test1 <- applyGraphIndexRules(
           varRangeClass$new(
           list(
               indexRange(quote(matrix(2))),
               indexRange(quote(matrix(4)))
           ))
          , rules)
       ,
       test2 <- varRangeClass$new(
           list(
               indexRange(quote(matrix(2))),
               indexRange(quote(matrix(4)))
           ))
   )
    
    ## from 2 separate arbitrary indexRanges
    expect_equal(
        test1 <- applyGraphIndexRules(
            varRangeClass$new(
                list(
                    indexRange(c(2,4)),
                    indexRange(c(3,5))
                ))
          , rules)
       ,
        test2 <- varRangeClass$new(
            list(
                indexRange(c(2,4)),
                indexRange(c(3,5))
            ))
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                 RHS = quote(x[j, i]),
                                 context = context_ij)
    expect_equal(applyGraphIndexRules(c(2, 4), rules),
                 matrix(c(4, 2), nrow = 1))

    rules <- makeGraphIndexRules(LHS = quote(y[i, j]),
                                 RHS = quote(x[j, i]),
                                 context = context_ij)
    expect_equal(applyGraphIndexRules(c(2, 4), rules),
                 matrix(c(4, 2), nrow = 1))

    rules <- makeGraphIndexRules(LHS = quote(y[i+2, j]),
                                 RHS = quote(x[j+3, i]),
                                 context = context_ij)
    expect_equal(applyGraphIndexRules(c(5, 1), rules),
                 matrix(c(3, 2), nrow = 1))
    
})

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
