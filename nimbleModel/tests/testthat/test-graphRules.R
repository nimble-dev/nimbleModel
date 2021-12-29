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
## (done) [all,block] y[j,i] <- x[i], x[i,2], x[i,2:3], x[i,], x[i,2,4]  
## (done) [constant,block] y[2,i] <- x[i], x[i,2], x[i,2:3], x[i,], x[i,2,3] 
## (done) [constant,all] y[2,i] <- x, x[2], x[2:3], x[], x[2,4], x[,2], x[2,3,4]
## (done) [constant,constant] y[2,2:3] <- x, x[2], x[2:3], x[], x[2,4], x[,2], x[2,3,4]
## (done) [block,block] y[i,j] <- x[i,j], x[i,j,2], x[i,j,]
## (done) [block,block,block] y[i,j,k] <- x[k,i,j], x[2,k,i,j], x[,k,i,j], x[2:3,x,i,j]  ## presumably sufficient for other reordering cases

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

## move tests that concentrate on validity of RHS constraints to separate testthat?

## set up matrix inputs (for all RHS indexes or subset) in various cases where constraints ARE satisfied
## to check full graphRules processing; note that LHS ranges may be matrices

## non-separable cases
## try matrix ranges that cover non-adjacent indexes or reversed indexes - what is possible?
## move matrix cases into separate test_that? right now in 1-d block and 1-d all and (I think) 2-d block

## error trapping if provide too many RHS indexes, including via matrix indexRange

## need to consider arbitrary indexes in the various cases above, e.g., y[i] <- x[k[i]]
## also y[k[i]] <- x[i] or y[k[i]] <- x[j[i]]
## and matrix cases like y[i,j] <- x[j[i],k[i]] (this is a matrix to matrix case, I think)

## look into arbitrary rules and arbitrary constraints, e.g., y[i] <- sum(x[c(1,3,5)])

## 1:n[i] type cases?

## multiple mapping cases
## y[i] <- x[i, i+1]
## y[i, i] <- x[i], x[2], etc.

irEmpty <- nimbleModel:::indexRange_empty()
vrEmpty <- varRangeClass$new(list(irEmpty))
vrEmpty2 <- varRangeClass$new(list(irEmpty, irEmpty))

test_that("graphRules works for 1D sequence rule", {
    ## y[i] from x[i], etc.

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    ## Single simple sequence rule
    rule <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)

    ## Are some of these not needed given testing in test-indexRule_block.R?
    
    ## block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)))),
            rule),
        varRangeClass$new(list(
                          indexRange(quote(2:5))))
    )
    
    ## with not all valid inputs
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(8:13)))),
            rule), 
        varRangeClass$new(list(
                          indexRange(quote(7:10))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:4)))),
            rule),
        varRangeClass$new(list(
                          indexRange(quote(1:3))))
    )

    ## With no valid inputs
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(12:15)))),
            rule),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0:1)))),
            rule),
        vrEmpty
    )

    ## Apply rule to a matrix indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2, 4), nrow = 2)))),
            rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(1, 3), nrow = 2))))
    )

    ## Apply rule to a matrix indexRange with only 1 row
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3), nrow = 1)))),
           rule),
       varRangeClass$new(list(
                         indexRange(matrix(c(2), nrow = 1))))
    )

    ## Apply to a matrix with no inputs in the right range
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(12, 13), nrow = 2)))),
            rule),
        vrEmpty
    )
    
    ## Apply to a matrix with one of two inputs in the right range
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                    indexRange(matrix(c(3, 12), nrow = 2)))),
            rule),
        varRangeClass$new(list(
                          indexRange(matrix(c(2), nrow = 1))))
    )

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

    ## alternative inputs: block, matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(5,3), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_error(  ## Not yet checking input variable bounds in blank index case
        expect_equal(
            applyGraphIndexRules(
                varRangeClass$new(list(
                           indexRange(matrix(c(33,55), nrow = 2)))), rule),      
            vrEmpty)
    )

    ## one valid input
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(25, 3), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))


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

    ## alternative inputs: block, matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(5,2), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(33,55), nrow = 2)))), rule),      
        vrEmpty)

    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2:3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    ## alternative inputs: block, matrix
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

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,2), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(5,2), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(33,55), nrow = 2)))), rule),      
        vrEmpty)

    
    rule <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2,3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))),
            rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(4)))),
            rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(4)))),
            rule),
        vrEmpty)

    ## alternative inputs: block, 2 1D matrix, 1 2D matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(1:3)))),
            rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(4:5)))),
            rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:5)))),
            rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)),
                              indexRange(matrix(3)))),
            rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)),
                              indexRange(matrix(4)))),
            rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(1)),
                              indexRange(matrix(4)))),
            rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,4), nrow = 2)),
                              indexRange(matrix(c(3,1), nrow = 2)))),
            rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

     expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4), nrow = 2)),
                              indexRange(matrix(c(1,5), nrow = 2)))),
            rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3), nrow = 1)))),
            rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                               indexRange(matrix(c(2, 4, 3, 1), nrow = 2)))),
            rule),
        varRangeClass$new(list(indexRange(quote(2:11)))))

     expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3, 4, 1, 5), nrow = 2)))),
            rule),
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

    ## not echecking block, matrix inputs; presumably redundant with previous tests

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

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,2), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(3)))), rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,5), nrow = 2)))), rule),
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

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(4)))), rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2)),
                                   indexRange(matrix(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2)),
                                   indexRange(matrix(4)))), rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3), nrow = 1)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,3), nrow = 1)))), rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,4), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,4), nrow = 2)))), rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,4), nrow = 2)))), rule),
        vrEmpty)
    
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

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2)),
                                   indexRange(matrix(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(3)),
                                   indexRange(matrix(3)))), rule),
        vrEmpty)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3), nrow = 1)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,3), nrow = 1)))), rule),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,4), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,4), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,3,4,4), nrow = 2)))), rule),
        vrEmpty)

    
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
                                 RHS = quote(x[2]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule)->tmp
    

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

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,4), nrow = 1)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,5), nrow = 1)))), rule),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), 
                              indexRange(matrix(c(3,4), nrow = 1)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(matrix(c(3,3), nrow = 1)))), rule),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(matrix(c(3,4), nrow = 1)))), rule),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)), 
                              indexRange(matrix(c(3,4), nrow = 1)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(matrix(c(3,3), nrow = 1)))), rule),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:4)),
                              indexRange(matrix(c(3,4), nrow = 1)))), rule),
        vrEmpty2
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
                                 RHS = quote(x[3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(irEmpty, irEmpty))
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
                                 RHS = quote(x[]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

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

test_that("graphRules works for 2D constant rule", {
    ## y[2,3] from x[], x[2], etc.
    
    context_0 <- modelContextClass$new()

    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2:3,3]),
                                 RHS = quote(x),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rule),
        varRangeClass$new(list(indexRange(quote(2:3)),
                               indexRange(quote(3))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[3]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[3]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[2:3]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )
    
    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[2,]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[2, 3, 4]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)),
                              indexRange(quote(4)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )
})

test_that("graphRules works for 2D block+constant rule", {

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))


    rule <- makeGraphIndexRules(LHS = quote(y[2,i+1]),
                                 RHS = quote(x[i]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(22:24)))), rule),
        varRangeClass$new(list(indexRange(quote(2)), irEmpty))
    )


    rule <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[i, 3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )
    
    rule <- makeGraphIndexRules(LHS = quote(y[2:4, i+1]),
                                 RHS = quote(x[i, 3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(2:4)),
                               indexRange(quote(2:4))))
    )
    
    rule <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[i, 3:5]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(5:6)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[i, ]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[3, i, ]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(1:3)),
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4,5), nrow = 1)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(matrix(5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,3,4,5,5,5), nrow = 2)))), rule),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(matrix(c(5,6),nrow=2))))
    )
})

test_that("graphRules works for 2D block+all rule", {

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
  
    rule <- makeGraphIndexRules(LHS = quote(y[j+2,i+1]),
                                 RHS = quote(x[i]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)))), rule),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[i, 3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )
    
    rule <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[i, 2:3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3:4)))), rule),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[i, ]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3:4)))), rule),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[3, i, ]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)), 
                              indexRange(quote(1:3)),
                              indexRange(quote(3:4)))), rule),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )
 
})

test_that("graphRules works for 2D with two sequence rules (block + block)", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    ## Use a rule that includes a permutation and offsets
    rule <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                RHS = quote(x[i + 2, j + 3]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(1:3)),
               indexRange(quote(3:5))))
    )

    ## from 2 indexRange blocks that run over ranges
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(5:15)),
                              indexRange(quote(4:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(1:3)),
               indexRange(quote(3:10))))
    )

    ## from 2 indexRange blocks with one that yields empty result
    ## This gives mixed result with one indexRange empty and one not.
    ## I think that makes sense where there is a direct from->to mapping,
    ## but later code will need to determine that this result is an empty
    ## overall varRange even though at least one indexRange is not empty.
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(13:15)),
                              indexRange(quote(4:6)))), rule),
        varRangeClass$new(list(
                          indexRange(quote(1:3)),
                          irEmpty))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                RHS = quote(x[3, i + 2, j + 3]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(1:3)),
               indexRange(quote(3:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(4),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rule),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(4)),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))),
            rule),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                RHS = quote(x[ , i + 2, j + 3]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(1:3)),
               indexRange(quote(3:5))))
    )

    ## HERE: need to work on these next ones: 1 2D and 1 1D, 1 3D
    
    ## from a matrix indexRange with 1 row
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(4, 7), nrow = 1)))), rule),
       varRangeClass$new(list(
                         indexRange(matrix(c(4, 2), nrow = 1))))
    )

    ## from a matrix indexRange with 1 row running over some boundaries
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(2, 7), nrow = 1)))), rule),
       varRangeClass$new(list(irEmpty), list(1:2))  # 1:2 is so have identical rangeID_2_indexID
    )

    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(1,9), nrow = 1)))), rule),
       varRangeClass$new(list(irEmpty), list(1:2))  
    )
    
    ## from a matrix indexRange with multiple rows    
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(4, 7, 6, 4), byrow = TRUE, nrow = 2)))), rule),
       varRangeClass$new(list(
                         indexRange(matrix(c(4, 2, 1, 4), byrow = TRUE, nrow = 2))))
    )

    ## from a matrix indexRange with multiple rows, one running over some boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(4, 7, 2, 7), byrow = TRUE, nrow = 2)))), rule),
       varRangeClass$new(list(
                         indexRange(matrix(c(4, 2), nrow = 1))))
    )

    ## from a matrix indexRange with multiple rows, both running over some boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(1, 7, 2, 7), byrow = TRUE, nrow = 2)))), rule),
       varRangeClass$new(list(irEmpty), list(1:2))
    )

    ## from a matrix indexRange with multiple rows, both running over all boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(1, 35, 2, 35), byrow = TRUE, nrow = 2)))), rule),
       varRangeClass$new(list(irEmpty), list(1:2))
    )
    
    message('To do: Fill in testing of 2D sequence rule application to matrix with arbitrary elements not in the range.')

    ## from 2 single indexRanges (representing crossed indices)
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(4)),
                             indexRange(matrix(7)))), rule),
       varRangeClass$new(list(
                             indexRange(matrix(4)),
                             indexRange(matrix(2))
                         ))
    )
    
    ## from 2 separate arbitrary indexRanges
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                    indexRange(matrix(c(4,6), ncol = 1)),
                    indexRange(matrix(c(7,4), ncol = 1)))), rule),
        varRangeClass$new(list(
                indexRange(matrix(c(4,1), ncol = 1)),
                indexRange(matrix(c(2,4), ncol = 1))
            ))
    )

    ## from 1 arbitrary and 1 block indexRanges
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                    indexRange(quote(4:6)),
                    indexRange(matrix(c(7,4), ncol = 1)))), rule),       
        varRangeClass$new(list(
                indexRange(matrix(c(4,1), ncol = 1)),
                indexRange(quote(2:4))
            ))
    )

    ## from 1 arbitrary and 1 block indexRanges
    expect_equal(applyGraphIndexRules(
            varRangeClass$new(list(
                    indexRange(quote(4:6)),
                    indexRange(matrix(c(7), ncol = 1)))), rule),       
        varRangeClass$new(list(
                indexRange(matrix(c(4), ncol = 1)),
                indexRange(quote(2:4))
            ))
    )
})


test_that("graphRules works for 3D with three sequence rules (block + block + block)", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 4:6){}))

    context_ijk <- modelContextClass$new(list(singleContext1,
                                              singleContext2,
                                              singleContext3))

## y[i,j,k] <-   x[,k,i,j], x[2:3,x,i,j] 
    
    rule <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(2:3)),
               indexRange(quote(4:6))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, 2, i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(2)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(2:3)),
               indexRange(quote(4:6))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(3:4)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rule),
        varRangeClass$new(list(irEmpty, irEmpty, irEmpty))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, 2:4, i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:6)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(2:3)),
               indexRange(quote(4:6))))
    )

    rule <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, , i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:6)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rule),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(2:3)),
               indexRange(quote(4:6))))
    )

})


## NOT YET LOOKED MORE AT THESE FROM HERE DOWN

## This is the beginning of these cases.
test_that("graphRules works for 2D single set cases", {

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    ## This is failing - need to look more at arbitrary indexRule.
    
    rule <- makeGraphIndexRules(LHS = quote(y[i+1,i]),
                                RHS = quote(x[2]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rule),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:10))))
    )
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rule),
        varRangeClass$new(list(irEmpty, irEmpty))
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
