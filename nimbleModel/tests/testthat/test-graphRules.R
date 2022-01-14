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
})

## This is not fully set up because graphRuleClass not fully set up.
test_that("graphRuleClass works", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
    
    rule <- graphRuleClass$new(LHS = quote(y[i, j]),
                               RHS = quote(x[i, j]),
                               context = context_ij)

    expect_error(
        rule$apply(indexRange(c(2, 4))),
        "needs to be a varRange"
    )
    expect_error(
        rule$apply(varRangeClass$new(indexRange(matrix(c(2, 4), nrow = 1)))),
        "list of indexRanges"
    )
    ## rule$apply(varRangeClass$new(list(indexRange(matrix(c(2, 4), nrow = 1)))))
    ## rule$apply(varRangeClass$new(list(indexRange(3), indexRange(4))))
})

## Test various child cases:

## (done) [seq] y[i] <- x[i], x[i,2], x[i,2:3], x[i,]
## (done) [all] y[i] <- x, x[2], x[2:3], x[], x[2,3], x[2,]; y[3*i] <- x[2]
## (done) [constant] y[2] or y[2:3] <- x, x[2], x[2:3], x[], x[2,3], x[2,], x[2,i]
## (done) [constant] y <- x, x[2], x[2:3], x[], x[2,3], x[2,]
## (done) [all,all] y[i,j] <- x, x[2], x[2:3], x[2,4], x[], x[,2], x[2,3,4]
## (done) [all,seq] y[j,i] <- x[i], x[i,2], x[i,2:3], x[i,], x[i,2,4]  
## (done) [constant,seq] y[2,i] <- x[i], x[i,2], x[i,2:3], x[i,], x[i,2,3] 
## (done) [constant,all] y[2,i] <- x, x[2], x[2:3], x[], x[2,4], x[,2], x[2,3,4]
## (done) [constant,constant] y[2,2:3] <- x, x[2], x[2:3], x[], x[2,4], x[,2], x[2,3,4]
## (done) [seq,seq] y[i,j] <- x[i,j], x[i,j,2], x[i,j,]
## (done) [seq,seq,seq] y[i,j,k] <- x[k,i,j], x[2,k,i,j], x[,k,i,j], x[2:3,x,i,j]  ## presumably sufficient for other reordering cases

## (done) [arbitrary] y[i] <- x[k[i]]
## (done) [arbitrary, arbitrary] y[i,j] <- x[k1[i],k2[i]] 
## (done) [arbitrary, seq] y[i,j] <- x[k[i], j] 
## (done) [{arbitrary, arbitrary}] y[i,j] <- x[k[i,j]]
## (done) [arbitrary (LHS)] y[k[i]]] <- x[i]

## multiple mapping cases
## (done) [all] y[i,i] <- x[2], y[i+j,i] <- x[2], y[i+j] <- x[2]

## y[i] <- x[i, i+1]  (Perry single set cases)
## y[i, i] <- x[i], x[2], etc.
## y[i+j] <- x[i] or x[i,j]
## y[i+j, i] <- x[i] or x[i,j]


## should we check y[k[i]] <- x[j[i]]?

## This is not allowed in current nimble: y[i] <- sum(x[c(1,3,5)])


## 1:n[i] type cases?

## test some cases where have scalar + multirow matrix input indexRanges
## or multiple matrix input indexRanges with different numbers of rows
## should do crossing since we correctly cross multiple block indexRanges (I think, but check that too)
## key thing is to check with arbitrary indexRules

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
## currently the 2d matrix validity check is in the 1-d seq testing for 2d RHS constants case

## try matrix ranges that cover non-adjacent indexes or reversed indexes - what is possible?

## clarify terminology of block and sequence; use sequence for indexRange and block for indexRule?

irEmpty <- nimbleModel:::indexRange_empty()
vrEmpty <- varRangeClass$new(list(irEmpty))
vrEmpty_mat2 <- varRangeClass$new(list(irEmpty), list(1:2))
vrEmpty2 <- varRangeClass$new(list(irEmpty, irEmpty))

test_that("error trap incorrect number of input indexes", {
    ## y[i] from x[i], etc.

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
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
        "incorrect number of input indexes"
    )

    ## incorrect length of input matrix indexRange
    expect_error(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4), nrow = 1)))), rules),
        "incorrect number of input indexes"
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[2, 3]),
                                 context = context_i)

    expect_error(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3))), rules), 
        "incorrect number of input indexes"
    )
})

test_that("graphRules works for 1D sequence rule", {
    ## y[i] from x[i], etc.

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    ## Single simple sequence rule
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)

    ## Are some of these not needed given testing in test-indexRule_block.R?
    
    ## block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)))),
            rules),
        varRangeClass$new(list(
                          indexRange(quote(2:5))))
    )
    
    ## with not all valid inputs
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(8:13)))),
            rules), 
        varRangeClass$new(list(
                          indexRange(quote(7:10))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:4)))),
            rules),
        varRangeClass$new(list(
                          indexRange(quote(1:3))))
    )

    ## With no valid inputs
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(12:15)))),
            rules),
        vrEmpty
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0:1)))),
            rules),
        vrEmpty
    )

    ## Apply rule to a matrix indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2, 4), nrow = 2)))),
            rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(1, 3), nrow = 2))))
    )

    ## Apply rule to a matrix indexRange with only 1 row
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3), nrow = 1)))),
           rules),
       varRangeClass$new(list(
                         indexRange(matrix(c(2), nrow = 1))))
    )

    ## Apply to a matrix with no inputs in the right range
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(12, 13), nrow = 2)))),
            rules),
        vrEmpty
    )
    
    ## Apply to a matrix with one of two inputs in the right range
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                    indexRange(matrix(c(3, 12), nrow = 2)))),
            rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(2), nrow = 1))))
    )

    ## Simple sequence rule with additional indexing cases
    
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[2, i+1]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )


    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(3:6)))), rules),
        vrEmpty)

    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[2:3, i+1]),
                                 context = context_i)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:4)),
                              indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(4:5)),
                              indexRange(quote(3:6)))), rules),
        vrEmpty)

    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x[, i+1]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(3:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2:5))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[3*i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)))),
            rules),
        varRangeClass$new(list(indexRange(matrix(seq(6, 15, by = 3), ncol = 1))))
    )

})

test_that("graphRules works for 1D all rule", {
    ## y[i] from x[], x[2], etc.
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    context_i <- modelContextClass$new(list(singleContext1))

    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_error(  ## Not yet checking input variable bounds in blank index case
        expect_equal(
            applyGraphIndexRules(
                varRangeClass$new(list(
                                  indexRange(quote(33)))), rules),
            vrEmpty)
    )

    ## alternative inputs: block, matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(5,3), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_error(  ## Not yet checking input variable bounds in blank index case
        expect_equal(
            applyGraphIndexRules(
                varRangeClass$new(list(
                           indexRange(matrix(c(33,55), nrow = 2)))), rules),      
            vrEmpty)
    )

    ## one valid input
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(25, 3), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))


    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        vrEmpty)

    ## alternative inputs: block, matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(5,2), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(33,55), nrow = 2)))), rules),      
        vrEmpty)

    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2:3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    ## alternative inputs: block, matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:4)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(5:6)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:4)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,2), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(5,2), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(33,55), nrow = 2)))), rules),      
        vrEmpty)

    
    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[2,3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))),
            rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(4)))),
            rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(4)))),
            rules),
        vrEmpty)

    ## alternative inputs: block, 2 1D matrix, 1 2D matrix
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(1:3)))),
            rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(4:5)))),
            rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:5)))),
            rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)),
                              indexRange(matrix(3)))),
            rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)),
                              indexRange(matrix(4)))),
            rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(1)),
                              indexRange(matrix(4)))),
            rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,4), nrow = 2)),
                              indexRange(matrix(c(3,1), nrow = 2)))),
            rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

     expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4), nrow = 2)),
                              indexRange(matrix(c(1,5), nrow = 2)))),
            rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3), nrow = 1)))),
            rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                               indexRange(matrix(c(2, 4, 3, 1), nrow = 2)))),
            rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

     expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3, 4, 1, 5), nrow = 2)))),
            rules),
        vrEmpty)
   

    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x[,3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), indexRange(quote(4)))), rules),
        vrEmpty)

    expect_error(  ## Not yet checking input variable bounds in blank index case
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(33)), indexRange(quote(3)))), rules),
        vrEmpty)
    )

    ## not echecking block, matrix inputs; presumably redundant with previous tests

    rules <- makeGraphIndexRules(LHS = quote(y[i+1]),
                                 RHS = quote(x),
                                 context = context_i)

    ## Not sure if indexRange(0) is how we want to handle `x`.
    ## We have NULL as part of constraint if blank indexing, so use of NULL here doesn't work.
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(0))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rules),
        vrEmpty)

    ## more complicated expression than simple index
    rules <- makeGraphIndexRules(LHS = quote(y[3*i]),
                                RHS = quote(x[2]),
                                context = context_i)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rules),
        varRangeClass$new(list(indexRange(matrix(seq(3, 30, by = 3), ncol = 1))))
    )

})


test_that("graphRules works for 1D constant rule", {
    ## y[2] from x[3], etc.

    context_0 <- modelContextClass$new()

    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3)))), rules),
        vrEmpty)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(3:4)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,2), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(3)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,5), nrow = 2)))), rules),
        vrEmpty)

    rules <- makeGraphIndexRules(LHS = quote(y[2:3]),
                                RHS = quote(x[2]),
                                context = context_0)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:3)))))

    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2:3]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2,3]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(4)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2)),
                                   indexRange(matrix(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2)),
                                   indexRange(matrix(4)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3), nrow = 1)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,3), nrow = 1)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,4), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,4), nrow = 2)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,4,4), nrow = 2)))), rules),
        vrEmpty)
    
    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))


    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x[2,]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(2)),
                                   indexRange(matrix(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(3)),
                                   indexRange(matrix(3)))), rules),
        vrEmpty)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3), nrow = 1)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,3), nrow = 1)))), rules),
        vrEmpty)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,2,3,4), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(2,3,4,4), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(matrix(c(3,3,4,4), nrow = 2)))), rules),
        vrEmpty)

    
    rules <- makeGraphIndexRules(LHS = quote(y[2]),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(0))), rules),
        varRangeClass$new(list(indexRange(quote(2)))))

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)))), rules),
        vrEmpty)
})

test_that("graphRules works for 1D constant rule, LHS no indexing", {
    ## y from x[2] etc.

    context_0 <- modelContextClass$new()

    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rules),
        varRangeClass$new(list(indexRange(quote(0)))))

    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2:3]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rules),
        varRangeClass$new(list(indexRange(quote(0)))))

    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2,3]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2), indexRange(3))), rules),
        varRangeClass$new(list(indexRange(quote(0)))))
    
    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[]),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(2))), rules),
        varRangeClass$new(list(indexRange(quote(0)))))

    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x[2,]),
                                context = context_0)
    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(quote(2)),
                                   indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(0)))))
    
    rules <- makeGraphIndexRules(LHS = quote(y),
                                RHS = quote(x),
                                context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(indexRange(0))), rules),
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

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules)->tmp
    

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2:3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2,3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[,3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,j]),
                                 RHS = quote(x[2,3,4]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)),
                              indexRange(quote(4)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,4), nrow = 1)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,5), nrow = 1)))), rules),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)), 
                              indexRange(matrix(c(3,4), nrow = 1)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(matrix(c(3,3), nrow = 1)))), rules),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(matrix(c(3,4), nrow = 1)))), rules),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)), 
                              indexRange(matrix(c(3,4), nrow = 1)))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(matrix(c(3,3), nrow = 1)))), rules),
        vrEmpty2
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:4)),
                              indexRange(matrix(c(3,4), nrow = 1)))), rules),
        vrEmpty2
    )
    
})

test_that("graphRules works for 2D all+constant rule", {
    ## y[i,2] from x[], x[2], etc.
    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))


    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[2:3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[2, 3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[, 2]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i,2]),
                                 RHS = quote(x[2, 3, 4]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)),
                              indexRange(quote(4)))), rules),
        varRangeClass$new(list(indexRange(quote(1:10)),
                               indexRange(quote(2))))
    )
})

test_that("graphRules works for 2D constant rule", {
    ## y[2,3] from x[], x[2], etc.
    
    context_0 <- modelContextClass$new()

    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2:3,3]),
                                 RHS = quote(x),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rules),
        varRangeClass$new(list(indexRange(quote(2:3)),
                               indexRange(quote(3))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[3]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[3]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[2:3]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[2,]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2,3]),
                                 RHS = quote(x[2, 3, 4]),
                                 context = context_0)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)),
                              indexRange(quote(3)),
                              indexRange(quote(4)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(3))))
    )
})

test_that("graphRules works for 2D seq+constant rule", {

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))


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

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(22:24)))), rules),
        varRangeClass$new(list(indexRange(quote(2)), irEmpty))
    )


    rules <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[i, 3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y[2:4, i+1]),
                                 RHS = quote(x[i, 3]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(2:4)),
                               indexRange(quote(2:4))))
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[i, 3:5]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(5:6)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[i, ]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[2, i+1]),
                                 RHS = quote(x[3, i, ]),
                                 context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)),
                              indexRange(quote(1:3)),
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(quote(2:4))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4,5), nrow = 1)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(matrix(5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,3,4,5,5,5), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(quote(2)),
                               indexRange(matrix(c(5,6),nrow=2))))
    )
})

test_that("graphRules works for 2D seq+all rule", {

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
  
    rules <- makeGraphIndexRules(LHS = quote(y[j+2,i+1]),
                                 RHS = quote(x[i]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)))), rules),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[i, 3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3)))), rules),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )
    
    rules <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[i, 2:3]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3:4)))), rules),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[i, ]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:3)),
                              indexRange(quote(3:4)))), rules),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[j+2, i+1]),
                                 RHS = quote(x[3, i, ]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3)), 
                              indexRange(quote(1:3)),
                              indexRange(quote(3:4)))), rules),
        varRangeClass$new(list(indexRange(quote(3:7)),
                               indexRange(quote(2:4))))
    )
 
})

test_that("graphRules works for 2D with seq+seq", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    ## Use a rule that includes a permutation and offsets
    rules <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                RHS = quote(x[i + 2, j + 3]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rules),
        varRangeClass$new(list(
               indexRange(quote(1:3)),
               indexRange(quote(3:5))))
    )

    ## from 2 indexRange blocks that run over ranges
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(5:15)),
                              indexRange(quote(4:6)))), rules),
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
                              indexRange(quote(4:6)))), rules),
        varRangeClass$new(list(
                          indexRange(quote(1:3)),
                          irEmpty))
    )

    ## from 1 arbitrary and 1 block indexRanges
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                    indexRange(quote(4:6)),
                    indexRange(matrix(c(7,4), ncol = 1)))), rules),       
        varRangeClass$new(list(
                indexRange(matrix(c(4,1), ncol = 1)),
                indexRange(quote(2:4))
            ))
    )

    ## from 1 arbitrary and 1 block indexRanges
    expect_equal(applyGraphIndexRules(
            varRangeClass$new(list(
                    indexRange(quote(4:6)),
                    indexRange(matrix(c(7), ncol = 1)))), rules),       
        varRangeClass$new(list(
                indexRange(matrix(c(4), ncol = 1)),
                indexRange(quote(2:4))
            ))
    )


    rules <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                RHS = quote(x[3, i + 2, j + 3]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rules),
        varRangeClass$new(list(
               indexRange(quote(1:3)),
               indexRange(quote(3:5))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(4),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rules),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(4)),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))),
            rules),
        varRangeClass$new(list(irEmpty, irEmpty))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                RHS = quote(x[ , i + 2, j + 3]),
                                context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3),
                              indexRange(quote(5:7)),
                              indexRange(quote(4:6)))), rules),
        varRangeClass$new(list(
               indexRange(quote(1:3)),
               indexRange(quote(3:5))))
    )

    ## from a 3D matrix indexRange with 1 row
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 4, 7), nrow = 1)))), rules),
       varRangeClass$new(list(
                         indexRange(matrix(c(4, 2), nrow = 1))))
    )

    ## from a matrix indexRange with 1 row running over some boundaries
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3, 2, 7), nrow = 1)))), rules),
        vrEmpty_mat2)
    
    ## from a matrix indexRange with multiple rows    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3, 4, 4, 6, 7, 4), nrow = 2)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(4, 1, 2, 4), nrow = 2))))
    )

    ## from a matrix indexRange with multiple rows, one running over some boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 4, 7, 4, 2, 7), byrow = TRUE, nrow = 2)))), rules),
       varRangeClass$new(list(
                         indexRange(matrix(c(4, 2), nrow = 1))))
    )

    ## from a matrix indexRange with multiple rows, both running over some boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 1, 7, 4, 2, 7), byrow = TRUE, nrow = 2)))), rules),
       vrEmpty_mat2
    )

    ## from a matrix indexRange with multiple rows, both running over all boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 1, 35, 4, 2, 35), byrow = TRUE, nrow = 2)))), rules),
       vrEmpty_mat2
    )

    
    ## from combo matrix and non-matrix indexRanges
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 4), nrow = 1)),
                             indexRange(7))), rules),
       varRangeClass$new(list(
                         indexRange(4), indexRange(matrix(2))))
    )

    ## from two matrix indexRanges (representing crossed indices) with 1 row running over some boundaries
    expect_equal(
        applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 2), nrow = 1)),
                             indexRange(7))), rules),
        varRangeClass$new(list(
                         indexRange(4), irEmpty))
    )

    ## from two matrix indexRanges (representing crossed indices) with multiple rows    
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3, 4, 4, 6), nrow = 2)),
                              indexRange(matrix(c(7, 4), nrow = 2)))), rules),
        varRangeClass$new(list(
                          indexRange(matrix(c(4, 1), nrow = 2)),
                          indexRange(matrix(c(2, 4), nrow = 2))))
    )

    ## from two matrix indexRanges (representing crossed indices) with multiple rows, one running over some boundaries
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 4, 4, 2), byrow = TRUE, nrow = 2)),
                             indexRange(matrix(c(7, 7), nrow = 2)))), rules),
       varRangeClass$new(list(
                         indexRange(matrix(c(4, 4), nrow = 2)),
                         indexRange(matrix(2))))
    )

    ## from two matrix indexRanges (representing crossed indices) with multiple rows, both running over some boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 1, 4, 2), byrow = TRUE, nrow = 2)),
                             indexRange(matrix(c(7, 7), nrow = 2)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(4, 4), nrow = 2)),
                              irEmpty))
    )

    ## from two matrix indexRanges (representing crossed indices) with multiple rows, both running over all boundaries   
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                             indexRange(matrix(c(3, 1, 4, 2), byrow = TRUE, nrow = 2)),
                             indexRange(matrix(c(35, 35))))), rules),
       vrEmpty2
    )

})


test_that("graphRules works for 3D with seq+seq+seq", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 4:6){}))

    context_ijk <- modelContextClass$new(list(singleContext1,
                                              singleContext2,
                                              singleContext3))

## y[i,j,k] <-   x[,k,i,j], x[2:3,k,i,j] 
    
    rules <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rules),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(2:3)),
               indexRange(quote(4:6))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, 2, i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(2)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rules),
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
                              indexRange(quote(5:6)))), rules),
        varRangeClass$new(list(irEmpty, irEmpty, irEmpty))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, 2:4, i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:6)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rules),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(2:3)),
               indexRange(quote(4:6))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i, j, k]),
                                RHS = quote(x[k - 1, , i + 2, j + 3]),
                                context = context_ijk)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(4:6)),
                              indexRange(quote(4:6)),
                              indexRange(quote(5:6)))), rules),
        varRangeClass$new(list(
               indexRange(quote(2:4)),
               indexRange(quote(2:3)),
               indexRange(quote(4:6))))
    )

})


test_that("graphRules works for 1D arbitrary rule", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    ## Single simple sequence rule
    k <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 1 and 2 are repeated
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[ k[i] ]),
                                 context = context_i,
                                 constants = list(k = k))

    ## Apply rule to a block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)))), rules), ## 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(matrix(bruteForceNestedIndexing(k, 3:6)))))
    )

    ## Apply rule to a block indexRange with multiples
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:6)))), rules), ## 1 and 2 yield multiples, 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(matrix(bruteForceNestedIndexing(k, 1:6)))))
    )

    
    ## Apply rule to a matrix indexRange
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(matrix(c(2, 4, 6, 1), ncol = 1)))), rules),
       varRangeClass$new(list(
               indexRange(matrix(bruteForceNestedIndexing(k, c(2, 4, 6, 1))))))
    )

    ## Apply rule1 to a matrix indexRange with only 1 row
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(matrix(c(7), nrow = 1)))), rules),
       varRangeClass$new(list(
               indexRange(matrix(bruteForceNestedIndexing(k, 7)))))
    )

    ## Apply rule to a matrix with an empty result
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(matrix(c(4), nrow = 1)))), rules),
       vrEmpty
    )
    
    ## Apply to a block with not all valid inputs
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(quote(8:13)))), rules),
       varRangeClass$new(list(
               indexRange(matrix(bruteForceNestedIndexing(k, 8:13)))))
    )

    ## With no valid inputs
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(quote(12:15)))), rules),
       vrEmpty
    )
})

test_that("graphRules works for 2D arbitrary+seq rule", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    ## Single simple sequence rule
    k <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 1 and 2 are repeated
    rules <- makeGraphIndexRules(LHS = quote(y[j, i]),
                                 RHS = quote(x[ k[i] , j]),
                                 context = context_ij,
                                 constants = list(k = k))

    ## Apply rule to a block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)),
                              indexRange(quote(2:3)))), rules), ## 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(quote(2:3)),
                          indexRange(matrix(bruteForceNestedIndexing(k, 3:6)))))
    )
})



test_that("graphRules works for 2D arbitrary+arbitrary rule", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    ## Single simple sequence rule
    k1 <- c(1, 10, 2, 6, 1, 8, 2, 7, 5, 9) ## Note 1 and 2 are repeated
    k2 <- c(1, 10, 2, 6, 1) ## Note 1 and 2 are repeated
    rules <- makeGraphIndexRules(LHS = quote(y[j+2, i]),
                                 RHS = quote(x[ k1[i] , k2[j]]),
                                 context = context_ij,
                                 constants = list(k1 = k1, k2 = k2))

    ## Apply rule to a block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)),
                              indexRange(quote(1:3)))), rules), ## 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(matrix(bruteForceNestedIndexing(k2, 1:3)+2)),
                          indexRange(matrix(bruteForceNestedIndexing(k1, 3:6)))))
    )

    ## HERE need to add case of matrix indexRange input

    ## Apply rule where one index has no valid elements
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)),
                              indexRange(quote(3:4)))), rules), ## 3 and 4 yield nothing
        varRangeClass$new(list(
                          irEmpty,
                          indexRange(matrix(bruteForceNestedIndexing(k1, 3:6)))))
    )

})    

## 2D input managed by 1 2D arbitrary rules
test_that("graphRules works for 1 2D arbitrary rule", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:4){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:3){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    ## Single simple sequence rule
    k <- matrix(c(10, 1, 2,
                  2, 6, 2,
                  7, 5, 9,
                  7, 5, 5), nrow =4,  byrow = TRUE) # note some are repeated and others missing
    rules <- makeGraphIndexRules(LHS = quote(y[j+2, i]),
                                 RHS = quote(x[ k[i, j] ]),
                                 context = context_ij,
                                 constants = list(k = k))

    ## Apply rule to a block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:6)))), rules), ## 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(matrix(c(4,3,4,4,5,4,4,2), ncol = 2, byrow = TRUE))))
    )

    ## Apply rule where one index has no valid elements
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(3:4)))), rules), ## 3 and 4 yield nothing
        vrEmpty_mat2
    )
})   

test_that("graphRules works for 1D arbitrary rule where LHS is arbitrary", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    k <- c(10:2, 11)
    rules <- makeGraphIndexRules(LHS = quote(y[ k[i] ]),
                                 RHS = quote(x[i]),
                                 context = context_i,
                                 constants = list(k = k))

    ## Apply rule to a block indexRange
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(8:10)))), rules), ## 3 and 4 yield nothing
        varRangeClass$new(list(
                          indexRange(matrix(c(3,2,11), ncol = 1))))
    )

    ## Apply rule to a matrix indexRange
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(matrix(c(2, 4, 6, 1), ncol = 1)))), rules),
       varRangeClass$new(list(
               indexRange(matrix(c(9, 7, 5, 10), ncol = 1))))
    )

    ## Apply rule to a matrix with an empty result
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(matrix(c(11), nrow = 1)))), rules),
       vrEmpty
    )

    
    ## Apply to a block with not all valid inputs
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(quote(8:13)))), rules),
       varRangeClass$new(list(
                          indexRange(matrix(c(3,2,11), ncol = 1))))
    )

    ## With no valid inputs
    expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(
                    indexRange(quote(12:15)))), rules),
       vrEmpty
    )
    
})

test_that("graphRules works for 2D single set all cases", {

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,i]),
                                RHS = quote(x[2]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(cbind(2:11, 1:10))))
    )
                          
    rules <- makeGraphIndexRules(LHS = quote(y[i+1,i]),
                                RHS = quote(x[]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(cbind(2:11, 1:10))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,i]),
                                RHS = quote(x),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(0)))), rules),
        varRangeClass$new(list(indexRange(cbind(2:11, 1:10))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+1,i]),
                                RHS = quote(x[i]),
                                context = context_i)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(3, 2), nrow = 1))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(3, 4, 2, 3), nrow = 2))))
    )

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:2){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    rules <- makeGraphIndexRules(LHS = quote(y[j+3*i]),
                                 RHS = quote(x[2]),
                                 context = context_ij)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(4,5,7,8,10,11), ncol = 1))))
    )

    rules <- makeGraphIndexRules(LHS = quote(y[i+j, i]),
                                 RHS = quote(x[2]),
                                 context = context_ij)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(2,3,3,4,4,5,1,1,2,2,3,3), ncol = 2))))
    )

})

## NEED to work on these

## deal with all cases - need new code
## check that arbitrary cases work
test_that("graphRules works for 2D single set sequence cases", {

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    rules <- makeGraphIndexRules(LHS = quote(y[i+1, i]),
                                RHS = quote(x[i]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)))), rules),
        varRangeClass$new(list(indexRange(cbind(3:4, 2:3))))
    )


    ## HERE

    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:2){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    rules <- makeGraphIndexRules(LHS = quote(y[j+3*i]),
                                 RHS = quote(x[i, j]),
                                 context = context_ij)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,1,2), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(7, 11), ncol = 1))))
    )

    ## one valid, one invalid input
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(3,4,1,1), nrow = 2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(10))))
    )

    ## crossed 1D matrices
    ## ERROR
    ## varRange$rangeID_2_indexID is a list not a vector (above input is a vector)
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3), ncol = 1)),
                              indexRange(matrix(c(1,2), ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(7, 11), ncol = 1))))
    )

    ## ERROR: subscript out of bounds
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,1), ncol = 1)),
                              indexRange(matrix(c(3,2), ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(7, 11), ncol = 1))))
    )

    ## correct output but two columns
    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(quote(1:2)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(7, 11), ncol = 1))))
    )

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(2:3)),
                              indexRange(matrix(1:2, ncol = 1)))), rules),
        varRangeClass$new(list(indexRange(matrix(c(7, 11), ncol = 1))))
    )

         
    ## also add matrix indexRange input case to 2D arbitrary+arbitrary rule tests

    
    ## might be tricky in terms of constraint - actually constraint should be automatic
    ## via arbitrary rule in terms of RHS part of mapping, I think
    ## need to check various valid and invalid and mixed inputs
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                RHS = quote(x[i, i]),
                                context = context_i)

    expect_equal(
        applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(2),
                              indexRange(2))), rules),
        varRangeClass$new(list(indexRange(quote(2:11)),
                               indexRange(quote(1:10))))
    )



    rules <- makeGraphIndexRules(LHS = quote(y[i, j+2]),
                                RHS = quote(x[i + j]),
                                context = context_i)

## like y[k[i]] (need to make sure no multiple LHS definitions
## fails
    rules <- makeGraphIndexRules(LHS = quote(y[i+10*j]),
                                RHS = quote(x[i, j]),
                                context = context_ij)
  applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(2), indexRange(3))), rules)

## fails
rules <- makeGraphIndexRules(LHS = quote(y[i+j, j+2]),
                                RHS = quote(x[i, j]),
                                context = context_ij)
   applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(2), indexRange(3))), rules)




    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    ## this produces a matrix and uses arbitrary rule
    rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[1+i]),
                                 context = context_i)
  applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3))), rules)

## produces a scalar and uses sequence rule
   rules <- makeGraphIndexRules(LHS = quote(y[i]),
                                 RHS = quote(x[i+1]),
                                 context = context_i)

  applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(3))), rules)


### dealing with inner indexing

rules <- makeGraphIndexRules(LHS = quote(y[j+3*i]),
                                 RHS = quote(x[i, 3, j]),
                                 context = context_ij)

applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,3,3,1,2), nrow = 2)))), rules)
## 7,11

applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3), ncol = 1)),
                              indexRange(matrix(c(3,3,1,2), nrow = 2)))), rules)
## 7,10,8,11

applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3), ncol = 1)),
                              indexRange(matrix(c(3,3), ncol = 1)),
                             indexRange(matrix(c(1,2), ncol = 1)))), rules)

rules <- makeGraphIndexRules(LHS = quote(y[i,i]),
                                 RHS = quote(x[i,i]),
                                 context = context_i)
applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(4,4),nrow=1)))), rules)
applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(4), indexRange(4))), rules)


   singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:2){}))
    
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 1:3){}))
    
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
    
    context_ijk <- modelContextClass$new(list(singleContext1,
                                              singleContext2,
                                              singleContext3))
 
## complicated one given reordering and possibility that input indexRange used for
## multiple rules
rules <- makeGraphIndexRules(LHS = quote(y[j+3*i,j,k]),
                                 RHS = quote(x[i, k, j]),
                                 context = context_ijk)

rules <- makeGraphIndexRules(LHS = quote(y[j+3*i,j,k]),
                                 RHS = quote(x[i,j,k]),
                                 context = context_ijk)

applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3), ncol = 1)),
                              indexRange(matrix(c(1,3,1,2), nrow = 2)))), rules)

## these don't get crossed for first rule
applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,1,2,1,3), nrow = 2)))), rules)

## these get crossed for first rule, without k index
applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3), ncol = 1)),
                              indexRange(matrix(c(1,2), ncol = 1)),
                             indexRange(matrix(c(1,3), ncol = 1)))), rules)


rules <- makeGraphIndexRules(LHS = quote(y[j+3*i,k,j]),
                                 RHS = quote(x[i, k, j]),
                                 context = context_ijk)




rules <- makeGraphIndexRules(LHS = quote(y[j,i]),
                                 RHS = quote(x[i, j]),
                                 context = context_ij)
applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(matrix(c(2,3,1,2), nrow = 2)))), rules)


   singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
     singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:2){}))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

rules <- makeGraphIndexRules(LHS = quote(y[j+3*i]),
                                 RHS = quote(x[i, j]),
                                 context = context_ij)
applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:2)),
                              indexRange(quote(1:2)))), rules)

rules <- makeGraphIndexRules(LHS = quote(y[j,i+1]),
                                 RHS = quote(x[i, j]),
                                 context = context_ij)
applyGraphIndexRules(
            varRangeClass$new(list(
                              indexRange(quote(1:2)),
                              indexRange(quote(1:2)))), rules)

## very complicated multi-crossing case
   singleContext1 <-
        modelSingleContext(forCode = quote(for(l in 1:10){}))
    
     singleContext2 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
   singleContext3 <-
        modelSingleContext(forCode = quote(for(j in 1:10){}))
    
     singleContext4 <-
        modelSingleContext(forCode = quote(for(k in 1:10){}))
   singleContext5 <-
        modelSingleContext(forCode = quote(for(m in 1:10){}))
    
   
    context_lijkm <- modelContextClass$new(list(singleContext1,
                                             singleContext2, singleContext3, singleContext4, singleContext5))

rules <- makeGraphIndexRules(LHS = quote(y[i+3*k, l, i+1, j+3*m]),
                                 RHS = quote(x[l,i,j,k,m]),
                             context = context_lijkm)
applyGraphIndexRules(
    varRangeClass$new(list(
                      indexRange(matrix(c(1,3,2,4), nrow = 2)),
                      indexRange(matrix(c(3,5,1,7,4,9), nrow = 3)),
                      indexRange(matrix(c(9,11), nrow = 2)))), rules)



## DONE

test_that("graphRules works for non-contiguous indexes in an indexRange", {
    ## Case of y[i,j,k] where i,k are in an indexRange together
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:2){}))
    
    singleContext3 <-
        modelSingleContext(forCode = quote(for(k in 1:3){}))
    
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
    
    context_ijk <- modelContextClass$new(list(singleContext1,
                                              singleContext2,
                                              singleContext3))

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
)

