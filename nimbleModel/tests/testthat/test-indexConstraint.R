## TODO: check this:

## before refactoring rule$indexRules[[2]] was an indexRuleClass_constant
## now it is some how a seq indexRange
rule <- makeGraphRule(LHS = quote(x[2:5]),
                                RHS = quote(y[i]),
                                context = context_i_short)

context("indexConstraint")


test_that("indexConstraint", {
    constraint <- nimbleModel:::newIndexConstraint_fromSimple(3, 1, list())
    
    expect_equal(constraint, indexConstraintScalarClass$new(3, 1))
    expect_true(constraint$check(newIndexRange(3)))
    expect_false(constraint$check(newIndexRange(2)))
    expect_true(constraint$check(newIndexRange(quote(2:4))))
    expect_false(constraint$check(newIndexRange(quote(4:6))))
    expect_identical(
        constraint$check(newIndexRange(matrix(c(3,4)))),
        c(TRUE, FALSE)
    )
    expect_identical(
        constraint$check(newIndexRange(matrix(c(6,4)))),
        c(FALSE, FALSE)
    )
    
    constraint <- nimbleModel:::newIndexConstraint_fromSimple(quote(2:4), 1, list())
    expect_equal(constraint, indexConstraintSequenceClass$new(2, 4, 1))
    expect_true(constraint$check(newIndexRange(3)))
    expect_false(constraint$check(newIndexRange(5)))
    expect_true(constraint$check(newIndexRange(quote(3:5))))
    expect_false(constraint$check(newIndexRange(quote(6:7))))
    expect_identical(
        constraint$check(newIndexRange(matrix(c(3,5)))),
        c(TRUE, FALSE)
    )
    expect_identical(
        constraint$check(newIndexRange(matrix(c(6,1)))),
        c(FALSE, FALSE)
    )

    constraint <- nimbleModel:::newIndexConstraint_fromSimple(quote(c(2,4)), 1, list())
    expect_equal(constraint, indexConstraintMatrix1dClass$new(c(2,4), 1))
    expect_true(constraint$check(newIndexRange(4)))
    expect_false(constraint$check(newIndexRange(5)))
    expect_true(constraint$check(newIndexRange(quote(3:5))))
    expect_false(constraint$check(newIndexRange(quote(6:7))))
    expect_identical(
        constraint$check(newIndexRange(matrix(c(2,5)))),
        c(TRUE, FALSE)
    )
    expect_identical(
        constraint$check(newIndexRange(matrix(c(6,1)))),
        c(FALSE, FALSE)
    )
    
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:3){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    constraint <- nimbleModel:::newIndexConstraint_fromUnrolling(list(f1 = quote(k[i])), 1,
                                                                 context_i,
                                                                 constants = list(k = c(2,4,7)))
    expect_equal(constraint, indexConstraintMatrix1dClass$new(c(2,4,7), 1))
    expect_true(constraint$check(newIndexRange(4)))
    expect_false(constraint$check(newIndexRange(5)))
    expect_true(constraint$check(newIndexRange(quote(3:5))))
    expect_false(constraint$check(newIndexRange(quote(8:9))))
    expect_identical(
        constraint$check(newIndexRange(matrix(c(2,5)))),
        c(TRUE, FALSE)
    )
    expect_identical(
        constraint$check(newIndexRange(matrix(c(6,1)))),
        c(FALSE, FALSE)
    )

    constraint <- nimbleModel:::newIndexConstraint_fromUnrolling(list(f1 = quote(k[i]), f2 = quote(3*i)), c(1,2),
                                                                 context_i,
                                                                 constants = list(k = c(2,4,7)))
    expect_equal(constraint, indexConstraintMatrixClass$new(matrix(c(2,4,7,3,6,9), ncol = 2), c(1,2)))
    expect_true(constraint$check(newIndexRange(matrix(c(4,6), nrow = 1))))
    expect_false(constraint$check(newIndexRange(matrix(c(4,7), nrow = 1))))
    expect_identical(
        constraint$check(newIndexRange(matrix(c(4,6,4,7), nrow = 2, byrow = TRUE))),
        c(TRUE, FALSE)
    )
    expect_identical(
        constraint$check(newIndexRange(matrix(c(6,1,3,9), nrow = 2, byrow = TRUE))),
        c(FALSE, FALSE)
    )
})

test_that("checkIndexConstraints", {
    
    constraints <- list(
        indexConstraintScalarClass$new(2, 1),
        indexConstraintSequenceClass$new(2, 4, 3)
    )

    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(1),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(FALSE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(2),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(TRUE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(quote(7:9)),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(FALSE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(TRUE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(matrix(c(3,2,4,3), nrow = 2)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(c(FALSE,TRUE),TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(
                          varRangeClass$new(list(newIndexRange(matrix(c(3,2,4,2,2,4,2,5,7),
                                                                      nrow = 3, byrow =  TRUE))),
                                                               varName = 'x'),
                                            constraints),
        list(c(FALSE,TRUE,FALSE))
    )
    
    constraints <- list(
        indexConstraintScalarClass$new(2, 1),
        indexConstraintMatrix1dClass$new(c(2,4), 3)
    )

    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(1),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(FALSE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(2),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(TRUE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(quote(7:9)),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(FALSE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(quote(1:3)),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(TRUE,NULL,TRUE)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(matrix(c(3,2,4,3), nrow = 2)),
                                                                   newIndexRange(quote(3:5))), varName = 'x'),
                                            constraints),
        list(c(FALSE,TRUE),TRUE)
    )
    ## Extract multiple columns from matrix range and apply constraints and combine.
    expect_identical(
        nimbleModel:::checkIndexConstraints(
                          varRangeClass$new(list(newIndexRange(matrix(c(3,2,4,2,2,4,2,5,7),
                                                                      nrow = 3, byrow =  TRUE))),
                                                               varName = 'x'),
                                            constraints),
        list(c(FALSE,TRUE,FALSE))
    )


    ##  x[2,k[i],,3*i]
    constraints <- list(
        indexConstraintScalarClass$new(2, 1),
        indexConstraintMatrixClass$new(matrix(c(2,4,5,3,6,9),ncol = 2), c(2,4))
    )


    expResult <- c(FALSE, TRUE, rep(FALSE, 9), TRUE)

    ## Two input ranges on the matrix constraint; ranges are crossed and result duplicated.
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(1),
                                                                   newIndexRange(3:5),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(6:9))), varName = 'x'),
                                            constraints),
        list(FALSE, expResult, NULL, expResult)
    )
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(2:4),
                                                                   newIndexRange(3:5),
                                                                   newIndexRange(quote(2:7)),
                                                                   newIndexRange(quote(6:9))), varName = 'x'),
                                            constraints),
        list(c(TRUE,FALSE,FALSE), expResult, NULL, expResult)
    )
    
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(matrix(c(2,3,3,2), ncol = 2)),
                                                                   newIndexRange(3:5),
                                                                   newIndexRange(quote(6:9))),
                                                              rangeToIndex <- list(c(1,3),2,4),
                                                              varName = 'x'),
                                            constraints),
        list(c(TRUE,FALSE), expResult, expResult)
    )
    ## Input multi-slot range covers two constraints, but only one of the slots in the matrix constraint.
    expect_error(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(matrix(c(2,9,2,4,3,5), ncol = 2, byrow = TRUE)),
                                                                   newIndexRange(2:7),
                                                                   newIndexRange(quote(6:9))),
                                                              varName = 'x'),
                                            constraints),
        "should have been fully crossed"
    )
    ## Input multi-slot range covers only one constraint (and an unconstrained slot).
    expResult <- c(FALSE,TRUE,rep(FALSE,6))
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(2),
                                                                   newIndexRange(matrix(c(2,9,4,2,5,5,6,7), ncol = 2, byrow = TRUE)),
                                                                   newIndexRange(quote(6:7))),
                                                              varName = 'x'),
                                            constraints),
        list(TRUE, expResult, expResult)
        
    )
    ## Input range covers all slots.
    expect_identical(
        nimbleModel:::checkIndexConstraints(varRangeClass$new(list(newIndexRange(
                                                              matrix(c(3,4,3,6, 2,4,3,6, 2,1,3,3, 2,2,3,3, 2,4,3,9), ncol = 4, byrow = TRUE))),
                                                              varName = 'x'),
                                            constraints),
        list(c(FALSE, TRUE, FALSE, TRUE, FALSE))
    )

})
