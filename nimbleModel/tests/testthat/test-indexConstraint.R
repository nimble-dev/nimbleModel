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
    ## TODO: sequence, matrix1d, matrix
    ## checkIndexConstraints(var, list)

})
