context("indexRuleBlock")


test_that("indexRuleBlock", {    

    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    ## Check that constant, all, and arbitrary cases are rejected.
    rule <- indexRuleBlockClass$new(list(3),
                                    list(),
                                    list())
    expect_identical(rule$setupResults, NULL)

    rule <- indexRuleBlockClass$new(list(quote(i+1)),
                                  list(2),
                                  context_i)
    expect_identical(rule$setupResults, NULL)

    rule <- indexRuleBlockClass$new(list(quote(3*i)),
                                  list(quote(i)),
                                  context_i)
    expect_identical(rule$setupResults, NULL)

    ## Standard block rule.
    rule <- indexRuleBlockClass$new(list(quote(i + 1)),
                                            list(quote(i + 3)),
                                            context_i)
    expect_identical(
        rule$setupResults,
        list(offset = -2, fromMin = 4, fromMax = 13)
    )

    ## Sequence input
    expect_equal(
        rule$apply(newIndexRange(quote(4:5))),
        newIndexRange(quote(2:3)))

    ## Partially or fully out of range
    expect_equal(
        rule$apply(newIndexRange(quote(12:14))),
        newIndexRange(quote(10:11))
    )

    expect_equal(
        rule$apply(newIndexRange(quote(2:5))),
        newIndexRange(quote(2:3))
    )

    expect_equal(
        rule$apply(newIndexRange(quote(2:4))),
        newIndexRange(2)
    )

    expect_equal(
        rule$apply(newIndexRange(quote(15:16))),
        indexRangeEmptyClass$new()
    )
    
    ## Scalar input
    expect_equal(
        rule$apply(newIndexRange(13)),
        newIndexRange(11)
    )

    expect_equal(
        rule$apply(newIndexRange(15)),
        indexRangeEmptyClass$new()
    )
    
    ## Matrix input
    expect_equal(
        rule$apply(newIndexRange(matrix(4))),
        newIndexRange(matrix(2))
    )

    expect_equal(
        rule$apply(newIndexRange(matrix(c(4,6,8), nrow = 3))),
        newIndexRange(matrix(c(2,4,6), nrow = 3))
    )

    ## with duplicates
    expect_equal(
        rule$apply(newIndexRange(matrix(c(4,4,8), nrow = 3))),
        newIndexRange(matrix(c(2,2,6), nrow = 3))
    )

    ## These produce NAs. They need to be kept at this stage
    ## so that if have multiple rules applied to a multi-column
    ## indexRange matrix, we can piece the results of the rules
    ## together element by element.
    expect_equal(
        rule$apply(newIndexRange(matrix(c(10,12,14), nrow = 3))),
        newIndexRange(matrix(c(8,10, NA), nrow = 3))
    )

    expect_equal(
        rule$apply(newIndexRange(matrix(c(14,16,18), nrow = 3))),
        newIndexRange(matrix(rep(as.numeric(NA), 3), nrow = 3))
    )

    ## getMax
    expect_identical(rule$getMax(), 13)
})

