context("indexRuleConstant")

test_that("indexRuleConstant", {

    ## Check that non-missing 'from' is rejected.
    rule <- indexRuleConstantClass$new(list(2),
                                       list(3),  
                                       list())
    expect_identical(rule$setupResults, NULL)

    ## Check that indexing is rejected.
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    rule <- indexRuleConstantClass$new(list(quote(i)),
                                       list(),  
                                       context_i)
    expect_identical(rule$setupResults, NULL)


    rule <- indexRuleConstantClass$new(list(2),
                                       list(),  
                                       list())
    
    expected_result <- newIndexRange(quote(2))
    expect_equal(rule$setupResults,
                 list(constant = expected_result))

    expect_equal(
        rule$apply(NULL),
        expected_result
    )

    expect_equal(
        rule$apply(newIndexRange(quote(4:5))),
        expected_result
    )

    ## Block constant
    rule <- indexRuleConstantClass$new(list(quote(2:3)),
                                       list(), 
                                       list())

    expected_result <- newIndexRange(quote(2:3))
    expect_equal(rule$setupResults,
                 list(constant = expected_result))
    
    expect_equal(
        rule$apply(NULL),
        expected_result
    )    
})

