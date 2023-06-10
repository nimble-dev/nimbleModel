context("indexRuleAll")

test_that("indexRuleAll",
{    
    context_0 <- modelContextClass$new()
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))

    ## Check that any from expression is rejected.
    rule <- indexRuleAllClass$new(list(quote(i+1)),
                                  list(2),
                                  context_i)
    expect_identical(rule$setupResults, NULL)

    rule <- indexRuleAllClass$new(list(quote(i+1)),
                                  list(quote(i)),
                                  context_i)
    expect_identical(rule$setupResults, NULL)

    rule <- indexRuleAllClass$new(list(quote(2)),
                                  list(),
                                  context_0)
    expect_identical(rule$setupResults, NULL)
    
    ## Sequence case, `y[i+1]`.
    rule <- indexRuleAllClass$new(list(quote(i+1)),
                                       list(),  
                                       context_i)
    expected_result <- newIndexRange(quote(2:11))
    expect_equal(rule$setupResults,
                 list(all = expected_result))

    expect_equal(
        rule$apply(NULL),
        expected_result
    )

    expect_equal(
        rule$apply(newIndexRange(quote(4:5))),
        expected_result
    )

    ## Single-column case, `y[3*i]`.
    rule <- indexRuleAllClass$new(list(t1 = quote(3*i)),
                                       list(),  
                                       context_i)
    expected_result <- newIndexRange(matrix(seq(3, 30, by = 3), ncol = 1))
    expect_equal(rule$setupResults,
                 list(all = expected_result))

    ## Multi-column case, e.g, `y[i, i+1]`.
    rule <- indexRuleAllClass$new(list(t1 = quote(i), t2 = quote(i+1)),
                                  list(),  
                                  context_i)
    expected_result <- newIndexRange(matrix(c(1:10,2:11), ncol = 2))
    expect_equal(rule$setupResults,
                 list(all = expected_result))

})

