context("indexRule_all")

test_that("indexRule_all works",
{    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))


    rule <- nimbleModel:::indexRuleClass_all$new(list(quote(i + 1)),
                                       list(),  # perhaps this would generally be NULL?
                                       context_i)
    expected_result <- indexRange(quote(2:11))
    expect_equal(rule$setupResults,
                 list(all = expected_result))

    ## Should non-empty RHS produce NULL or an "empty" rule?
    rule_alt <- nimbleModel:::indexRuleClass_all$new(list(quote(i + 1)),
                                                     list(2),
                                                     context_i)
    expect_identical(rule_alt$setupResults, NULL)
    
    expect_identical(
        rule$apply(NULL),
        expected_result
    )

    expect_identical(
        rule$apply(indexRange(quote(4:5))),
        expected_result
    )
}

