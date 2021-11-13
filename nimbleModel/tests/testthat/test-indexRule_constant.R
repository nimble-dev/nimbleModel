context("indexRule_constant")

test_that("indexRule_constant works",
{    
    rule <- nimbleModel:::indexRuleClass_constant$new(list(2),
                                       list(),  # perhaps this would generally be NULL?
                                       list())
    expected_result <- indexRange(quote(2))
    expect_equal(rule$setupResults,
                 list(constant = expected_result))

    ## Should non-empty RHS produce NULL or an "empty" rule?
    rule_alt <- nimbleModel:::indexRuleClass_all$new(list(2),
                                                     list(3),
                                                     list())
    expect_identical(rule_alt$setupResults, NULL)
    
    expect_identical(
        rule$apply(NULL),
        expected_result
    )

    expect_identical(
        rule$apply(indexRange(quote(4:5))),
        expected_result
    )

    ## Block constant
    rule <- nimbleModel:::indexRuleClass_constant$new(list(quote(2:3)),
                                       list(),  # perhaps this would generally be NULL?
                                       list())

    expected_result <- indexRange(quote(2:3))
    expect_equal(rule$setupResults,
                 list(constant = expected_result))
    
    expect_identical(
        rule$apply(NULL),
        expected_result
    )
}

