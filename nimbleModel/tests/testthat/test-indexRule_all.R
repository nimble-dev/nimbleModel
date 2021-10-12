context("indexRule_all")

test_that("indexRule_all works",
{    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    setupResults <- nimbleModel:::indexRule_all_setup(list(quote(i + 1)),
                                          list(),
                                          context_i)
    expect_equal(setupResults,
                 c(2, 11))

    thisRule <- nimbleModel:::indexRuleClass_all$new(list(quote(i + 1)),
                                       list(),
                                       context_i)
    expect_equal(
        thisRule$apply(NULL),
        indexRange_block(list(2, 11))
    )

    thisRule <- nimbleModel:::indexRuleClass_all$new(list(quote(i + 1)),
                                       list(2),
                                       context_i)
    expect_equal(
        thisRule$apply(2),
        indexRange_block(list(2, 11))
    )
    expect_equal(
        thisRule$apply(3),
        matrix(numeric(),0,1)
    )
    expect_equal(
        thisRule$apply(indexRange(quote(2:3))),
        indexRange_block(list(2, 11))
    )

    thisRule <- nimbleModel:::indexRuleClass_all$new(list(quote(i + 1)),
                                       list(2:3),
                                       context_i)
    expect_equal(
        thisRule$apply(2),
        indexRange_block(list(2, 11))
    )
    expect_equal(
        thisRule$apply(indexRange(quote(3:4))),
        indexRange_block(list(2, 11))
    )
    expect_equal(
        thisRule$apply(indexRange(quote(5:6))),
        matrix(numeric(),0,1)
    )
    expect_error(
        thisRule$apply(3:4))
}

## RHS no index
thisRule <- nimbleModel:::indexRuleClass_all$new(list(quote(i + 1)),
                                       NULL,
                                       context_i)
    expect_equal(
        thisRule$apply(NULL)
        indexRange_block(list(2, 11))
    )
## Note presumably further upstream we would trap if try to look for dependencies of `x[2]` if no RHS indexing

)
