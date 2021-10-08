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
}
)
