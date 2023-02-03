context("varRules")

test_that("varRules works for 2 1D sequence rules", {
    ## for(i in 1:10) y[i] <- foo(x[i])
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    context_i1 <- modelContextClass$new(list(singleContext1))
    rule1 <- graphRuleClass$new(LHS = quote(y[i]),
                                RHS = quote(x[i]),
                                context = context_i1)

    ## for(i in 6:15) z[i] <- foo(x[i])
    singleContext2 <-
        modelSingleContext(forCode = quote(for(i in 6:15){}))
    context_i2 <- modelContextClass$new(list(singleContext2))
    rule2 <- graphRuleClass$new(LHS = quote(z[i]),
                                RHS = quote(x[i]),
                                context = context_i2)

    varRule_x <- varRuleClass$new(
        list(rule1,
             rule2)
    )

    expect_equal(
        test1 <- varRule_x$apply(varRangeClass$new(
            list(
                indexRange(
                    quote(3:12)
                )), varName = 'x')
            ),
        test2 <- list(
            varRangeClass$new(
                list(
                    indexRange(
                        quote(3:10)
                    )), varName = 'y'),
            varRangeClass$new(
                list(
                    indexRange(
                        quote(6:12)
                    )), varName = 'z')
        )
    )
})
