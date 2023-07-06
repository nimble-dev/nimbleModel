context("varRulesClass")

test_that("varRules initialization and apply", {
    ## Simple case of applying two simple graphRules containing single block rules.
    
    ## Example declaration: for(i in 1:10) y[i] <- foo(x[i])
    singleContext1 <-
        singleContextClass$new(forCode = quote(for(i in 1:10){}))
    context_i1 <- modelContextClass$new(list(singleContext1))
    rule1 <- graphRuleClass$new(toExpr = quote(y[i]),
                                fromExpr = quote(x[i]),
                                context = context_i1)

    ## Example declaration: for(i in 6:15) z[i] <- foo(x[i])
    singleContext2 <-
        singleContextClass$new(forCode = quote(for(i in 6:15){}))
    context_i2 <- modelContextClass$new(list(singleContext2))
    rule2 <- graphRuleClass$new(toExpr = quote(z[i]),
                                fromExpr = quote(x[i]),
                                context = context_i2)

    varRules_x <- varRulesClass$new(
        list(rule1, rule2), varName = 'x'
    )

    expect_equal(
        result <- varRules_x$apply(varRangeClass$new(
            list(
                newIndexRange(
                    quote(3:12)
                )), varName = 'x')
            ),
        expected <- list(
            varRangeClass$new(
                list(
                    newIndexRange(
                        quote(3:10)
                    )), varName = 'y'),
            varRangeClass$new(
                list(
                    newIndexRange(
                        quote(6:12)
                    )), varName = 'z')
        )
    )
})
