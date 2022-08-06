test_that("setting top nodes works", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    context_i <- modelContextClass$new(list(singleContext1))

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(0,1)), 1, context_i)
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), TRUE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(mu, sigma)), 1, context_i, constants = list(mu = 0, sigma = 1))
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), TRUE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(c[i],1)), 1, context_i, constants = list(c = rnorm(7)))
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), TRUE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(mu, sigma)), 1, context_i, constants = list(mu = 0))
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), FALSE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(c[i],1)), 1, context_i)
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), FALSE)

    ## TODO: add some more complicated cases
})

## test case:

## y[1:10] ~ dmnorm(mu[1:10],sigma[1:10])
## mu[2:4] ~ dmnorm()
## for(i in 4:7)
##     sigma[i] ~ dgamma(1,1)

## need various module complicated cases
