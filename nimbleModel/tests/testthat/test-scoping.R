## Tests to assess if we successfully isolate model-building related
## steps from R's global environment.

## FUTURE: We migth expand this to include other aspects of NIMBLE.

context("scoping")


test_that("avoid using indexing values from global", {

    code <- quote({
        for(i in 1:3)
        y[k[i]] ~ dnorm(mu[i], 1)
    })

    k <- c(1,2,4)
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    expect_error(modelDef$processDecls(),
                 "not found as loop index or in constants")

    ## dynamic indexing case; not yet handled
    ## TODO: address dynamic indexing
    code <- quote({
        for(i in 1:3)
            y[i] ~ dnorm(mu[k[i]], 1)
    })
    k <- c(1,2,4)
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    expect_error(modelDef$processDecls()))

    code <- quote({
        for(i in 1:n)
        y[i] ~ dnorm(mu[i], 1)
    })

    n <- 3
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    expect_error(modelDef$processDecls(), "not found")

    ## HERE - error seqNoDecr
    ## how get that into env? - use nimbleName space instead of baseenv?
    code <- quote({
        for(i in 1:3)
            for(j in 1:n[i])
                y[i,j] ~ dnorm(mu, 1)
    })
    
    n  <- c(1,3,2)
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    expect_error(modelDef$processDecls(), "not found")


    
})
   
    
