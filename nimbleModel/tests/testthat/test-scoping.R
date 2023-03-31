## Tests to assess if we successfully isolate model-building related
## steps from R's global environment.

## We should always produce an error if a user tries to use an
## object from the global environment (instead of passed via
## constants) for indexing in model code or specifying model
## variables/nodes.

## FUTURE: We might expand this to include other aspects of NIMBLE.

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

    code <- quote({
        for(i in 1:3)
            for(j in 1:n[i])
                y[i,j] ~ dnorm(mu, 1)
    })
    
    n  <- c(1,3,2)
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    expect_error(modelDef$processDecls(), "is indexing information provided")

    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(mu, 1)
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$makeGraphInfo()

    p <- 3
    expect_error(getNodes(modelDef, 'y[1:p]'), "must involve two positive")
    expect_error(getDependencies(modelDef, 'y[1:p]'), "must involve two positive")
    
})
   
    
test_that("scoping when processing if-then-else", {

    code <- quote({
        y ~ dnorm(x, 1)
        if(useX)
            x ~ dnorm(0, 1)
    })

    expect_error(modelDef <- modelDefClass$new(code), "cannot evaluate condition")
 
    expect_silent(modelDef <- modelDefClass$new(code, constants = list(useX = TRUE)))

    useX <- TRUE
    assign('useX', useX, globalenv())
    expect_silent(modelDef <- modelDefClass$new(code))
})
