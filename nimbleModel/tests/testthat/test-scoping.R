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
    expect_error(modelDef <- modelDefClass$new(code),
                 "not found as loop index or in `constants`")                 

    ## dynamic indexing case; not yet handled
    ## TODO: address dynamic indexing
    code <- quote({
        for(i in 1:3)
            y[i] ~ dnorm(mu[k[i]], 1)
    })
    k <- c(1,2,4)
    
    expect_message(modelDef <- modelDefClass$new(code, inits = list(k=k), dimensions = list(mu=5)),
                 "Detected use of non-constant indices")
    expect_message(modelDef <- modelDefClass$new(code, data = list(k=k), dimensions = list(mu=5)),
                 "Detected use of non-constant indices")

    code <- quote({
        for(i in 1:n)
            y[i] ~ dnorm(mu[i], 1)
    })

    n <- 3
    expect_error(modelDef <- modelDefClass$new(code),
                 "not found in `constants`")

    code <- quote({
        for(i in 1:3)
            for(j in 1:n[i])
                y[i,j] ~ dnorm(mu, 1)
    })
    
    n  <- c(1,3,2)
    expect_error(modelDef <- modelDefClass$new(code), "Is indexing information provided")

    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(mu, 1)
    })
    model <- nimbleModel(code)

    p <- 3
    expect_error(getNodes(model, 'y[1:p]'), "must involve two positive")
    expect_error(getDependencies(model$modelDef, 'y[1:p]'), "must involve two positive")
    
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
