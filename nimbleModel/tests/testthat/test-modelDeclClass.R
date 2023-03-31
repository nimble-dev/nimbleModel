context("modelDeclClass")

test_that("modelDeclClass works", {
    test1 <- modelDeclClass$new()
    ## get dnorm in getAllDistributionsInfo
    test1$setup(code = quote(a ~ dnorm(0, 1)),
                context = NULL,
                sourceLineNum = 2)
    test1$makeSymbolicParentNodes(constants = list(),
                                 nimFunNames = list())
    expect_identical(test1$symbolicParentNodes,
                     NULL)
    
    test1 <- modelDeclClass$new()
    ## get dnorm in getAllDistributionsInfo
    test1$setup(code = quote(a[i] ~ dnorm(b * const * mu[i+1], sigma)),
                context = modelContextClass$new(list(quote(for(i in 1:10){}))),
                sourceLineNum = 2)
    test1$makeSymbolicParentNodes(constants = list(const = 7),
                                 nimFunNames = list())
    expect_identical(test1$symbolicParentNodes,
                     list(quote(b),
                          quote(mu[i+1]),
                          quote(sigma)
                          )
                     )
})

test_that("makeSymbolicParentNodes works", {
    modelCode <- quote({
        for(i in 1:10) {
            a[i] ~ dnorm(mu[i]+thetaVal, tau)
            mu[i] ~ dnorm(mu0, theta)
        }
        tau ~ dunif(0, 1)
        theta <- thetaVal
    })
    constants <- list(thetaVal = 7)
    modelDef <- modelDefClass$new(modelCode, constants = constants)
    modelDef$processModelCode()
    modelDef$declInfo[[1]]$makeSymbolicParentNodes(constants, c('dnorm','dunif'))
    modelDef$declInfo[[2]]$makeSymbolicParentNodes(constants, c('dnorm','dunif'))
    modelDef$declInfo[[3]]$makeSymbolicParentNodes(constants, c('dnorm','dunif'))
    modelDef$declInfo[[4]]$makeSymbolicParentNodes(constants, c('dnorm','dunif'))
    expect_identical(
        modelDef$declInfo[[1]]$symbolicParentNodes,
        list(quote(mu[i]), quote(tau)))
    expect_identical(
        modelDef$declInfo[[2]]$symbolicParentNodes,
        list(quote(mu0), quote(theta)))
    expect_identical(
        modelDef$declInfo[[3]]$symbolicParentNodes, NULL)
    expect_identical(
        modelDef$declInfo[[4]]$symbolicParentNodes, NULL)
    
})
