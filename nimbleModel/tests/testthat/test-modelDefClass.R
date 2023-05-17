context("processModelCode")

test_that("processModelCode works in simplest case", {
    modelCode <- quote({
        a ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    expect_equal(
        modelDef$declInfo[[1]]$symbolicParentNodes,
        NULL
    )
})

test_that("processModelCode works", {
    modelCode <- quote({
        for(i in 1:10)
            logit(a[i]) ~ dnorm(mu[i], tau)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()

    expected <- modelDeclClass$new()
    expected$setup(
                 quote(logit(a[i]) ~ dnorm(mu[i], tau)),
                 modelContextClass$new(list(
                                           quote(for(i in (1):(10)){})
                                       )),
                 sourceLineNumber = 2)

    expect_equal(modelDef$declInfo[[1]], expected) 

    ## Multiple lines; and check declRules
    modelCode <- quote({
        for(i in 1:10) {
            a[i] ~ dnorm(mu[i], tau)
            mu[i] ~ dnorm(mu0, theta)
        }
        tau ~ dunif(0, 1)
        theta <- thetaVal
    })
    modelDef <- modelDefClass$new(modelCode, constants = list(thetaVal = 7))
    modelDef$processModelCode()
    modelDef$processDecls()

    expect_identical(
        modelDef$declInfo[[1]]$targetNodeExpr, quote(a[i]))
    expect_identical(
        modelDef$declInfo[[1]]$valueExpr, quote(dnorm(mu[i],tau)))
    expect_identical(
        modelDef$declInfo[[1]]$indexExpr, list(quote(i)))
    expect_identical(
        modelDef$declInfo[[1]]$stoch, TRUE)

    expect_identical(
        modelDef$declInfo[[4]]$targetNodeExpr, quote(theta))
    expect_identical(
        modelDef$declInfo[[4]]$valueExpr, quote(thetaVal))
    expect_identical(
        modelDef$declInfo[[4]]$indexExpr, NULL)
    expect_identical(
        modelDef$declInfo[[4]]$stoch, FALSE)
    
    expect_identical(
        modelDef$declInfo[[1]]$declRule$varName, "a")
    expect_identical(
        modelDef$declInfo[[1]]$declRule$stoch, TRUE)
    expect_identical(
        modelDef$declInfo[[4]]$declRule$varName, "theta")
    expect_identical(
        modelDef$declInfo[[4]]$declRule$stoch, FALSE)
    
})


test_that("makeRHSoriginalNodes works", {
    modelCode <- quote({
        theta[1:2] <- phi[1:2]
        for(i in 1:10)
            a[i] ~ dnorm(mu[i], tau)
        sigma <-  tau2
        for(j in 1:3)
            for(i in 1:2)
                y[i,j] ~ dnorm(mu0[i], 1)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    modelDef$processDecls()
    
    ## TODO: placeholder:
    nimFunNames <- list(as.name(':'), as.name('dmnorm'), as.name('dnorm'), as.name('dunif'), as.name('dwish'))
    
    modelDef$declInfo[[1]]$makeRules(constants = list(), nimFunNames)
    for(i in 1:4)
        modelDef$declInfo[[i]]$makeRHSoriginalRules(constants = list())
    expect_identical(length(modelDef$declInfo[[1]]$rhsOriginalRules), 1L)
    expect_identical(length(modelDef$declInfo[[2]]$rhsOriginalRules), 2L)
    expect_identical(length(modelDef$declInfo[[3]]$rhsOriginalRules), 1L)
    expect_identical(length(modelDef$declInfo[[4]]$rhsOriginalRules), 1L)     
})


test_that("makeRules works", {
    modelCode <- quote({
        for(i in 1:10)
            a[i] ~ dnorm(mu[i], tau)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()

    ## TODO: placeholder:
    nimFunNames <- list(as.name(':'), as.name('dmnorm'), as.name('dnorm'), as.name('dunif'), as.name('dwish'))
    
    modelDef$declInfo[[1]]$makeRules(constants = list(), nimFunNames)

    expect_identical(
        length(modelDef$declInfo[[1]]$downstreamRules), 2L)
    expect_is(modelDef$declInfo[[1]]$downstreamRules[[1]]$indexRules[[1]],
              'indexRuleBlockClass')
    expect_is(modelDef$declInfo[[1]]$downstreamRules[[2]]$indexRules[[1]],
              'indexRuleAllClass')

    ## FIXME: LHS transformations not currently being processed correctly.
    modelCode <- quote({
        for(i in 1:10)
            logit(a[i]) ~ dnorm(mu[i], tau)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$declInfo[[1]]$makeRules(constants = list(), nimFunNames)
    
    result <- modelDeclClass$new()
    result$setup(
               quote(logit(a[i]) ~ dnorm(mu[i], tau)),
               modelContextClass$new(list(
                                         quote(for(i in (1):(10)){})
                                     )),
               sourceLineNumber = 2)
    
    expect_equal(modelDef$declInfo[[1]], result)
})

test_that("processDecls works", {
    modelCode <- quote({
        for(i in 1:10)
            a[i] ~ dnorm(mu[i], tau)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    modelDef$processDecls()

    expect_identical(length(modelDef$declInfo[[1]]$downstreamRules), 2L)
    expect_identical(length(modelDef$declInfo[[1]]$rhsOriginalRules), 2L)
})

test_that("makeGraphInfo works", {
    modelCode <- quote({
        for(i in 1:10) {
            a[i] ~ dnorm(mu[i], tau)
            mu[i] ~ dnorm(mu0, 1)
        }
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$makeGraphInfo()

    expect_identical(sort(names(modelDef$calcRules)), c('a','mu'))
    expect_identical(sort(names(modelDef$downstreamRules)), c('mu','mu0','tau'))
    expect_identical(sort(names(modelDef$rhsOnlyRules)), c('mu0','tau'))
   
})

