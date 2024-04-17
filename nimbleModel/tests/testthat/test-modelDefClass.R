context("processModelCode")

test_that("processModelCode works in simplest case", {
    modelCode <- quote({
        a ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(modelCode)
    expect_identical(length(modelDef$declInfo), 1L)
    expect_equal(
        modelDef$declInfo[[1]]$symbolicParentNodes,
        list()
    )
})

test_that("processModelCode works", {
    ## Multiple lines; and check declRules
    modelCode <- quote({
        for(i in 1:n) {
            a[i] ~ dnorm(mu[i], tau)
            mu[i] ~ dnorm(mu0, theta)
        }
        tau ~ dunif(0, 1)
        theta <- thetaVal
    })
    modelDef <- modelDefClass$new(modelCode, constants = list(thetaVal = 7, n = 10))

    expect_identical(
        modelDef$declInfo[[2]]$targetNodeExpr, quote(a[i]))
    expect_equal(
        modelDef$declInfo[[2]]$valueExpr,
        quote(dnorm(mean = mu[i], sd = lifted_d1_over_sqrt_oPtau_cP, lower_ = -Inf, 
    upper_ = Inf, .tau = tau, .var = lifted_d1_over_sqrt_oPtau_cP * 
        lifted_d1_over_sqrt_oPtau_cP)))
    expect_identical(
        modelDef$declInfo[[2]]$indexExpr, list(quote(i)))
    expect_identical(
        modelDef$declInfo[[2]]$stoch, TRUE)

    expect_identical(
        modelDef$declInfo[[6]]$targetNodeExpr, quote(theta))
    expect_identical(
        modelDef$declInfo[[6]]$valueExpr, 7)
    expect_identical(
        modelDef$declInfo[[6]]$indexExpr, NULL)
    expect_identical(
        modelDef$declInfo[[6]]$stoch, FALSE)
    
    expect_identical(
        modelDef$declInfo[[2]]$declRule$varName, "a")
    expect_identical(
        modelDef$declInfo[[2]]$declRule$stoch, TRUE)
    expect_identical(
        modelDef$declInfo[[6]]$declRule$varName, "theta")
    expect_identical(
        modelDef$declInfo[[6]]$declRule$stoch, FALSE)
    
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

    expect_identical(length(modelDef$declInfo[[1]]$rhsOriginalRules), 1L)
    expect_identical(length(modelDef$declInfo[[2]]$rhsOriginalRules), 1L)
    expect_identical(length(modelDef$declInfo[[3]]$rhsOriginalRules), 3L)
    expect_identical(length(modelDef$declInfo[[4]]$rhsOriginalRules), 1L)
    expect_identical(length(modelDef$declInfo[[5]]$rhsOriginalRules), 1L)

    expect_equal(modelDef$declInfo[[1]]$rhsOriginalRules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:2))),
                                        varName = 'phi'))
    expect_equal(modelDef$declInfo[[2]]$rhsOriginalRules[[1]]$fullRange,
                 varRangeClass$new(list(), varName = 'tau'))
    expect_equal(modelDef$declInfo[[3]]$rhsOriginalRules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:10))), varName = 'mu'))
    expect_equal(modelDef$declInfo[[3]]$rhsOriginalRules[[2]]$fullRange,
                 varRangeClass$new(list(), varName = 'lifted_d1_over_sqrt_oPtau_cP'))
    expect_equal(modelDef$declInfo[[3]]$rhsOriginalRules[[3]]$fullRange,
                 varRangeClass$new(list(), varName = 'tau'))
    expect_equal(modelDef$declInfo[[4]]$rhsOriginalRules[[1]]$fullRange,
                 varRangeClass$new(list(), varName = 'tau2'))
    expect_equal(modelDef$declInfo[[5]]$rhsOriginalRules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:2))), varName = 'mu0'))
      
})

test_that("makeGraphRules works", {
    modelCode <- quote({
        for(i in 1:10) {
            a[i] ~ dnorm(mu[i], sd = tau)
            mu[i] ~ dnorm(0, 1)
        }
    })
    modelDef <- modelDefClass$new(modelCode)

    expect_identical(
        length(modelDef$declInfo[[1]]$downstreamRules), 2L)
    expect_is(modelDef$declInfo[[1]]$downstreamRules[[1]]$indexRules[[1]],
              'indexRuleBlockClass')
    expect_is(modelDef$declInfo[[1]]$downstreamRules[[2]]$indexRules[[1]],
              'indexRuleAllClass')
    expect_identical(length(modelDef$declInfo[[1]]$rhsOriginalRules), 2L)

    expect_identical(
        length(modelDef$declInfo[[1]]$upstreamRules), 2L)
    expect_is(modelDef$declInfo[[1]]$upstreamRules[[1]]$indexRules[[1]],
              'indexRuleBlockClass')
    expect_identical(modelDef$declInfo[[1]]$upstreamRules[[2]]$indexRules,
              list())
    
    
    modelCode <- quote({
        for(i in 1:10)
            logit(a[i]) ~ dnorm(mu[i], tau)
    })
    modelDef <- modelDefClass$new(modelCode)
    
    expect_identical(length(modelDef$declInfo), 3L)
    expect_equal(
        modelDef$declInfo[[2]]$targetExpr,
        quote(logit_a[i]))
    expect_equal(
        modelDef$declInfo[[2]]$valueExpr,
        quote(dnorm(mean = mu[i], sd = lifted_d1_over_sqrt_oPtau_cP, 
                                 lower_ = -Inf, upper_ = Inf, .tau = tau,
                                 .var = lifted_d1_over_sqrt_oPtau_cP * lifted_d1_over_sqrt_oPtau_cP)))
    expect_equal(
        modelDef$declInfo[[3]]$valueExpr,
        quote(expit(logit_a[i])))
    expect_equal(
        modelDef$declInfo[[3]]$targetExpr,
        quote(a[i]))
})


test_that("makeGraphInfo works", {
    modelCode <- quote({
        for(i in 1:10) {
            a[i] ~ dnorm(mu[i], tau)
            mu[i] ~ dnorm(mu0, 1)
        }
    })
    modelDef <- modelDefClass$new(modelCode)

    expect_identical(sort(names(modelDef$calcRules)),
                     c('a','lifted_d1_over_sqrt_oPtau_cP','mu'))
    expect_identical(sort(names(modelDef$downstreamRules)),
                     c('lifted_d1_over_sqrt_oPtau_cP','mu','mu0','tau'))
    expect_identical(sort(names(modelDef$rhsOnlyRules)), c('mu0','tau'))
   
})

test_that("makeVarInfo works", {
    modelCode <- quote({
        for(i in 1:10) {
            y[i] ~ dnorm(mu[i], sd = sigma[k[i]])
            mu[i] ~ dnorm(mu0, 1)
            sigma[i] ~ dunif(0, 10)
            k[i] ~ dcat(p[1:10])
        }
        z[1:10] <- mu[1:10]
        z[12] ~ dnorm(0, 1)
        tau[1:10] <- 1/sigma[1:10]^2

        w[1:3] ~ dmnorm(zeroes[1:3], pr[1:3,1:3])
    })
    
    modelDef <- modelDefClass$new(modelCode)

    vi <- modelDef$varInfo
    
    expect_true(vi$mu$anyStoch)
    expect_false(vi$mu$anyDynamicallyIndexed)
    expect_true(vi$sigma$anyStoch)
    expect_true(vi$sigma$anyDynamicallyIndexed)
    expect_true(vi$z$anyStoch)
    expect_false(vi$z$anyDynamicallyIndexed)
    expect_false(vi$tau$anyStoch)
    expect_false(vi$tau$anyDynamicallyIndexed)

    expect_identical(vi$mu$mins, 1)
    expect_identical(vi$mu$maxs, 10)
    expect_identical(vi$z$mins, 1)
    expect_identical(vi$z$maxs, 12)
    expect_identical(vi$sigma$mins, 1)
    expect_identical(vi$sigma$maxs, as.numeric(.Machine$integer.max))
    expect_identical(vi$pr$mins, rep(1, 2))
    expect_identical(vi$pr$maxs, rep(3, 2))

    expect_identical(vi$mu0$nDim, 0)
    expect_identical(vi$mu$nDim, 1)
    expect_identical(vi$pr$nDim, 2)
})


test_that("handling dimensions if not specified", {
    modelCode <- quote({
        y <- sum(mu[])
    })

    expect_error(
        modelDef <- modelDefClass$new(modelCode),
        "which contains missing indices")
    
    modelDef <- modelDefClass$new(modelCode, inits = list(mu = rnorm(5)))
    expect_identical(modelDef$varInfo$mu$maxs, 5)

    expect_identical(body(modelDef$declRules[['y']]$rules[[1]]$calculate),
                 quote(y <<- sum(mu[1:5])))
        
    modelDef <- modelDefClass$new(modelCode, dimensions = list(mu = 5))
    expect_identical(modelDef$varInfo$mu$maxs, 5)

    expect_message(
        modelDef <- modelDefClass$new(modelCode, dimensions = list(mu = 3),
                                      inits = list(mu = rnorm(5))),
        "Inconsistent dimensions")

    expect_error(
        modelDef <- modelDefClass$new(modelCode, dimensions = list(mu = c(5,2))),
        "inconsistent dimensionality")

})    

test_that("truncation processing", {
    modelCode <- quote({
        y ~ T(dnorm(mu, sd = sigma), 3, b)
    })
    modelDef <- modelDefClass$new(modelCode)
    
    expect_identical(
        modelDef$declInfo[[1]]$symbolicParentNodes,
        list(quote(mu),quote(sigma),quote(b)))

    expect_identical(
        body(modelDef$declRules[['y']]$rules[[1]]$calculate),
        quote(logProb_y <<- dnorm(y, mean = mu, sd = sigma, lower_ = 3, upper_ = b, 
                                  log = 1)))
})

test_that("detection of duplicated declarations", {
    code <- quote({
        z ~ dbin(p, 1)
        z ~ dbin(p, c)
    })
    expect_error(m <- modelClass$new(code), "There are multiple definitions")
    
    code <- quote({
        z ~ dbin(p, 1)
        z[1:2] <- tmp[1:2]
    })
    expect_error(m <- modelClass$new(code), "Inconsistent dimensions")
    
    code <- quote({
        z[1:2] <- tmp[3:4]
        z[1:2] <- tmp[1:2]
    })
    expect_error(m <- modelClass$new(code), "overlaps")

    code <- quote({
        z[1:2] <- tmp[3:4]
        z[2:3] <- tmp[1:2]
    })
    expect_error(m <- modelClass$new(code), "overlaps")
    
    code <- quote({
        z[1:2,1:2] <- tmp[3:4,1:2]
        z[2:3,1:2] <- tmp[1:2,1:2]
    })
    expect_error(m <- modelClass$new(code), "overlaps")
    
    code <- quote({
        z[1:2,1:2] <- tmp[3:4,1:2]
        z[3:4,1:2] <- tmp[1:2,1:2]
    })
    m <- modelClass$new(code)
    
})

test_that("detection of non-constant block indices", {
    code <- quote({
        for(i in 1:2) {
            x[i:5] ~ dmnorm(z[i:5], Q[i:5,i:5])
        }
    })
    expect_error(m <- modelClass$new(code), "Non-constant indexing")
    
    code <- quote({
        for(i in 1:2) {
            x[i,(i+1):5] ~ dmnorm(z[(i+1):5], Q[(i+1):5,(i+1):5])
        }
    })
    expect_error(m <- modelClass$new(code), "Non-constant indexing")

})
