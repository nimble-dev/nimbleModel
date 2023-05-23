context("modelDeclClass")

test_that("modelDeclClass works", {
    test1 <- modelDeclClass$new(
                                code = quote(a ~ dnorm(0, 1)),
                                context = NULL,
                                sourceLineNum = 2)
    test1$makeSymbolicParentNodes(nimFunNames = list())
    expect_identical(test1$symbolicParentNodes,
                     NULL)
    
    test1 <- modelDeclClass$new(
                                code = quote(a[i] ~ dnorm(b * const * mu[i+1], sigma)),
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

test_that("getSymbolicParentNodes works", {
    expect_equal(
        getSymbolicParentNodes(
            quote(foo(a, x[i] * y[i+1] + w)),
            constNames = list(),
            indexNames = list(quote(i)),
            nimbleFunctionNames = list(quote(foo)),
            addDistNames = FALSE
        ),
        list(quote(a),
             quote(x[i]),
             quote(y[i+1]),
             quote(w))
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

    expect_identical(
        modelDef$declInfo[[1]]$symbolicParentNodes,
        list(quote(mu[i])))
    expect_identical(
        modelDef$declInfo[[2]]$symbolicParentNodes,
        list(quote(tau)))
    expect_identical(
        modelDef$declInfo[[3]]$symbolicParentNodes,
        list(quote(lifted_mu_oBi_cB_plus_7_L2[i]), quote(lifted_d1_over_sqrt_oPtau_cP),
             quote(tau)))
    expect_identical(
        modelDef$declInfo[[4]]$symbolicParentNodes,
        list(quote(theta)))
    expect_identical(
        modelDef$declInfo[[5]]$symbolicParentNodes,
        list(quote(mu0), quote(lifted_d1_over_sqrt_oPtheta_cP), quote(theta)))
    expect_identical(
        modelDef$declInfo[[6]]$symbolicParentNodes, list())
    expect_identical(
        modelDef$declInfo[[7]]$symbolicParentNodes, list())
    
})
