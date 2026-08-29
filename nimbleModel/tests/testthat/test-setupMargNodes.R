# This file tests both setupMargNodes and model$getConditionallyIndependentSets
# There is some overlap with test-ADlaplace, which relies on these features.

setNimbleModelOption('nodesAsChars', TRUE)

test_that("getConditionallyIndependentSets works in model with a couple of sets", {
  mc <- nimbleCode({
    mu ~ dnorm(0,1)
    for(i in 1:2) {
      x[i] ~ dnorm(mu, 1)
      y[i] ~ dnorm(x[i], 1)
      z[i] ~ dnorm(y[i], 1)
    }
  })
  m <- nimbleModel(mc, data = list(z = 1:2))

  expect_identical(getConditionallyIndependentSets(m), list(c('x[1]', 'y[1]'), c('x[2]', 'y[2]')))
  expect_identical(getConditionallyIndependentSets(m, 'y[2]', explore = "down"), list('y[2]'))
  expect_identical(getConditionallyIndependentSets(m, 'x[1:2]', explore = "up"), list(c('x[1]'), c('x[2]')))
  expect_true(nimble:::testConditionallyIndependentSets(m, getConditionallyIndependentSets(m)))
  # expect_identical(getConditionallyIndependentSets(m, omit = 'y[2]'), list(c('x[1]', 'y[1]'), c('x[2]')))
  # expect_identical(getConditionallyIndependentSets(m, omit = 5), list(c('x[1]', 'y[1]'), c('x[2]')))
  expect_identical(getConditionallyIndependentSets(m, 'x[1]'), list(c('x[1]')))
  expect_identical(getConditionallyIndependentSets(m, 'x[1]', unknownAsGiven=FALSE), list(c('x[1]', 'y[1]')))

  SMN <- setupMargNodes(m)
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), "mu")
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("x[1]", "x[2]", "y[1]", "y[2]"))

  SMN <- setupMargNodes(m, randomEffectsNodes = c("y[1]"))
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), "x[1]")
  expect_identical(unlist(lapply(SMN$calcNodes, \(x) x$toNodeChars())), c("y[1]", "z[1]"))

  SMN <- setupMargNodes(m, paramNodes = character())
  expect_identical(SMN$randomEffectsNodes, list())
  expect_identical(SMN$calcNodes, NULL)

  SMN <- setupMargNodes(m, calcNodes = c("y[1]", "z[1]"))
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), "x[1]")
  expect_identical(SMN$randomEffectsNodes[[1]]$toNodeChars(), "y[1]")

  SMN <- setupMargNodes(m, calcNodes = c("x[1]", "y[1]", "z[1]"))
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), "mu")
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("x[1]","y[1]"))

  SMN <- setupMargNodes(m, randomEffectsNodes = "mu")
  expect_identical(SMN$paramNodes, NULL)
  expect_identical(unlist(lapply(SMN$calcNodes, \(x) x$toNodeChars())), c("mu","x[1]","x[2]"))

  expect_message(SMN <- setupMargNodes(m, paramNodes = character(),
                                      randomEffectsNodes = "x[1]", calcNodes = c("x[1]", "y[1]")),
                "some `randomEffectsNodes` provided")
  expect_message(SMN <- setupMargNodes(m, paramNodes = c("mu"),
                                      randomEffectsNodes = "y[1]", calcNodes = c("y[1]", "z[1]")),
                "included in `randomEffectsNodes`")

})

test_that("setupMargNodes/GCIS works in model with an extra edge among random effects", {
  mc <- nimbleCode({
    mu ~ dnorm(0,1)
    sigma ~ dunif(0,1)
    for(i in 1:2) {
      x[i] ~ dnorm(mu, 1)
      y[i] ~ dnorm(x[i], sigma)
      z[i] ~ dnorm(y[i] + x[i], 1)
    }
  })
  m <- nimbleModel(mc, data = list(z = 1:2))
  SMN <- setupMargNodes(m)
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("mu", "sigma"))
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("x[1]", "x[2]", "y[1]", "y[2]"))

  expect_message(SMN <- setupMargNodes(m, randomEffectsNodes = "z[1]"),
                 "some `randomEffectsNodes` provided")
  expect_message(SMN <- setupMargNodes(m, randomEffectsNodes = "x[1]", calcNodes = c("y[1]", "z[1]")),
                 "included in `randomEffectsNodes`")
  expect_message(SMN <- setupMargNodes(m, randomEffectsNodes = "x[1]", calcNodes = c("x[1]","y[1]", "z[1]")),
                 "included in `randomEffectsNodes`")
  expect_message(SMN <- setupMargNodes(m, paramNodes = "mu", randomEffectsNodes = "y[1]"),
                 "included in `randomEffectsNodes`")
  SMN <- setupMargNodes(m, paramNodes = "x[1]", randomEffectsNodes = "y[1]")
  expect_identical(unlist(lapply(SMN$calcNodes, \(x) x$toNodeChars())),c('y[1]','lifted_y_oBi_cB_plus_x_oBi_cB_L6[1]','z[1]'))
})

test_that("setupMargNodes/GCIS works in model with an extra edge from param to random effects", {
  mc <- nimbleCode({
    mu ~ dnorm(0,1)
    sigma ~ dunif(0,1)
    for(i in 1:2) {
      x[i] ~ dnorm(mu, 1)
      y[i] ~ dnorm(x[i], 1)
      z[i] ~ dnorm(y[i] + mu, sd = sigma)
    }
  })
  m <- nimbleModel(mc, data = list(z = 1:2))
  SMN <- setupMargNodes(m)
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("mu", "sigma"))
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("x[1]", "x[2]", "y[1]", "y[2]"))

  SMN <- setupMargNodes(m, randomEffectsNodes = c("y[1]", "x[2]"))
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("mu", "sigma", "x[1]"))

  SMN <- setupMargNodes(m, randomEffectsNodes = c("mu", "x[1]"))
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("sigma", "y[2]"))
  expect_identical(unlist(lapply(SMN$givenNodes, \(x) x$toNodeChars())), c('sigma','y[2]','x[2]','y[1]','z[1]','z[2]'))

  # Warning from missing deterministic nodes
  expect_message(SMN <- setupMargNodes(m, calcNodes = c("x[1]","x[2]","y[1]","y[2]","z[1]","z[2]")),
                 "included in the `calcNodes`")

  SMN <- setupMargNodes(m, calcNodes = m$getDependencies('x',downstream=TRUE))
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("mu","sigma"))
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("x[1]","x[2]","y[1]","y[2]"))
})

test_that("setupMargNodes/GCIS catches discrete randomEffectsNode", {
  code <- nimbleCode({
    p ~ dnorm(0,1)
    re1 ~ dnorm(p, 1)
    re2 ~ dbern(re1)    ## discrete node here
    re3 ~ dnorm(re2, 1)
    y ~ dnorm(re3, 1)
  })
  Rmodel <- nimbleModel(code, data = list(y = 1))

  expect_message(SMN <- setupMargNodes(Rmodel, 'p'), "discrete")
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c('re1','re3'))
})

test_that("getConditionallyIndependentSets works in model with one set and deterministic intermediates", {
  m <- nimbleModel(
    nimbleCode({
      P1 ~ dnorm(0,1)
      D1 <- P1 + 1

      REA1 ~ dnorm(D1, 1)
      D2 <- REA1 + 1
      REA2 ~ dnorm(D2, 1)
      D3 <- REA2 + 1

      REB1 ~ dnorm(D1, 1)
      C2 <- REB1 + 1
      REB2 ~ dnorm(C2, 1)
      C3 <- REB2 + 1

      D3C3 <- D3 + C3
      Y1 ~ dnorm(D3C3, 1)
    }),
    data = list(Y1 = 1)
  )
  # All sets
  expect_identical(getConditionallyIndependentSets(m), list(c("REA1", "REA2", "REB2", "REB1")))
  # first-stage latents stay separated
  expect_identical(getConditionallyIndependentSets(m, c("REA1", "REB1")), list("REA1", "REB1"))
  # first-stage latents are combined if unknownAsGiven is FALSE
  expect_identical(getConditionallyIndependentSets(m, c("REA1", "REB1"), unknownAsGiven=FALSE), list(c("REA1", "REA2", "REB2", "REB1")))
  # second-stage latents are connected
  expect_identical(getConditionallyIndependentSets(m, c("REA2", "REB2")), list(c("REA2", "REB2")))
  # TODO: need to allow deterministic as given; currently error-trapped.
  # Using determinstics as given works
  # expect_identical(getConditionallyIndependentSets(m, givenNodes = c("D1", "D3C3")), list(c("REA1", "REB1", "REA2", "REB2")))
  # Using determinstics as given works
  # expect_identical(getConditionallyIndependentSets(m, givenNodes = c("D1", "D3", "C3")), list(c("REA1", "REA2"), c("REB1", "REB2")))
  # expect_identical(getConditionallyIndependentSets(m, givenNodes = c("D1", "D3")),
  #                 list(c("REA1", "REA2")))
  # expect_identical(getConditionallyIndependentSets(m, givenNodes = c("D3")),
  #                 list(c("REA1", "REA2")))
  # expect_identical(getConditionallyIndependentSets(m, givenNodes = c("D3"), unknownAsGiven=FALSE),
  #                 list(c("P1", "REA1", "REB1", "REA2", "REB2", "Y1")))

  SMN <- setupMargNodes(m)
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), "P1")
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("REA1", "REA2", "REB1", "REB2"))
  expect_identical(SMN$randomEffectsSets, list(c('REA1','REA2','REB2','REB1')))

  SMN <- setupMargNodes(m, paramNodes = "REA1")
  expect_identical(SMN$randomEffectsNodes[[1]]$toNodeChars(), c('REA2'))
  expect_identical(unlist(lapply(SMN$calcNodes, \(x) x$toNodeChars())), c('REA2','D3','D3C3','Y1'))

  SMN <- setupMargNodes(m, calcNodes = m$getDependencies("REB2"))
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("REA2", "REB1"))

  SMN <- setupMargNodes(m, randomEffectsNodes = c("P1", "REA1","REA2", "REB1","REB2"))
  expect_identical(SMN$paramNodes, NULL)
  expect_identical(sort(unlist(lapply(SMN$calcNodes, \(x) x$toNodeChars()))),
                   sort(c('P1','D1','REA1','REB1','D2','C2','REA2','REB2','D3','C3','D3C3','Y1')))
})

test_that("getConditionallyIndependentSets works in state-space model with a couple of sets", {
  # Two state-space models, one of which has data at the end.
  mc <- nimbleCode({
    x[1] ~ dnorm(0, 1)
    w[1] ~ dnorm(0, 1)
    for(i in 1:3) {
      x[i+1] ~ dnorm(x[i], 1)
      y[i+1] ~ dnorm(x[i+1], 1)
      w[i+1] ~ dnorm(w[i], 1)
      z[i+1] ~ dnorm(w[i+1], 1)
    }
  })
  m <- nimbleModel(mc, data = list(y = 1:4))
  expect_identical(getConditionallyIndependentSets(m, endAsGiven=TRUE), list(c("x[4]", "x[3]", "x[2]"),
                                                                             c("w[4]", "w[3]", "w[2]")))
  expect_identical(getConditionallyIndependentSets(m), list(c("x[4]", "x[3]", "x[2]")))
  expect_identical(getConditionallyIndependentSets(m, "x[1:2]", givenNodes = c("w[1]", "y"), unknownAsGiven=FALSE),
                   list(c("x[1]", "x[2]", "x[3]", "x[4]")))
  expect_identical(getConditionallyIndependentSets(m, "x[2]", givenNodes = c("w[1]", "y"), unknownAsGiven=FALSE),
                   list(c("x[2]", "x[1]", "x[3]", "x[4]")))
  expect_identical(getConditionallyIndependentSets(m, "x[1]", givenNodes = c("w[1]", "y"), unknownAsGiven=FALSE),
                   list(c("x[1]", "x[2]", "x[3]", "x[4]")))
  expect_identical(getConditionallyIndependentSets(m, givenNodes = c("y"), unknownAsGiven=FALSE),
                   list(c("x[4]", "x[3]", "x[2]", "x[1]")))
  expect_identical(getConditionallyIndependentSets(m, 'z[1]'), list())
  expect_true(nimble:::testConditionallyIndependentSets(m, getConditionallyIndependentSets(m)))

  SMN <- setupMargNodes(m)
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), "x[1]")
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("x[4]","x[2]","x[3]"))

  SMN <- setupMargNodes(m, randomEffectsNodes = 'x[1:4]')
  expect_identical(SMN$paramNodes, NULL)
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("x[1]","x[2]","x[3]","x[4]"))
})

test_that("getConditionallyIndependentSets works in model with diamond shape", {
  # diamond graph shape
  mc <- nimbleCode({
    mu ~ dnorm(0,1)
    for(i in 1:2) {
      x[i] ~ dnorm(mu, 1)
    }
    y ~ dnorm(x[1] + x[2], 1)
  })
  m <- nimbleModel(mc, data = list(y = 1))

  expect_identical(getConditionallyIndependentSets(m), list(c("x[1]","x[2]")))
  expect_true(nimble:::testConditionallyIndependentSets(m, getConditionallyIndependentSets(m)))

})

test_that("getConditionallyIndependentSets works in double-state state-space model", {
  # two stae-space chains of latent states with one data set that depends on both
  mc <- nimbleCode({
    x[1] ~ dnorm(0, 1)
    w[1] ~ dnorm(0, 1)
    for(i in 1:3) {
      x[i+1] ~ dnorm(x[i], 1)
      w[i+1] ~ dnorm(w[i], 1)
      y[i+1] ~ dnorm(x[i+1] + w[i+1], 1)
    }
  })
  m <- nimbleModel(mc, data = list(y = 1:4))

  expect_identical(getConditionallyIndependentSets(m),
                   list(c('x[4]','x[3]','x[2]','w[2]','w[3]','w[4]')))
  # expect_identical(getConditionallyIndependentSets(m, omit = "w[2]"),
  #                  list(c("x[2]", "x[3]", "w[3]", "x[4]", "w[4]")))
  expect_identical(getConditionallyIndependentSets(m, givenNodes = c("y", "w[3]"), unknownAsGiven=FALSE),
                   list(c("x[4]", "x[3]", "x[2]", "x[1]", "w[2]", "w[1]", "w[4]")))
  expect_identical(getConditionallyIndependentSets(m, givenNodes = c("y", "x[3]", "w[3]"), unknownAsGiven=FALSE),
                   list(c("x[4]", "w[4]"), c("x[2]", "x[1]", "w[2]", "w[1]")))
  expect_true(nimble:::testConditionallyIndependentSets(m, getConditionallyIndependentSets(m)))

  SMN <- setupMargNodes(m)
  expect_identical(SMN$randomEffectsSets,
                   list(c('x[4]','x[3]','x[2]','w[2]','w[3]','w[4]')))
})

test_that("getConditionallyIndependentSets works in model with LHSinferred (aka split) nodes", {
  c3 <- nimbleCode({
    a[1:3] ~ dmnorm( mu[1:3], cov = cov[1:3, 1:3])
    b[1:3] <- a[1] + c(1, 2, 3)
    sig <- sqrt(var)
    var ~ dunif(0, 1)
    c[1] ~ dnorm(b[1], sd = sig)
    a2 ~ dnorm(0,1) ## isolated node
    sig2 ~ dunif(0,1)
    c[2] ~ dnorm(a[2], sd = sig2)
  })
  m3 <- nimbleModel(c3, inits = list(mu = 1:3, cov = diag(3)))

  expect_identical(getConditionallyIndependentSets(m3), list()) # Note there are no latent nodes
  expect_identical(getConditionallyIndependentSets(m3, "a[1:3]", givenNodes = "sig2", unknownAsGiven=FALSE),
                   list(c("a[1:3]", "c[2]", "c[1]", "var")))
  expect_identical(getConditionallyIndependentSets(m3, "a[1]", givenNodes = "sig2", unknownAsGiven=FALSE),
                   list(c("a[1:3]", "c[2]", "c[1]", "var")))
  expect_identical(getConditionallyIndependentSets(m3, "a[2]", givenNodes = "sig2", unknownAsGiven=FALSE),
                   list(c("a[1:3]", "c[2]", "c[1]", "var")))
# Revisit
  #  expect_identical(getConditionallyIndependentSets(m3, "b[1]", givenNodes = "sig2", unknownAsGiven=FALSE),
#                   list(c("var", "a[1:3]", "c[2]", "c[1]")))
  expect_identical(getConditionallyIndependentSets(m3, m3$getNodes(stochOnly = TRUE), givenNodes = "sig2"),
                   list(c("a[1:3]" , "c[2]", "c[1]", "var"), c("a2")))
})

test_that("getConditionallyIndependentSets works for tweaked pump model", {
  pumpCode <- nimbleCode({
    # This pump code is tweaked so that theta[1] and theta[2] are
    # in a conditionally independent set together.
    # Other theta[i]s are in their own set.
    for (i in 3:N){
      theta[i] ~ dgamma(alpha, beta)
      lambda[i] <- theta[i] * t[i]
      x[i] ~ dpois(lambda[i])
    }
    for (i in 1:2){
      theta[i] ~ dgamma(alpha, beta)
      lambda[i] <- 0.5 * (theta[1] * t[1] + theta[2] * t[2])
      x[i] ~ dpois(lambda[i])
    }
    alpha ~ dexp(1.0)
    beta ~ dgamma(0.1, 1.0)
  })

  pumpConsts <- list(N = 10,
                     t = c(94.3, 15.7, 62.9, 126, 5.24,
                           31.4, 1.05, 1.05, 2.1, 10.5))

  pumpData <- list(x = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22))

  pumpInits <- list(alpha = 0.1, beta = 0.1,
                    theta = rep(0.1, pumpConsts$N))


  ## Create the model
  pump <- nimbleModel(code = pumpCode, name = "pump", constants = pumpConsts,
                      data = pumpData, inits = pumpInits)

  expect_identical(getConditionallyIndependentSets(pump),
                   list('theta[3]', 'theta[4]', 'theta[5]', 'theta[6]',
                        'theta[7]', 'theta[8]', 'theta[9]', 'theta[10]',
                        c('theta[1]', 'theta[2]')))
})

test_that("getConditionallyIndependentSets works with unknownAsGiven=TRUE or FALSE", {
  # This test is from NCT #405
  m <- nimbleModel(
  nimbleCode({
    mu ~ dnorm(0,1)
    for(i in 1:4) a[i] ~ dnorm(mu, 1)
    y[1] ~ dnorm(a[1]+a[2], 1)
    y[2] ~ dnorm(a[1]-a[2], 1)
    y[3] ~ dnorm(a[3]+a[4], 1)
    y[4] ~ dnorm(a[3]-a[4], 1)
  }),
  data = list(y = c(1, 1, 1, 1)),
  inits = list(mu = 1))

  expect_identical(
    getConditionallyIndependentSets(m, "a", givenNodes = c("y"), unknownAsGiven=FALSE),
    list(c("a[1]","mu", paste0("a[", 2:4, "]"))))

  expect_identical(
    getConditionallyIndependentSets(m, "a", givenNodes = c("y"), unknownAsGiven=TRUE),
    list(c('a[1]','a[2]'), c('a[3]','a[4]')))
})

test_that("setupMargNodes/GCIS works with random effects without parameters", {
  m <- nimbleModel({
    nimbleCode({
      P ~ dnorm(0,1)
      for(i in 1:2) RE[i] ~ dnorm(0,1)
      sigma ~ dunif(0,1)
      for(i in 1:2) mu[i] <- P + RE[i] * sigma
      for(i in 1:2) Y[i] ~ dnorm(mu[i], 1)
    })
  }, data = list(Y = rnorm(2)))

  SMN <- setupMargNodes(m)
  expect_identical(SMN$randomEffectsNodes, list())

  SMN <- setupMargNodes(m, randomEffectsNodes = 'RE')
  expect_identical(SMN$randomEffectsNodes[[1]]$toNodeChars(), c('RE[1]', 'RE[2]'))
  expect_identical(SMN$randomEffectsSets, list('RE[1]', 'RE[2]'))
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("P","sigma"))

  SMN <- setupMargNodes(m, paramNodes = "P", randomEffectsNodes = 'RE')
  expect_identical(SMN$randomEffectsNodes[[1]]$toNodeChars(), c('RE[1]', 'RE[2]'))
  expect_identical(SMN$randomEffectsSets, list('RE[1]', 'RE[2]'))
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), c("P"))

  SMN <- setupMargNodes(m, paramNodes = c("P", "sigma"), randomEffectsNodes = 'RE')
  expect_identical(SMN$randomEffectsNodes[[1]]$toNodeChars(), c('RE[1]', 'RE[2]'))
  expect_identical(SMN$randomEffectsSets, list('RE[1]', 'RE[2]'))
  expect_identical(unlist(lapply(SMN$paramNodes, \(x) x$toNodeChars())), c("P", "sigma"))

  expect_message(SMN <- setupMargNodes(m, paramNodes = c("P", "sigma"), randomEffectsNodes = 'RE',
                                       calcNodes = c("RE[1]", "mu[1]", "Y[1]")),
                 "included in the `calcNodes`")
  expect_message(SMN <- setupMargNodes(m, paramNodes = c("P"), randomEffectsNodes = 'RE',
                                       calcNodes = c("RE[1]", "mu[1]", "Y[1]")),
                 "included in the `calcNodes`")
                                        # The next one can't really create meaningful results anyway.
  expect_message(SMN <- setupMargNodes(m, paramNodes = c("P"),
                                       calcNodes = c("RE[1]", "mu[1]", "Y[1]")),
                 "some `calcNodes` provided")
})

test_that("setupMargNodes works with determimistic node as parameter", {
  # This case is not generally useful because it is not supported in buildLaplace,
  # where all params need priors for purpose of determining valid range and
  # distinguishing from covariates and such.
  # However, the setupMargNodes step should work with a deterministic parameter, so
  # here is a test.
  m <- nimbleModel({
    nimbleCode({
      Pstoch ~ dnorm(0,1)
      P <- Pstoch + 1
      for(i in 1:2) RE[i] ~ dnorm(0,1)
      sigma ~ dunif(0,1)
      for(i in 1:2) mu[i] <- P + RE[i] * sigma
      for(i in 1:2) Y[i] ~ dnorm(mu[i], 1)
    })
  }, data = list(Y = rnorm(2)))

  SMN <- setupMargNodes(m, paramNodes = "P", randomEffectsNodes = 'RE')
  # expect_identical(SMN$randomEffectsNodes, c('RE[1]', 'RE[2]'))
  # expect_identical(SMN$randomEffectsSets, list('RE[1]', 'RE[2]'))
  # expect_identical(SMN$paramNodes, c("P"))
})

test_that("setupMargNodes finds correct randomEffectsNodes based on calcNodes input", {
  code <- nimbleCode({
    for(i in 1:2){
      p[i] ~ dnorm(0, 1)
      r[i] ~ dnorm(p[i], 1)
      s[i] ~ dnorm(r[i], 1)
    }
    p[3] ~ dnorm(0, 1)
    y[1] ~ dnorm(p[3], 1)
    y[2] ~ dnorm(s[1] + s[2], 1)
  })
  m <- nimbleModel(code, data = list(y = c(1, 2)))
  # new warning in 1.4.2 and here because randomEffectsNodes now includes s[1:2]
  # add expect_warning()
  SMN <- setupMargNodes(m, calcNodes = c("r", "s"))  
  expect_identical(unlist(lapply(SMN$randomEffectsNodes, \(x) x$toNodeChars())), c("r[1]","r[2]","s[1]","s[2]"))
  expect_identical(SMN$paramNodes[[1]]$toNodeChars(), c("p[1]","p[2]"))
})

test_that("regression tests that `getConditionallyIndependentSets` works with traversal of determ nodes", {
    code <- nimbleCode({
        for(i in 1:5) {
            y[i] ~ dnorm(b1*x[i] + mu[i], sd = exp(b2*x[i]+ mu2[i]))
            mu[i] ~ dnorm(0, tau)
            mu2[i] ~ dnorm(0, tau2)
        }
        b1~dflat()
        b2 ~dflat()
        tau ~ dhalfflat()
        tau2~dhalfflat()
    })
    m <- nimbleModel(code, data = list(y = rnorm(5)))
    given <- c('tau','tau2',paste0('y[', 1:5, ']'))
    
    ## Correct
    latents <- c('b1','b2',paste0('mu[', 1:5, ']'),paste0('mu2[', 1:5, ']'))
    expect_length(getConditionallyIndependentSets(m, nodes = latents, givenNodes = given, unknownAsGiven = TRUE), 1)
    
    ## In issue 1564, incorrect with different order for the latents: `mu2[1]` split out into its own set
    latents <- c(paste0('mu[', 1:5, ']'),paste0('mu2[', 1:5, ']'),'b1','b2')
    expect_length(getConditionallyIndependentSets(m, nodes = latents, givenNodes = given, unknownAsGiven = TRUE), 1)
})

test_that("`setupMargNodes` handling of missing/extra latents", {
    code <- nimbleCode({
        for(i in 1:5)
            y[i] ~ dnorm(b[k[i]], 1)
        for(i in 1:3)
            b[i] ~ dnorm(mu,1)
        mu ~ dnorm(0,1)
    })
    ## Only b[1] and b[2] have data dependents.
    m <- nimbleModel(code, data = list(y = rnorm(5)), constants = list(k = c(1,1,1,2,2)))

    expect_silent(result <- setupMargNodes(m))
    expect_identical(result$randomEffectsNodes[[1]]$toNodeChars(), c("b[1]","b[2]"))
    expect_identical(result$paramNodes[[1]]$toNodeChars(), c("mu"))

    ## `b[2]` won't be marginalized over, and its dependent `y`s are not in calcNodes.
    ## Could be user error, but we simply accept the user choices. 
    expect_silent(result <- setupMargNodes(m, randomEffectsNodes = 'b[1]'))
    expect_identical(result$randomEffectsNodes[[1]]$toNodeChars(), c("b[1]"))
    expect_identical(result$paramNodes[[1]]$toNodeChars(), c("mu"))
    
    ## This gives a warning.
    ## `b[2]` now in `paramNodes`, presumably since its `y`s are in calcNodes.
    expect_message(result <- setupMargNodes(m, randomEffectsNodes = 'b[1]', calcNodes = c('b[1]','y')),
                   "they should be included")
    expect_identical(result$randomEffectsNodes[[1]]$toNodeChars(), c("b[1]"))
    expect_identical(unlist(lapply(result$paramNodes, \(x) x$toNodeChars())), c("b[2]", "mu"))
 
    expect_message(result <- setupMargNodes(m, randomEffectsNodes = 'b'), "they are not needed for the provided")
    expect_identical(result$randomEffectsNodes[[1]]$toNodeChars(), c("b[1]", "b[2]"))
    expect_identical(result$paramNodes[[1]]$toNodeChars(), c("mu"))

    # TODO: presumably rework how this option is handled in nimble 2.
    nimble::nimbleOptions(includeUnneededLatents = TRUE)
    expect_message(result <- setupMargNodes(m, randomEffectsNodes = 'b'), "they are not needed for the provided")
    expect_identical(result$randomEffectsNodes[[1]]$toNodeChars(), c("b[1]", "b[2]", "b[3]"))
    expect_identical(result$paramNodes[[1]]$toNodeChars(), c("mu"))
    ## Note `b[3]` is not in `calcNodes` because `predictiveNodes` are excluded.
    nimble::nimbleOptions(includeUnneededLatents = FALSE)
})

test_that("getConditionallyIndependentSets works with grouped data", {
    code <- nimbleCode({
        for(i in 1:2)
            mu[i] ~ dnorm(mu0, 1)
        for(i in 1:10) {
            y[i] ~ dnorm(mu[k[i]], sd = sigma)
        }
        sigma ~ dunif(0,1)
        mu0 ~ dnorm(0,1)
    })
    m <- nimbleModel(code, data=list(y=rnorm(10)), constants = list(k=c(rep(1,5),rep(2,5))))
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma','y')),
                                                     nodes=m$getNodes('mu')),
                                        list('mu[1]','mu[2]')) # {mu[1]},  {mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','y')),
                                                     nodes=m$getNodes(c('sigma','mu'))),
                                        list(c("sigma","mu[1]","mu[2]"))) # {sigma,mu[1:2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('y','mu'))),
                     list(c("y[1]","mu[1]", paste0("y[",2:5,"]")),
                          c("y[6]","mu[2]", paste0("y[",7:10,"]"))))            # {mu[1],y[1],y[2:5]}, {mu[2],y[6],y[7:10]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('mu','y'))),
                     list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]"))))  # {mu[1],y[1:5]}, {mu[2],y[6:10]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('mu'))),
                                        list('mu[1]','mu[2]')) # unknownAsGiven = T; {mu[1]},{mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('mu')),unknownAsGiven=FALSE),
                     list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]")))) # {mu[1],y[1:5]}, {mu[2],y[6:10]}
    
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma'),
                                                     nodes=c('mu','y')),
                                                            list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]"))))  # {mu[1],y[1:5]}, {mu[2],y[6:10]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma'),
                                                     nodes=c('mu','y'), returnScalarComponents=TRUE, nodesAsChars=TRUE),list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]"))))
                     
                                        #  {"mu[1]" "y[1]"  "y[2]"  "y[3]"  "y[4]"  "y[5]"}  {"mu[2]" "y[6]"  "y[7]"  "y[8]"  "y[9]"  "y[10]"}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma')), list())

    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma'), unknownAsGiven = FALSE), list())
    
    expect_identical(getConditionallyIndependentSets(m, nodes='mu'), list('mu[1]','mu[2]'))  # {mu[1]}, {mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma','y')), list('mu[1]','mu[2]'))  # {mu[1]}, {mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m), list('mu[1]','mu[2]'))  # {mu[1]}, {mu[2]}
    
    
    result <- getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                              nodes=m$getNodes(c('y','mu')))
    expect_identical(result,
                                 list(c("y[1]","mu[1]", paste0("y[",2:5,"]")),
                                      c("y[6]","mu[2]", paste0("y[",7:10,"]"))))  
                                        # {mu[1],y[1],y[2:5]}, {mu[2],y[6],y[7:10]}
 
    # Now with intermediate deterministic nodes.
    code <- nimbleCode({
        for(i in 1:2)
            mu[i] ~ dnorm(mu0, 1)
        for(i in 1:10) {
            mn[i] <- mu[k[i]]
            y[i] ~ dnorm(mn[i], var = sigma)
        }
        sigma ~ dunif(0,1)
        mu0 ~ dnorm(0,1)
    })
    m <- nimbleModel(code, data=list(y=rnorm(10)), constants = list(k=c(rep(1,5),rep(2,5))))

    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma','y')),
                                                     nodes=m$getNodes('mu')),
                                        list('mu[1]','mu[2]')) # {mu[1]},  {mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','y')),
                                                     nodes=m$getNodes(c('sigma','mu'))),
                                        list(c("sigma","mu[1]","mu[2]"))) # {sigma,mu[1:2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('y','mu'))),
                     list(c("y[1]","mu[1]", paste0("y[",2:5,"]")),
                          c("y[6]","mu[2]", paste0("y[",7:10,"]"))))            # {mu[1],y[1],y[2:5]}, {mu[2],y[6],y[7:10]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('mu','y'))),
                     list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]"))))  # {mu[1],y[1:5]}, {mu[2],y[6:10]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('mu'))),
                                        list('mu[1]','mu[2]')) # unknownAsGiven = T; {mu[1]},{mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = m$getNodes(c('mu0','sigma')),
                                                     nodes=m$getNodes(c('mu')),unknownAsGiven=FALSE),
                     list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]")))) # {mu[1],y[1:5]}, {mu[2],y[6:10]}
    
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma'),
                                                     nodes=c('mu','y')),
                                                            list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]"))))  # {mu[1],y[1:5]}, {mu[2],y[6:10]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma'),
                                                     nodes=c('mu','y'), returnScalarComponents=TRUE, nodesAsChars=TRUE),list(c("mu[1]", paste0("y[",1:5,"]")),
                          c("mu[2]", paste0("y[",6:10,"]"))))
                     
                                        #  {"mu[1]" "y[1]"  "y[2]"  "y[3]"  "y[4]"  "y[5]"}  {"mu[2]" "y[6]"  "y[7]"  "y[8]"  "y[9]"  "y[10]"}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma')), list())

    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma'), unknownAsGiven = FALSE), list())
    
    expect_identical(getConditionallyIndependentSets(m, nodes='mu'), list('mu[1]','mu[2]'))  # {mu[1]}, {mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma','y')), list('mu[1]','mu[2]'))  # {mu[1]}, {mu[2]}
    
    expect_identical(getConditionallyIndependentSets(m), list('mu[1]','mu[2]'))  # {mu[1]}, {mu[2]}
    
    # What happens if provide deterministic nodes?
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma','y'),
                                    nodes=c('mn','mu')), list('mu[1]','mu[2]'))  # {mu[1]},  {mu[2]}

    # Cn't have deterministic in givenNodes yet.
    # expect_identical(getConditionallyIndependentSets(m, givenNodes = c('mu0','sigma','y','lifted_sqrt_oPsigma_cP'),
    #                                nodes=c('mn','mu')) , list('mu[1]','mu[2]'))  # {mu[1]},  {mu[2]}

})

test_that("SSM case" , {
    code <- nimbleCode({
        for(i in 2:n) {
            y[i] ~ dnorm(mu[i],sd=sigma)
            mu[i] ~ dnorm(mu[i-1],sd=tau)
        }
        sigma ~ dunif(0,1)
        tau~dunif(0,1)
        mu[1] ~ dnorm(0,1)
    })
    n <- 10
    m <- nimbleModel(code, data=list(y=rnorm(n)), constants = list(n=n))
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('tau','sigma','y'),
                                                     nodes=c('mu')),
                     list(paste0("mu[", 1:10, "]")))

    code <- nimbleCode({
        for(i in 1:n) {
            y[i] ~ dnorm(mu[i],sd=sigma)
            mu[i] ~ dnorm(mu[i+1],sd=tau)
        }
        sigma ~ dunif(0,1)
        tau~dunif(0,1)
        mu[n+1] ~ dnorm(0,1)
    })
    n <- 10
    m <- nimbleModel(code, data=list(y=rnorm(n)), constants = list(n=n))
    
    expect_identical(getConditionallyIndependentSets(m, givenNodes = c('tau','sigma','y'),
                                                     nodes=c('mu')),
                     list(paste0("mu[", 1:11, "]")))
})

    
setNimbleModelOption('nodesAsChars', FALSE)

test_that("{aggregate,intersect,setdiff}_nodes", {
    code <- nimbleCode({
        for(i in 1:2)
            mu[i] ~ dnorm(mu0, 1)
        for(i in 1:10) {
            y[i] ~ dnorm(mu[k[i]], sd = sigma)
        }
        sigma ~ dunif(0,1)
        mu0 ~ dnorm(0,1)
    })
    m <- nimbleModel(code, data=list(y=rnorm(10)), constants = list(k=c(rep(1,5),rep(2,5))))
    
    set1 <- m$getNodes(c('mu','y','sigma'))
    set2 <- m$getNodes(c('y[1:3]','mu0','y[5:6]','mu[1]'))
    
    expect_identical(sapply(intersect_nodes(set1,set2), \(x) x$toNodeChars()),
                            list('mu[1]',paste0("y[", c(1,2,3,5,6), "]")))
    expect_identical(sapply(setdiff_nodes(set1,set2), \(x) x$toNodeChars()),
                            list('mu[2]',paste0("y[", c(4,7:10), "]"), "sigma"))

    expect_identical(aggregate_nodes(m$getNodes(c('y[1:5]', 'y[6:8]')))[[1]]$toNodeChars(),
                            paste0("y[", 1:8, "]"))
    expect_identical(aggregate_nodes(m$getNodes(c('y[1:7]', 'y[6:8]')))[[1]]$toNodeChars(),
                            paste0("y[", 1:8, "]"))
    expect_identical(aggregate_nodes(m$getNodes(c('y[1:6]','y[8]', 'y[6:7]')))[[1]]$toNodeChars(),
                            paste0("y[", 1:8, "]"))
    
    code <- nimbleCode({
        for(i in 1:3)
            y[i,1:2] ~ dmnorm(mu[1:2],pr[1:2,1:2])
    })
    m <- nimbleModel(code)
    
    set <- m$getNodes(c('y[1,1:2]','y[3,1:2]')) 
    expect_identical(aggregate_nodes(set)[[1]]$toNodeChars(),
                                        c("y[1, 1:2]", "y[3, 1:2]")) # node range for `y[idx1, 1:2]`, for `idx1` in c(1, 3)
    set <- m$getNodes(c('y[1,1:2]','y[2,1:2]')) 
    expect_identical(aggregate_nodes(set)[[1]]$toNodeChars(),
                                        c("y[1, 1:2]", "y[2, 1:2]")) # node range for `y[idx1, 1:2]`, for `idx1` in 1:2
    
    set <- m$getNodes(c('y[1,1:2]','y')) 
    expect_identical(aggregate_nodes(set)[[1]]$toNodeChars(),
                                        c("y[1, 1:2]", "y[2, 1:2]", "y[3, 1:2]")) # node range for `y[idx1, 1:2]`, for `idx1` in 1:2

})
