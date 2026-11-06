library(nimbleModel)
library(testthat)
library(nCompiler)

# Uncompiled nimCopy and its logProb expansion. Compiled nimCopy (in nimble2 nimbleFunctions)
# uses the same expansion through makeMultiCopier and is tested in nimble2.

make_nimCopy_model <- function(y = matrix(1:12, 3), mu = 0.2, x = matrix(1:6, 3)) {
  code <- nimbleCode({
    mu ~ dnorm(0, sd = 2)
    for (i in 1:3) {
      x[i, 1:2] ~ dmnorm(m0[1:2], pr[1:2, 1:2])
      for (j in 1:4) y[i, j] ~ dnorm(x[i, 1], 1)
    }
    w <- mu + 1
    for (k in 1:2) q[1:2, k] ~ dmnorm(m0[1:2], pr[1:2, 1:2])
  })
  m <- nimbleModel(code,
    data = list(y = y),
    inits = list(mu = mu, m0 = c(0, 0), pr = diag(2), x = x, q = diag(2))
  )
  m$calculate()
  m
}

test_that("logProbNodes gives logProb blocks, including for partly covered nodes", {
  m <- make_nimCopy_model()
  expect_identical(nimbleModel:::logProbNodes(m, "mu"), "logProb_mu")
  expect_identical(nimbleModel:::logProbNodes(m, "x"), "logProb_x[1:3, 1]")
  expect_identical(nimbleModel:::logProbNodes(m, "x[2:3, 1]"), "logProb_x[2:3, 1]") # partly covered
  expect_identical(nimbleModel:::logProbNodes(m, "q[2, 1:2]"), "logProb_q[1, 1:2]") # internal range first
  expect_identical(nimbleModel:::logProbNodes(m, "y[1:2, 2:3]"), "logProb_y[1:2, 2:3]")
  expect_identical(nimbleModel:::logProbNodes(m, "y"), "logProb_y[1:3, 1:4]")
  expect_identical(nimbleModel:::logProbNodes(m, c("w", "mu")), "logProb_mu") # deterministic w has none
  expect_length(nimbleModel:::logProbNodes(m, "w"), 0)
})

test_that("logProbNodes gives one element per node when there is no block", {
  code <- nimbleCode({
    for (i in 1:3) z[i, i:(i + 1)] ~ dmnorm(m0[1:2], pr[1:2, 1:2]) # internal range depends on i
    for (i in 1:3) v[2 * i] ~ dnorm(0, 1)
  })
  m <- suppressMessages(nimbleModel(code, inits = list(m0 = c(0, 0), pr = diag(2))))
  expect_identical(
    nimbleModel:::logProbNodes(m, "z"),
    c("logProb_z[1, 1]", "logProb_z[2, 2]", "logProb_z[3, 3]")
  )
  expect_identical(nimbleModel:::logProbNodes(m, "v"), c("logProb_v[2]", "logProb_v[4]", "logProb_v[6]"))
})

test_that("uncompiled nimCopy copies between models and modelValues, with logProb and logProbOnly", {
  m <- make_nimCopy_model()
  mv <- modelValues(m)$new()
  resize(mv, 2)

  # model to modelValues, with logProb
  nimCopy(m, mv, nodes = c("mu", "x[2:3, 1:2]"), row = 2, logProb = TRUE)
  expect_equal(mv["mu", 2], 0.2)
  expect_equal(mv["x", 2], rbind(0, m$x[2:3, ]))
  expect_equal(mv["logProb_mu", 2], m$logProb_mu)
  expect_equal(mv["logProb_x", 2], cbind(c(0, m$logProb_x[2:3, 1]), 0))
  expect_equal(mv["x", 1], matrix(0, 3, 2)) # other row untouched

  # nodes missing: all variables, or only logProb variables
  mv2 <- modelValues(m)$new()
  resize(mv2, 1)
  nimCopy(m, mv2, row = 1, logProbOnly = TRUE)
  expect_equal(mv2["logProb_y", 1], m$logProb_y)
  expect_equal(mv2["x", 1], matrix(0, 3, 2))
  nimCopy(m, mv2, row = 1)
  expect_equal(mv2["x", 1], m$x)
  expect_equal(mv2["pr", 1], diag(2))

  # modelValues to model, with nodesTo
  m3 <- make_nimCopy_model(y = matrix(0, 3, 4), mu = 0, x = matrix(0, 3, 2))
  nimCopy(mv, m3, nodes = "x[2:3, 1:2]", nodesTo = "x[1:2, 1:2]", row = 2)
  expect_equal(m3$x, rbind(m$x[2:3, ], 0))

  # model to model, with logProb; a whole variable keeps its dimensions
  nimCopy(m, m3, nodes = "y", logProb = TRUE)
  expect_equal(m3$y, m$y)
  expect_equal(m3$logProb_y, m$logProb_y)

  # modelValues to modelValues needs a model for logProb expansion of nodes, but not with nodes missing
  expect_error(nimCopy(mv, mv2, nodes = "x", row = 2, rowTo = 1, logProb = TRUE), "must be a model")
  nimCopy(mv, mv2, row = 2, rowTo = 1, logProb = TRUE)
  expect_equal(mv2["x", 1], mv["x", 2])
  expect_equal(mv2["logProb_x", 1], mv["logProb_x", 2])

  # logProb from a modelValues to a model uses the model to find the logProb elements
  m3$logProb_mu <- 0
  nimCopy(mv, m3, nodes = "mu", row = 2, logProb = TRUE)
  expect_equal(m3$logProb_mu, m$logProb_mu)

  expect_error(nimCopy(m, m3, nodes = "x", nodesTo = "mu"), "6 values to copy, but to has 1")
  expect_error(nimCopy(mv, m3, nodes = "x"), "row is needed")
})
