library(nimbleModel)
library(testthat)

test_that("basic getParam and getBound works from full interface", {
  # Multivariate distributions are tested below.
  code <- quote({
    mu ~ dnorm(0, 1)
    sd ~ dunif(0, 2)
    for (i in 1:5) {
      y[i] ~ dnorm(mean = mu + 1, sd = 2 * sd)
    }
    v ~ dunif(mu + 1, mu + 5)
  })
  set.seed(1)
  y <- rnorm(5)
  alpha <- runif(5) + 1
  z <- runif(5)
  z <- z / sum(z)
  mu <- rnorm(1)
  sd <- runif(1, 0, 2)
  data <- list(y = y, z = z, alpha = alpha)
  inits <- list(mu = mu, sd = sd, v = mu + 2.2)
  mclass <- nimbleModel(code, data = data, inits = inits, returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompiler::nCompile(mclass)
  cm <- cmclass$new()
  m$calculate()
  cm$calculate()

  # Commented-out code constructed instrList objects manually
  # Now that is supported (and tested) inside the functions
  # vr <- varRangeClass$new('y[3]')
  # instrList <- makeInstrList(m, vr)
  # y3 <- instrList$instr_nList[[1]]
  # vr <- varRangeClass$new("v")
  # instrList <- makeInstrList(m, vr)
  # v <- instrList$instr_nList[[1]]
  y3 <- "y[3]"
  v <- "v"
  for (case in c("comp", "uncomp")) {
    obj <- switch(case,
      comp = cm,
      uncomp = m
    )

    expect_equal(obj$getParamID(y3, "value"), 1)
    expect_equal(obj$getParamID(y3, "sd"), 3)
    expect_equal(obj$getParamID(v, "max"), 3)

    expect_equal(obj$getParam(y3, "value"), y[3])
    expect_equal(obj$getParam(y3, "mean"), mu + 1)
    expect_equal(obj$getParam(y3, "sd"), 2 * sd)
    expect_equal(obj$getParam(y3, "var"), (2 * sd)^2)
    expect_equal(obj$getParam(y3, "tau"), 1 / (2 * sd)^2)
    expect_equal(obj$getBound(y3, "upper"), Inf)
    expect_equal(obj$getBound(y3, "lower"), -Inf)

    expect_equal(obj$getParam(v, "value"), mu + 2.2)
    expect_equal(obj$getParam(v, "min"), mu + 1)
    expect_equal(obj$getParam(v, "max"), mu + 5)
    expect_equal(obj$getBound(v, "lower"), mu + 1)
    expect_equal(obj$getBound(v, "upper"), mu + 5)
  }
  rm(m, cm)
  gc()
})

test_that("getParam, calculate and simulate work for multivariate distributions", {
  code <- quote({
    x1[1:3] ~ dmnorm(mu[1:3], prec = P[1:3, 1:3])
    x2[1:3] ~ dmnorm(mu[1:3], cov = P[1:3, 1:3])
    x3[1:3] ~ dmnorm(mu[1:3], cholesky = U[1:3, 1:3], prec_param = 0)
    x4[1:3] ~ dmvt(mu[1:3], prec = P[1:3, 1:3], df = 4)
    W1[1:3, 1:3] ~ dwish(R = P[1:3, 1:3], df = 5)
    W2[1:3, 1:3] ~ dinvwish(S = P[1:3, 1:3], df = 5)
    z[1:3] ~ ddirch(alpha[1:3])
    k ~ dcat(p[1:3])
    n[1:3] ~ dmulti(p[1:3], N)
  })
  P <- crossprod(matrix(c(2, .3, .1, .3, 1.5, .2, .1, .2, 1), 3))
  mu <- c(.1, .2, .3)
  inits <- list(
    mu = mu, P = P, U = chol(P), alpha = c(1, 2, 3), p = c(.2, .3, .5), N = 10,
    x1 = 1:3, x2 = 1:3, x3 = 1:3, x4 = 1:3, W1 = 2 * P, W2 = 2 * P, z = c(.2, .3, .5), k = 2, n = c(2, 3, 5)
  )
  mclass <- nimbleModel(code, inits = inits, returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompiler::nCompile(mclass)
  cm <- cmclass$new()
  for (v in names(inits)) cm[[v]] <- inits[[v]]

  r_lp <- m$calculate()
  expect_equal(cm$calculate(), r_lp)
  expect_equal(
    m$calculate("x1"),
    nCompiler::dmnorm_chol(1:3, mu, chol(P), prec_param = 1, log = TRUE)
  )
  expect_equal(m$calculate("k"), log(.3))

  for (obj in list(m, cm)) {
    expect_equal(obj$getParam("x1", "mean"), mu)
    expect_equal(obj$getParam("x1", "prec"), P)
    expect_equal(obj$getParam("x1", "cov"), solve(P))
    expect_equal(obj$getParam("x1", "cholesky"), chol(P))
    expect_equal(obj$getParam("x2", "cov"), P)
    expect_equal(obj$getParam("x2", "prec"), solve(P))
    expect_equal(obj$getParam("x3", "cov"), P)
    expect_equal(obj$getParam("x4", "prec"), P)
    expect_equal(obj$getParam("x4", "df"), 4)
    expect_equal(obj$getParam("W1", "R"), P)
    expect_equal(obj$getParam("W1", "S"), solve(P))
    expect_equal(obj$getParam("W1", "df"), 5)
    expect_equal(obj$getParam("W2", "S"), P)
    expect_equal(obj$getParam("W2", "R"), solve(P))
    expect_equal(obj$getParam("z", "alpha"), c(1, 2, 3))
    expect_equal(obj$getParam("k", "prob"), c(.2, .3, .5))
    expect_equal(obj$getParam("n", "prob"), c(.2, .3, .5))
    expect_equal(obj$getParam("n", "size"), 10)
  }

  # Compiled simulate uses the same kernels as uncompiled, so draws match for the same seed.
  for (node in c("x1", "x2", "x3", "x4", "W1", "W2", "z", "k", "n")) {
    set.seed(5)
    m$simulate(node)
    set.seed(5)
    cm$simulate(node)
    expect_equal(cm[[node]], m[[node]], info = node)
  }
  expect_true(m$k %in% 1:3)
  expect_equal(sum(m$z), 1)
  expect_equal(sum(m$n), 10)
  expect_equal(m$W1, t(m$W1))
  expect_equal(cm$calculate(), m$calculate())
  rm(m, cm)
  gc()
})
