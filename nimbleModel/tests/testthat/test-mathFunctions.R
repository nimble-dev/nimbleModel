test_that("BUGS math functions work in model code", {
  code <- nimbleCode({
    a ~ dnorm(0, 1)
    b1 <- step(a) + equals(a, 0.5) + pow(a, 2) + inprod(v[1:2], v[1:2])
    b2 <- expit(a) + ilogit(a) + logit(0.3) + probit(0.3) + iprobit(a) + phi(a) +
      cloglog(0.3) + icloglog(a) + loggam(2.5) + lfactorial(3) + cube(a)
    d <- logdet(S[1:2, 1:2])
    Sinv[1:2, 1:2] <- inverse(S[1:2, 1:2])
    v[1:2] ~ dmnorm(mu[1:2], cov = S[1:2, 1:2])
    logit(p) <- a
    y ~ dbern(p)
  })
  S <- matrix(c(2, 0.5, 0.5, 1), 2)
  m <- nimbleModel(code,
    constants = list(mu = c(0, 0)), data = list(y = 1, S = S),
    inits = list(a = 0.5, v = c(1, 2))
  )
  for (a in c(-1, 0.5, 2)) {
    m$a <- a
    m$calculate()
    v <- c(1, 2)
    expect_equal(m$b1, (a >= 0) + (a == 0.5) + a^2 + sum(v * v))
    expect_equal(
      m$b2,
      2 * plogis(a) + qlogis(0.3) + qnorm(0.3) + 2 * pnorm(a) + log(-log(1 - 0.3)) +
        (1 - exp(-exp(a))) + lgamma(2.5) + lfactorial(3) + a^3
    )
    expect_equal(m$p, plogis(a))
  }
  expect_equal(m$d, as.numeric(determinant(S)$modulus))
  expect_equal(m$Sinv, solve(S))
})

test_that("expressions of constants using math functions are pre-evaluated", {
  code <- nimbleCode({
    y1 ~ dnorm(exp(c1), 1)
    y2 ~ dnorm(pow(c1, 2), 1)
    y3 ~ dnorm(step(c1) + equals(c1, 2), 1)
    y4 ~ dnorm(logit(c1 / 4) + cube(c1), 1)
  })
  m <- nimbleModel(code, constants = list(c1 = 2), inits = list(y1 = 0, y2 = 0, y3 = 0, y4 = 0))
  declCode <- lapply(m$modelDef$declInfo, function(d) d$code)
  expect_length(declCode, 4) # Nothing lifted.
  means <- sapply(declCode, function(x) x[[3]]$mean)
  expect_equal(means, c(exp(2), 4, 2, 8))
})

test_that("user-written chol and inverse in multivariate arguments are lifted", {
  code <- nimbleCode({
    v[1:2] ~ dmnorm(mu[1:2], cholesky = chol(S[1:2, 1:2]), prec_param = 0)
    w[1:2] ~ dmnorm(mu[1:2], cov = inverse(S[1:2, 1:2]))
  })
  S <- matrix(c(2, 0.5, 0.5, 1), 2)
  m <- withNimbleModelOptions(
    list(disallowMultivariateArgumentExpressions = FALSE),
    nimbleModel(code, constants = list(mu = c(0, 0)), data = list(S = S), inits = list(v = c(1, 2), w = c(1, 2)))
  )
  declInfo <- m$modelDef$declInfo
  liftedRHS <- lapply(declInfo[grepl("^lifted_", sapply(declInfo, function(d) d$targetNodeName))], function(d) d$valueExpr)
  # `cov = inverse(S)` is reparameterized to `cholesky = chol(inverse(S))`, which is lifted as a whole.
  expect_identical(liftedRHS, list(quote(nChol(S[1:2, 1:2])), quote(nChol(nInverse(S[1:2, 1:2])))))
  expect_false("nChol" %in% all.names(declInfo[[2]]$valueExpr))
  m$calculate()
  dmnormLogDensity <- function(x, Sigma) {
    U <- chol(Sigma)
    sum(dnorm(backsolve(U, x, transpose = TRUE), log = TRUE)) - sum(log(diag(U)))
  }
  expect_equal(m$calculate("v[1:2]"), dmnormLogDensity(c(1, 2), S))
  expect_equal(m$calculate("w[1:2]"), dmnormLogDensity(c(1, 2), solve(S)))
})
