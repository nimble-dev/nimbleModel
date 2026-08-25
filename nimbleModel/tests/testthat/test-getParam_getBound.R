library(nimbleModel)
library(testthat)

cat("getParam and getBound need multivariate tests\n")

test_that("basic getParam and getBound works", {
  # We need a vector node test, but at the time of this writing,
  # dmnorm is blocked by calc_dmnormAltParams
  # ddirch is blocked by needing handlers for rdirch
  code <- quote({
    mu ~ dnorm(0, 1)
    sd ~ dunif(0, 2)
    for(i in 1:5)
      y[i] ~ dnorm(mean = mu + 1, sd = 2*sd)
    v ~ dunif(mu + 1, mu + 5)
  #  z[1:5] ~ ddirch(alpha[1:5])
  })
  set.seed(1)
  y <- rnorm(5)
  alpha <- runif(5) + 1
  z <- runif(5)
  z <- z / sum(z)
  mu <- rnorm(1)
  sd <- runif(1, 0, 2)
  data <- list(y = y, z  = z, alpha = alpha)
  inits <- list(mu = mu, sd = sd, v = mu + 2.2)
  mclass <- nimbleModel(code, data = data, inits = inits, returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompiler::nCompile(mclass)
  cm <- cmclass$new()
  m$calculate()
  cm$calculate()

  vr <- varRangeClass$new('y[3]')
  instrList <- makeInstrList(m, vr)
  y3 <- instrList[[1]]
  vr <- varRangeClass$new("v")
  instrList <- makeInstrList(m, vr)
  v <- instrList[[1]]
  for(case in c("comp", "uncomp")) {
    obj <- switch(case, comp = cm, uncomp = m)
    expect_equal(obj$getParam(y3, "value"), y[3])
    expect_equal(obj$getParam(y3, "mean"), mu + 1)
    expect_equal(obj$getParam(y3, "sd"), 2*sd)
    expect_equal(obj$getParam(y3, "var"), (2*sd)^2)
    expect_equal(obj$getParam(y3, "tau"), 1/(2*sd)^2)
    expect_equal(obj$getBound(y3, "upper"), Inf)
    expect_equal(obj$getBound(y3, "lower"), -Inf)

    expect_equal(obj$getParam(v, "value"), mu+2.2)
    expect_equal(obj$getParam(v, "min"), mu  + 1)
    expect_equal(obj$getParam(v, "max"), mu  + 5)
    expect_equal(obj$getBound(v, "lower"), mu  + 1)
    expect_equal(obj$getBound(v, "upper"), mu  + 5)
  }
})
