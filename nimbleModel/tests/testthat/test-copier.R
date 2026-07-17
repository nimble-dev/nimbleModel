library(testthat)
library(nimbleModel)
library(nCompiler)

message("multiCopier needs to handle single and blank indices.")

test_that("multiCopier basics works", {
  code <- quote({
    tau ~ dunif(0, 100)
    mu ~ dnorm(0, 1)
    for (i in 1:5) {
      y[i] ~ dnorm(mu, var = tau)
    }
    for (i in 1:5) {
      for (j in 1:5) {
        z[i, j] ~ dnorm(mu, var = tau)
      }
    }
  })

  inits <- list(
    tau = 25, mu = 0,
    z = matrix(rnorm(25), nrow = 5)
  )
  data <- list(y = rnorm(5))

  mclass <- nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
  #  m <- mclass$new()

  nf <- nClass(
    Cpublic = list(
      m = "nimbleModel:::modelBase_nClass()",
      multCopy = "nimbleModel:::multiCopier_nClass()",
      init = nFunction(
        function() {
          multCopy$init(m)
        }
      ),
      get = nFunction(
        function() {
          res <- multCopy$getValues()
          return(res)
        },
        returnType = "numericVector"
      ),
      set = nFunction(
        function(v = "numericVector") {
          # Work is needed on opDef dispatch to support
          # some assignment using multCopy in the syntax via an opDef.
          cppLiteral("multCopy->flatViewGroup.setValues_() = v;")
        }
      )
    )
  )
  comp <- nCompile(mclass, nf)

  m <- comp$mclass$new()
  set.seed(1)
  m$simulate()
  obj <- comp$nf$new()
  obj$m <- m
  varRangeList <- list(
    varRangeClass$new("y[2:4]"),
    varRangeClass$new("z[2:4, 3:5]")
  )
  multCopy <- makeMultiCopier(m, varRangeList)
  obj$multCopy <- multCopy
  obj$init()
  val <- c(m$y[2:4], as.numeric(m$z[2:4, 3:5]))
  expect_equal(obj$get(), val)
  val2 <- rnorm(length(val))
  obj$set(val2)
  check2 <- c(m$y[2:4], as.numeric(m$z[2:4, 3:5]))
  expect_equal(val2, check2)
})
