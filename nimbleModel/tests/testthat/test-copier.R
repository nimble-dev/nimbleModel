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
          cppLiteral("multCopy->setValues_() = v;")
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

# values(model, nodes[i], mode = "raw" or "scalar"): one entry of nodes, or one flattened element.
test_that("multiCopier getValues_indexed and setValues_indexed work, compiled", {
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
  inits <- list(tau = 25, mu = 0, z = matrix(rnorm(25), nrow = 5))
  data <- list(y = rnorm(5))
  mclass <- nimbleModel(code, inits = inits, data = data, returnClass = TRUE)

  nf <- nClass(
    Cpublic = list(
      m = "nimbleModel:::modelBase_nClass()",
      multCopy = "nimbleModel:::multiCopier_nClass()",
      init = nFunction(function() {
        multCopy$init(m)
      }),
      get = nFunction(function() {
        return(multCopy$getValues())
      }, returnType = "numericVector"),
      get_i = nFunction(function(i = "integerScalar", mode = "string") {
        return(multCopy$getValues_indexed(i, mode))
      }, returnType = "numericVector"),
      set_i = nFunction(function(i = "integerScalar", v = "numericVector", mode = "string") {
        multCopy$setValues_indexed(i, v, mode)
      }),
      set_raw_scalar = nFunction(function(i = "integerScalar", x = "numericScalar") {
        # A scalar into a length-1 entry, as values(model, nodes[i]) <- x would do.
        cppLiteral("multCopy->setValues_raw_(i) = x;")
      })
    )
  )
  comp <- nCompile(mclass, nf)
  m <- comp$mclass$new()
  set.seed(1)
  m$simulate()
  m$calculate()
  obj <- comp$nf$new()
  obj$m <- m
  # Includes a whole scalar variable, a duplicated (overlapping) entry, and a logProb variable.
  nodes <- c("y[2:4]", "z[2:4, 3:5]", "mu", "y[3]", "logProb_y[1:2]")
  obj$multCopy <- makeMultiCopier(m, nodes)
  obj$init()

  entries <- function() {
    list(m$y[2:4], as.numeric(m$z[2:4, 3:5]), m$mu, m$y[3], m$logProb_y[1:2])
  }
  vals <- entries()
  for (k in seq_along(nodes)) {
    expect_equal(obj$get_i(k, "raw"), vals[[k]], info = nodes[k])
  }
  flat <- unlist(vals)
  expect_equal(obj$get(), flat)
  for (k in seq_along(flat)) {
    expect_equal(obj$get_i(k, "scalar"), flat[k], info = k)
  }

  # raw set, including into the duplicated entry
  new_z <- rnorm(9)
  obj$set_i(2L, new_z, "raw")
  expect_equal(as.numeric(m$z[2:4, 3:5]), new_z)
  obj$set_i(4L, 10.5, "raw")
  expect_equal(m$y[3], 10.5)
  obj$set_raw_scalar(3L, -2.5)
  expect_equal(m$mu, -2.5)

  # scalar set: element 5 is z[3, 3]; element 13 is mu
  obj$set_i(5L, 7.25, "scalar")
  expect_equal(m$z[3, 3], 7.25)
  obj$set_i(13L, 3.5, "scalar")
  expect_equal(m$mu, 3.5)

  # errors for bad indices, sizes, and modes
  expect_error(obj$get_i(0L, "raw"))
  expect_error(obj$get_i(6L, "raw"))
  expect_error(obj$get_i(length(flat) + 1L, "scalar"))
  expect_error(obj$set_i(1L, c(1, 2), "raw"))
  expect_error(obj$set_i(1L, c(1, 2), "scalar"))
  expect_error(obj$set_raw_scalar(1L, 1))
  expect_error(obj$get_i(1L, "node"))
})

test_that("uncompiled values(model, nodes[i], mode = ...) and values_indexed work", {
  code <- quote({
    mu ~ dnorm(0, 1)
    for (i in 1:5) {
      y[i] ~ dnorm(mu, 1)
    }
    for (i in 1:5) {
      for (j in 1:5) {
        z[i, j] ~ dnorm(mu, 1)
      }
    }
  })
  m <- nimbleModel(code, inits = list(mu = 1, y = rnorm(5), z = matrix(rnorm(25), nrow = 5)))
  nodes <- c("y[2:4]", "z[2:4, 3:5]", "mu")
  flat <- c(m$y[2:4], as.numeric(m$z[2:4, 3:5]), m$mu)

  # raw is the default and is ordinary R indexing of nodes
  expect_equal(values(m, nodes[2]), as.numeric(m$z[2:4, 3:5]))
  expect_equal(values_indexed(m, nodes, 2, "raw"), as.numeric(m$z[2:4, 3:5]))
  # nodes as a list of varRanges
  vr <- lapply(nodes, \(x) varRangeClass$new(x))
  expect_equal(values(m, vr[3]), m$mu)
  for (k in seq_along(flat)) {
    expect_equal(values(m, nodes[k], mode = "scalar"), flat[k], info = k)
  }
  i <- 4
  expect_equal(values(m, nodes[i + 1], mode = "scalar"), flat[5])

  values(m, nodes[3]) <- 2.5 # whole-variable entry
  expect_equal(m$mu, 2.5)
  values(m, nodes[1], mode = "raw") <- c(1, 2, 3)
  expect_equal(m$y[2:4], c(1, 2, 3))
  values(m, nodes[5], mode = "scalar") <- 7.25 # z[3, 3]
  expect_equal(m$z[3, 3], 7.25)
  values_indexed(m, vr, 13, "scalar") <- -1
  expect_equal(m$mu, -1)

  expect_error(values(m, nodes, mode = "scalar"))
  expect_error(values(m, nodes[length(flat) + 1], mode = "scalar"))
  expect_error(values_indexed(m, nodes, 1, "raw") <- 1)
  expect_error(values_indexed(m, nodes, 1, "node"))
})
