#library(nCompiler)
#library(nimbleModel)

test_that("basic modelValues class works", {
  varInfo <- list(
    vars = list(
      mu = list(name = "mu", nDim = 1),
      cov =list(name = "cov", nDim = 2)
    )
  )
  mvClass <- nimbleModel:::make_modelValues_nClass(varInfo)
  CmvClass <- nCompile(mvClass)
  obj <- CmvClass$new()
  expect_equal(obj$mu |> as.list(), list())
  sizes <- list(mu = 2, cov = c(3, 4))
  obj$sizes <- sizes
  expect_equal(obj$sizes, sizes)
  obj$resize(3)
  expect_equal(obj$mu |> as.list(), rep(list(numeric(2)), 3))
  expect_equal(obj$cov |> as.list(), matrix(0, nrow = 3, ncol = 4) |> list() |> rep(3))
})
