# library(nCompiler)
# library(nimbleModel)

test_that("basic modelValues class works", {
  varInfo <- list(
    vars = list(
      mu = list(name = "mu", nDim = 1),
      cov = list(name = "cov", nDim = 2)
    )
  )
  mvClass <- nimbleModel:::make_modelValues_nClass(varInfo)

  obj <- mvClass$new()

  # Basic uncompiled uses:
  expect_equal(obj$mu |> as.list(), list())
  sizes <- list(mu = 2, cov = c(3, 4))
  obj$set_sizeList(sizes)
  expect_equal(obj$get_sizeList(), sizes)
  obj$resize(3)
  expect_equal(obj$mu |> as.list(), rep(list(numeric(2)), 3))
  expect_equal(obj$cov |> as.list(), matrix(0, nrow = 3, ncol = 4) |> list() |> rep(3))

  # S3 interface, uncompiled
  expect_equal(length(obj), 3)
  expect_equal(obj["mu", 1], c(0, 0))
  obj["mu", 1] <- c(1, 2)
  expect_equal(obj["mu", 1], c(1, 2))
  length(obj) <- 2
  expect_equal(length(obj), 2)
  obj["mu", 1] <- c(1, 2)
  obj["mu", 2] <- c(3, 4)
  list_exp <- list(mu = list(c(1,2), c(3, 4)), cov = matrix(0, nrow = 3, ncol = 4) |> list() |> rep(2))
  expect_equal(as.list(obj), list_exp)

  # Make a holder class so we can test a compiled mv returned
  # as a base class

  mvHolder <- nCompiler::nClass(
    Cpublic = list(
      mv = "modelValuesBase_nClass"
    )
  )

#  debug(nCompiler:::compile_generateCpp)
#  debug(nCompiler:::process_inheritance)
  comp <- nCompiler::nCompile(mvClass, mvHolder)
  CmvClass <- comp$mvClass
  CmvHolder <- comp$mvHolder
  obj <- CmvClass$new()

  # Basic compiled uses
  expect_equal(obj$mu |> as.list(), list())
  sizes <- list(mu = 2, cov = c(3, 4))
  obj$set_sizeList(sizes)
  expect_equal(obj$get_sizeList(), sizes)
  obj$resize(3)
  expect_equal(obj$mu |> as.list(), rep(list(numeric(2)), 3))
  expect_equal(obj$cov |> as.list(), matrix(0, nrow = 3, ncol = 4) |> list() |> rep(3))

  # S3 interface, compiled
  expect_equal(length(obj), 3)
  expect_equal(obj["mu", 1], c(0, 0))
  obj["mu", 1] <- c(1, 2)
  expect_equal(obj["mu", 1], c(1, 2))
  length(obj) <- 2
  expect_equal(length(obj), 2)
  obj["mu", 1] <- c(1, 2)
  obj["mu", 2] <- c(3, 4)
  list_exp <- list(mu = list(c(1,2), c(3, 4)), cov = matrix(0, nrow = 3, ncol = 4) |> list() |> rep(2))
  expect_equal(as.list(obj), list_exp[sort(names(list_exp)) ] )

  # Accessed by base class pointer as a member of another class
  obj <- CmvClass$new()
  mvh <- CmvHolder$new()
  mvh$mv <- obj # assign the derived obj to be held by base class pointer
  obj <- mvh$mv # obj is now a base class pointer

  # In direct use, we need to use value and method because the base class
  # interface does not have active bindings for the derived content like the specific variables
  expect_true(is.null(obj$mu))
  expect_equal(nCompiler::value(obj, "mu") |> as.list(), list())
  sizes <- list(mu = 2, cov = c(3, 4))
  nCompiler::method(obj, "set_sizeList")(sizes)
  expect_equal(nCompiler::method(obj, "get_sizeList")(), sizes)
  nCompiler::method(obj, "resize")(3)
  expect_equal(nCompiler::value(obj, "mu") |> as.list(), rep(list(numeric(2)), 3))
  expect_equal(nCompiler::value(obj, "cov") |> as.list(), matrix(0, nrow = 3, ncol = 4) |> list() |> rep(3))

  # In S3 interface use, normal syntax should work
  # because the S3 interface functions use value() and method()
  expect_equal(length(obj), 3)
  expect_equal(obj["mu", 1], c(0, 0))
  obj["mu", 1] <- c(1, 2)
  expect_equal(obj["mu", 1], c(1, 2))
  length(obj) <- 2
  expect_equal(length(obj), 2)
  obj["mu", 1] <- c(1, 2)
  obj["mu", 2] <- c(3, 4)
  list_exp <- list(mu = list(c(1,2), c(3, 4)), cov = matrix(0, nrow = 3, ncol = 4) |> list() |> rep(2))
  expect_equal(as.list(obj), list_exp[sort(names(list_exp))])

  # We can create a new derived interface object with the same underlying C++ object
  # Then we have the active bindings
  obj_restored <- CmvClass$new(CppObj = obj)
  expect_equal(obj_restored[["mu"]][[1]], c(1, 2))
  expect_equal(obj_restored[["mu"]][[2]], c(3, 4))
  expect_equal(obj_restored$get_sizeList()$cov, c(3, 4))

  rm(obj, mvh, obj_restored); gc()
})

test_that("modelValues hashedID works and is invariant to equivalent cases", {
  suppressWarnings(rm(list = "varInfo"))
  VI1 <- list(
    vars = list(
      mu = list(name = "mu", nDim = 1),
      cov = list(name = "cov", nDim = 2)
    )
  )
  hash1 <- nimbleModel:::make_modelValues_hashID(VI1)
  VI2 <- VI1
  VI2$vars <- VI2$vars[c(2, 1)]
  hash2 <- nimbleModel:::make_modelValues_hashID(VI2)
  expect_equal(hash1, hash2)
  VI3 <- VI2
  VI3$vars <- VI3$vars |> lapply(\(x) list(x[[1]], x[[2]])) # remove names "name" and "nDim" in nested elements
  hash3 <- nimbleModel:::make_modelValues_hashID(VI3)
  expect_equal(hash2, hash3)
  VI4 <- VI1
  VI4$vars <- VI1$vars |> lapply(\(x) list(name_oops = x[[1]], nDim = x[[2]])) # create wrong names
  expect_error(
    nimbleModel:::make_modelValues_hashID(VI4)
  )

  VI5 <- VI1
  VI5$vars <- VI1$vars |> lapply(\(x) list(x[[2]], x[[1]])) # unnamed, with wrong types by order
  expect_error(
    nimbleModel:::make_modelValues_hashID(VI5)
  )

  VI6 <- VI1
  VI6$vars <- VI1$vars |> lapply(\(x) list(x[[1]], x[[2]], x[[1]])) # unnamed, with wrong number of arguments
  expect_error(
    nimbleModel:::make_modelValues_hashID(VI6)
  )

  VI7 <- VI1
  VI7$vars <- VI1$vars |> lapply(\(x) list(name = x[[1]], nDim = x[[2]], junk = x[[1]])) # named, with wrong number of arguments
  expect_error(
    nimbleModel:::make_modelValues_hashID(VI7)
  )
})

test_that("modelValues nClassBuilder types work in nCompiler", {
  varInfo <- list(
    vars = list(
      mu = list(name = "mu", nDim = 1),
      cov = list(name = "cov", nDim = 2)
    )
  )

  # example
  #nCompiler:::type2symbol("nCompiler:::nList('integerScalar()')") |>
  #  nCompiler:::resolveOneTBDsymbol()

  sym <- nCompiler:::type2symbol("nimbleModel:::modelValues(varInfo)") |>
    nCompiler:::resolveOneTBDsymbol()

  output <- capture_output(
    sym$print()
  )
  hashID <- nimbleModel:::make_modelValues_hashID(varInfo)
  expect_true(grepl(hashID, output))

  mvc <- nimbleModel:::modelValues(varInfo)
  nc <- nCompiler::nClass(
    Cpublic = list(
      mv = "nimbleModel:::modelValues(varInfo)",
      mvBase = "nimbleModel:::modelValuesBase_nClass()",
      init = nCompiler::nFunction(
        function() {
          mv <- mvc$new()
          mvBase <- mv
        }
      )
    )
  )

  comp <- nCompiler:::nCompile(nc, mvc)
  obj <- comp$nc$new()
  obj$init()
  sizes <- list(mu = 2, cov = c(3, 4))
  obj$mv$set_sizeList(sizes)
  expect_equal(obj$mv$get_sizeList(), sizes)

  sizes_alt <- list(mu = 4, cov = c(2, 1))
  obj$mvBase$set_sizeList(sizes_alt)
  expect_equal(obj$mv$get_sizeList(), sizes_alt)

  obj$mv$resize(3)
  expect_equal(obj$mv$cov |> as.list(), rep( list(matrix(0, nrow = 2, ncol = 1)), 3))
  expect_equal(nCompiler::value(obj$mv, "cov") |> as.list(), rep( list(matrix(0, nrow = 2, ncol = 1)), 3))
  expect_equal(obj$mv$getLength(), 3)
  expect_equal(obj$mvBase$getLength(), 3)

  obj$mvBase$resize(4)
  expect_equal(obj$mv$cov |> as.list(), rep( list(matrix(0, nrow = 2, ncol = 1)), 4))
  expect_equal(nCompiler::value(obj$mv, "cov") |> as.list(), rep( list(matrix(0, nrow = 2, ncol = 1)), 4))
  expect_equal(obj$mv$getLength(), 4)
  expect_equal(obj$mvBase$getLength(), 4)

  dup_mv_unc <- nimbleModel:::modelValues(varInfo)
  obj_unc <- dup_mv_unc$new()
  sizes2 <- list(mu = 3, cov = c(1, 2))
  obj_unc$set_sizeList(sizes2)
  obj_unc$resize(5)
  obj_unc$mu[[3]] <- 1:3
  obj$mv <- obj_unc
  # because we assigned an uncompiled to a compiled that already
  # had an instantiated object, it copied contents to the same
  # compiled object. So mvBase will see this too since it
  # still points to the same object.

  expect_true(obj$mv$isCompiled())
  expect_false(obj_unc$isCompiled())
  mu_exp <- rep(list(c(0,0,0)), 5)
  mu_exp[[3]] <- 1:3
  expect_equal(obj_unc$mu |> as.list(), mu_exp)
  expect_equal(obj$mv$mu |> as.list(), mu_exp)

  expect_equal(obj_unc$getLength(), 5)
  expect_equal(obj$mv$getLength(), 5)
  expect_equal(obj$mvBase$getLength(), 5)

  # new we will construct and assign a new compiled object
  # This will result in mv pointing to the new object
  # and mvBase pointing to the old object
  obj_new_comp <- comp$mvc$new()
  obj_new_comp$set_sizeList(list(mu = c(6), cov = c(4, 1)))
  obj_new_comp$resize(2)
  expect_equal(obj_new_comp$mu |> as.list(), rep(list(rep(0, 6)), 2))
  obj$mv <- obj_new_comp
  expect_equal(obj$mv$getLength(), 2)
  expect_equal(obj$mvBase$getLength(), 5)

  # Finally we will assign the derived to the base ptr
  # and they will point at the same thing again.
  obj$mvBase <- obj$mv
  expect_equal(obj$mvBase$getLength(), 2)
  rm(obj); gc()
})
