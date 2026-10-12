# library(nCompiler)
# library(nimbleModel)

message("Add scalar elements to all modelValues testing cases.")

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
  default_mu3 <- rep(list(numeric(2)), 3)
  default_cov3 <- matrix(0, nrow = 3, ncol = 4) |> list() |> rep(3)
  expect_equal(obj$mu |> as.list(), default_mu3)
  expect_equal(obj$cov |> as.list(), default_cov3)

  legacy_setting <- getNimbleModelOption("legacy_nimble")
  on.exit(setNimbleModelOption("legacy_nimble", legacy_setting))

  # S3 interface, uncompiled
  expect_equal(length(obj), 3)
  # Access for var and row
  expect_equal(obj["mu", 1], c(0, 0))
  # Access all rows for one var
  setNimbleModelOption("legacy_nimble", FALSE)
  expect_equal(obj["mu"] |> as.list(), default_mu3)
  # With legacy_nimble TRUE, `[.modelValues` returns a list instead of an nList
  setNimbleModelOption("legacy_nimble", TRUE)
  expect_equal(obj["mu"], default_mu3)
  setNimbleModelOption("legacy_nimble", FALSE)
  # Set all rows for one var
  testList <- list(c(1, 2), c(3, 4))
  cat("expected warning about wrong number of items to replace:\n")
  expect_warning(obj["mu"] <- testList) # Warning for wrong length
  test_muList <- list(c(1, 2), c(3, 4), c(5, 6))
  obj["mu"] <- test_muList
  expect_equal(obj["mu"] |> as.list(), test_muList)
  obj["mu"] <- default_mu3
  # Set for var and row
  obj["mu", 1] <- c(1, 2)
  expect_equal(obj["mu", 1], c(1, 2))

  # Change length using `length<-`, checking for preserved values.
  obj["mu"] <- test_muList
  length(obj) <- 2
  expect_equal(length(obj), 2)
  list_exp2 <- list(mu = test_muList[1:2], cov = default_cov3[1:2])
  list_exp2_stacked <- lapply(list_exp2, simplify2array)
  # list_exp2 <- list_exp2[sort(names(list_exp2))]
  expect_equal(as.list(obj, stack=FALSE), list_exp2)
  expect_equal(as.list(obj, iterationAsLastIndex = TRUE), list_exp2_stacked)
  length(obj) <- 3
  list_exp3 <- list(mu = c(list_exp2$mu, default_mu3[3]), cov = c(list_exp2$cov, default_cov3[3]))
  list_exp3_stacked <- lapply(list_exp3, simplify2array)
  #list_exp3 <- list_exp3[sort(names(list_exp3))]
  expect_equal(as.list(obj, stack=FALSE), list_exp3)
  expect_equal(as.list(obj, iterationAsLastIndex = TRUE), list_exp3_stacked)

  # Change length using `resize`, checking for preserved values.
  resize(obj, 2)
  expect_equal(getsize(obj), 2)
  expect_equal(obj$getSize(), 2)
  expect_equal(as.list(obj, stack=FALSE), list_exp2)
  resize(obj, 3)
  expect_equal(as.list(obj, stack=FALSE), list_exp3)
  expect_equal(obj$getSize(), 3)

  # Test compiling as a type
  # resize and getsize are supported "natively" by opDefs.
  # single and double bracket operators for now are
  # supported for nimble2 via its keyword processing system
  mvHolder <- nCompiler::nClass(
    Cpublic = list(
      mv = "nimbleModel:::modelValuesBase_nClass()",
      check1 = nCompiler::nFunction(
        function(m = 'integerScalar') {
          resize(mv, m)
        }
      ),
      check2 = nCompiler::nFunction(
        function() {
          return(getsize(mv))
          returnType(integerScalar())
        }
      )
    )
  )
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
  default_mu3 <- rep(list(numeric(2)), 3)
  default_cov3 <- matrix(0, nrow = 3, ncol = 4) |> list() |> rep(3)
  expect_equal(obj$mu |> as.list(), default_mu3)
  expect_equal(obj$cov |> as.list(), default_cov3)

  # S3 interface, compiled
  expect_equal(length(obj), 3)
  # Access for var and row
  expect_equal(obj["mu", 1], c(0, 0))
  setNimbleModelOption("legacy_nimble", FALSE)
  expect_equal(obj["mu"] |> as.list(), default_mu3)
  # With legacy_nimble TRUE, `[.modelValues` returns a list instead of an nList
  setNimbleModelOption("legacy_nimble", TRUE)
  expect_equal(obj["mu"], default_mu3)
  setNimbleModelOption("legacy_nimble", FALSE)
  # Set all rows for one var
  testList <- list(c(1, 2), c(3, 4))
  cat("expected warning about wrong number of items to replace:\n")
  expect_warning(obj["mu"] <- testList) # Warning for wrong length
  test_muList <- list(c(1, 2), c(3, 4), c(5, 6))
  obj["mu"] <- test_muList
  expect_equal(obj["mu"] |> as.list(), test_muList)
  obj["mu"] <- default_mu3
  # Set for var and row
  obj["mu", 1] <- c(1, 2)
  expect_equal(obj["mu", 1], c(1, 2))

  # Change length using `length<-`, checking for preserved values.
  obj["mu"] <- test_muList
  length(obj) <- 2
  expect_equal(length(obj), 2)
  list_exp2 <- list(mu = test_muList[1:2], cov = default_cov3[1:2])
  list_exp2_stacked <- lapply(list_exp2, simplify2array)
  # list_exp2 <- list_exp2[sort(names(list_exp2))]
  expect_equal(as.list(obj, stack=FALSE), list_exp2)
  expect_equal(as.list(obj, iterationAsLastIndex = TRUE), list_exp2_stacked)
  length(obj) <- 3
  list_exp3 <- list(mu = c(list_exp2$mu, default_mu3[3]), cov = c(list_exp2$cov, default_cov3[3]))
  #list_exp3 <- list_exp3[sort(names(list_exp3))]
  expect_equal(as.list(obj, stack=FALSE), list_exp3)

  # Change length using `resize`, checking for preserved values.
  resize(obj, 2)
  expect_equal(getsize(obj), 2)
  expect_equal(obj$getSize(), 2)
  expect_equal(as.list(obj, stack=FALSE), list_exp2)
  resize(obj, 3)
  expect_equal(as.list(obj, stack=FALSE), list_exp3)
  expect_equal(obj$getSize(), 3)

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
  expect_equal(nCompiler::value(obj, "mu") |> as.list(), default_mu3)
  expect_equal(nCompiler::value(obj, "cov") |> as.list(), default_cov3)

  # In S3 interface use, normal syntax should work
  # because the S3 interface functions use value() and method()
  expect_equal(length(obj), 3)
  # Access for var and row
  expect_equal(obj["mu", 1], c(0, 0))
  # Access all rows for one var
  setNimbleModelOption("legacy_nimble", FALSE)
  expect_equal(obj["mu"] |> as.list(), default_mu3)
  # With legacy_nimble TRUE, `[.modelValues` returns a list instead of an nList
  setNimbleModelOption("legacy_nimble", TRUE)
  expect_equal(obj["mu"], default_mu3)
  setNimbleModelOption("legacy_nimble", FALSE)
  # Set all rows for one var
  testList <- list(c(1, 2), c(3, 4))
  cat("expected warning about wrong number of items to replace:\n")
  expect_warning(obj["mu"] <- testList) # Warning for wrong length
  test_muList <- list(c(1, 2), c(3, 4), c(5, 6))
  obj["mu"] <- test_muList
  expect_equal(obj["mu"] |> as.list(), test_muList)
  obj["mu"] <- default_mu3
  # Set for var and row
  obj["mu", 1] <- c(1, 2)
  expect_equal(obj["mu", 1], c(1, 2))

  # Change length using `length<-`, checking for preserved values.
  obj["mu"] <- test_muList
  length(obj) <- 2
  expect_equal(length(obj), 2)
  list_exp2 <- list(mu = test_muList[1:2], cov = default_cov3[1:2])
  #list_exp2 <- list_exp2[sort(names(list_exp2))]
  expect_equal(as.list(obj, stack=FALSE), list_exp2)
  length(obj) <- 3
  list_exp3 <- list(mu = c(list_exp2$mu, default_mu3[3]), cov = c(list_exp2$cov, default_cov3[3]))
  # list_exp3 <- list_exp3[sort(names(list_exp3))]
  expect_equal(as.list(obj, stack=FALSE), list_exp3)

  # Change length using `resize`, checking for preserved values.
  resize(obj, 2)
  expect_equal(getsize(obj), 2)
  expect_equal(obj$getSize(), 2)
  expect_equal(as.list(obj, stack=FALSE), list_exp2)
  resize(obj, 3)
  expect_equal(as.list(obj, stack=FALSE), list_exp3)
  expect_equal(obj$getSize(), 3)

  # Change length using compiled `resize`, check with compiled getsize, checking for preserved values.
  mvh$check1(2)
  expect_equal(getsize(obj), 2)
  expect_equal(obj$getSize(), 2)
  expect_equal(mvh$check2(), 2)
  expect_equal(as.list(obj, stack=FALSE), list_exp2)
  mvh$check1(3)
  expect_equal(as.list(obj, stack=FALSE), list_exp3)
  expect_equal(obj$getSize(), 3)
  expect_equal(mvh$check2(), 3)

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

test_that("modelValuesConf works", {
  mvc <- modelValuesConf(vars = c('d1'),
                         types = c('double'),
                         sizes = list( d1 = 5 ))
  mv <- mvc(2)
  expect_equal(length(mv), 2)
  expect_equal(mv["d1", 1], rep(0, 5))
  mvcB <- modelValuesConf(vars = c('d1'),
                          types = c('double'),
                          sizes = list( d1 = 5 ))
  mvB <- mvcB(2)
  # two versions that should be identical end up so.
  expect_identical(class(mv), class(mvB))

  mvc <- modelValuesConf(vars = c('d1', 'd2', 'd3'),
                         types = c('double', 'double', 'double'),
                         sizes = list( d1 = 5, d2 = c(3, 5), d3 = c(3, 5, 7)))
  mv <- mvc(3)
  expect_identical(mv["d2", 2], matrix(0, nrow = 3, ncol = 5))
  expect_identical(mv["d3", 3], array(0, dim = c(3, 5, 7)))
  rm(mv); gc()
})

test_that("as.matrix(modelValues) and matrix2MV work", {
  varInfo <- list(
    vars = list(
      eta = list(name = "eta", nDim = 0),
      mu = list(name = "mu", nDim = 1),
      cov = list(name = "cov", nDim = 2)
    )
  )

  mvClass <- nimbleModel:::make_modelValues_nClass(varInfo)
  obj <- mvClass$new()
  sizes <- list(mu = 2, cov = c(3, 4))
  obj$set_sizeList(sizes)

  # Make an uncompiled object,
  # convert it to a matrix with as.matrix()
  # and invert that back to a new uncompiled object
  obj$resize(0)
  obj$resize(3)
  obj$mu[[2]] <- 5:6
  obj$mu[[3]] <- 10:11
  obj$cov[[2]] <- matrix(1:12, nrow = 3)
  obj$cov[[3]] <- obj$cov[[2]] + 100
  mat <- obj |> as.matrix()
  obj2 <- mvClass$new()

  junk <- nimbleModel:::matrix2mv(mat, obj2)
  expect_identical(junk, obj2)
  expect_identical(mat, obj2 |> as.matrix())
  expect_equal(obj$mu |> as.list(), obj2$mu |> as.list())

  # provide a way to get base class pointers
  # since this is how we'll want many algos to
  # use modelValues
  make_mvBase <- nCompiler::nFunction(
    function(x = "nimbleModel:::modelValuesBase_nClass()") {
      return(x)
      returnType("nimbleModel:::modelValuesBase_nClass()")
    }
  )

  comp <- nCompiler::nCompile(mvClass, make_mvBase, returnList = TRUE)

  # Check that the compiled, specialized modelValues type works
  # as above for uncompiled.
  Cobj <- comp$mvClass$new()
  Cobj$set_sizeList(sizes)
  Cobj$resize(0)
  Cobj$resize(3)
  Cobj$mu[[2]] <- 5:6
  Cobj$mu[[3]] <- 10:11
  Cobj$cov[[2]] <- matrix(1:12, nrow = 3)
  Cobj$cov[[3]] <- obj$cov[[2]] + 100
  Cmat <- Cobj |> as.matrix()
  expect_identical(mat, Cmat)
  # to compiled
  Cobj2 <- comp$mvClass$new()
  Cjunk <- nimbleModel:::matrix2mv(Cmat, Cobj2)
  expect_identical(Cjunk, Cobj2)
  expect_identical(Cmat, Cobj2 |> as.matrix())
  expect_equal(Cobj$mu |> as.list(), Cobj2$mu |> as.list())
  # to uncompiled
  length(obj2) <- 0
  junk <- nimbleModel:::matrix2mv(Cmat, obj2)
  expect_identical(junk, obj2)
  expect_identical(Cmat, obj2 |> as.matrix())
  expect_equal(Cobj$mu |> as.list(), obj2$mu |> as.list())


  # Check that use via base class works
  CbaseObj <- comp$make_mvBase(Cobj)
  expect_true(inherits(Cobj, "modelValues"))
  expect_true(inherits(CbaseObj, "modelValues"))
  expect_false(inherits(CbaseObj, class(Cobj)[1]))
  expect_identical(nCompiler::value(CbaseObj, "mu") |> as.list(), Cobj$mu |> as.list())
  CbaseMat <- CbaseObj |> as.matrix()
  expect_identical(CbaseMat, Cmat)
  Cobj_temp <- comp$mvClass$new()
  CbaseObj2 <- comp$make_mvBase(Cobj_temp)
  CbaseJunk <- nimbleModel:::matrix2mv(CbaseMat, CbaseObj2)
  expect_identical(CbaseJunk, CbaseObj2)
  expect_identical(Cmat, CbaseObj2 |> as.matrix())
  expect_equal(CbaseObj$mu |> as.list(), CbaseObj2$mu |> as.list())

  rm(Cobj, Cobj2, Cjunk, CbaseObj, CbaseObj2, Cobj_temp); gc()
})
