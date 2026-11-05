library(nimbleModel)
library(testthat)
library(nCompiler)


# Tests of numbering the nodes (steps) of instructions and instruction lists:
# `set_instr_idx` must give, for every step k, the k-th idx visited by the full runners,
# and the one-step (`*_indexed`, and through them `*_singleton`) entry points must act on exactly that node.

# Build an instruction (in its R-list form) directly from its indexing.
# values: start for a seq range; for a mat/matp range, the counter values, each counter's dims[r] values together.
make_test_instr <- function(index_types, dims, lens, values, slots = seq_len(sum(dims))) {
  instr <- list(
    lens = lens, index_types = index_types, nDim = sum(dims), dims = dims,
    slots = as.numeric(slots), values = values, declID = 1, sortID = 1
  )
  instr <- nimbleModel:::add_instr_precalcs(instr)
  class(instr) <- "Rinstr"
  instr
}

# Run the full runner for `instr` with a per-node function that records each idx it receives.
record_runner_idx <- function(instr) {
  seen <- list()
  nimbleModel:::run_instr(instr, function(idx) {
    seen[[length(seen) + 1]] <<- as.numeric(idx)
    0
  })
  seen
}

test_instrs <- list(
  "0" = list(lens = 1, index_types = 0, nDim = 0) |> nimbleModel:::add_instr_precalcs() |> structure(class = "Rinstr"),
  "1" = make_test_instr(1, 1, 4, list(3)),
  "2" = make_test_instr(2, 1, 3, list(c(5, 2, 7))),
  "3" = make_test_instr(2, 2, 3, list(c(1, 4, 2, 5, 3, 6))),
  "18" = make_test_instr(2, 2, 3, list(c(1, 4, 2, 5, 3, 6)), slots = c(2, 1)),
  "4" = make_test_instr(c(1, 1), c(1, 1), c(3, 4), list(2, 5)),
  "5" = make_test_instr(c(1, 2), c(1, 1), c(3, 2), list(2, c(9, 4))),
  "6" = make_test_instr(c(2, 1), c(1, 1), c(2, 3), list(c(9, 4), 2)),
  "7" = make_test_instr(c(2, 2), c(1, 1), c(2, 3), list(c(9, 4), c(1, 8, 3))),
  "8" = make_test_instr(c(1, 2), c(1, 2), c(3, 2), list(2, c(1, 4, 2, 5))),
  "9" = make_test_instr(c(2, 1), c(2, 1), c(2, 3), list(c(1, 4, 2, 5), 2)),
  "10" = make_test_instr(c(2, 2), c(2, 2), c(2, 2), list(c(1, 4, 2, 5), c(7, 8, 9, 6)), slots = c(3, 1, 4, 2)),
  "11" = make_test_instr(c(2, 1), c(1, 1), c(2, 3), list(c(9, 4), 2), slots = c(2, 1)),
  "12" = make_test_instr(c(1, 1, 1), c(1, 1, 1), c(2, 3, 2), list(1, 4, 2)),
  "13" = make_test_instr(c(1, 2, 2), c(1, 2, 1), c(2, 2, 3), list(3, c(1, 4, 2, 5), c(7, 9, 8)), slots = c(3, 1, 4, 2)),
  "14" = make_test_instr(rep(1, 4), rep(1, 4), c(2, 2, 3, 2), list(1, 2, 3, 4)),
  "15" = make_test_instr(c(2, 1, 2, 1), c(1, 1, 2, 1), c(2, 2, 2, 3), list(c(5, 3), 2, c(1, 4, 2, 5), 6), slots = c(5, 2, 1, 3, 4)),
  "16" = make_test_instr(rep(1, 5), rep(1, 5), c(2, 2, 2, 2, 2), list(1, 2, 3, 4, 5)),
  "17" = make_test_instr(c(1, 2, 1, 2, 1), c(1, 1, 1, 2, 1), c(2, 2, 2, 2, 3), list(1, c(6, 3), 2, c(1, 4, 2, 5), 7), slots = c(6, 4, 5, 1, 2, 3))
)

test_that("test instructions cover every instruction type", {
  types <- sapply(test_instrs, \(x) x$instr_type)
  expect_identical(as.character(types), names(test_instrs))
  expect_setequal(types, unlist(nimbleModel:::type2itype) |> unname())
})

test_that("set_instr_idx gives the k-th idx of the full runner for every instruction type", {
  for (type in names(test_instrs)) {
    instr <- test_instrs[[type]]
    full <- record_runner_idx(instr)
    expect_identical(length(full), as.integer(instr$nNodes), info = paste("type", type))
    for (k in seq_along(full)) {
      expect_identical(nimbleModel:::set_instr_idx(instr, k), full[[k]], info = paste("type", type, "step", k))
    }
  }
})

test_that("list steps map to the right instruction and step, including empty instructions", {
  empty <- make_test_instr(1, 1, 0, list(1))
  instrs <- list(test_instrs[["4"]], empty, test_instrs[["1"]], empty, empty, test_instrs[["2"]])
  instrList <- nimbleModel:::make_RinstrList(instrs)
  expect_equal(instrList$offsets, c(0, 12, 12, 16, 16, 16))
  expect_equal(instrList$nNodes_total, 19)
  expected <- list()
  for (j in seq_along(instrs)) {
    for (k in seq_len(instrs[[j]]$nNodes)) expected[[length(expected) + 1]] <- list(j = j, k = k)
  }
  for (step in 1:19) {
    expect_equal(nimbleModel:::instrList_find_singleton(instrList, step), expected[[step]], info = paste("step", step))
    loc <- expected[[step]]
    expect_identical(
      nimbleModel:::set_instr_idx_from_instrList(instrList, step),
      nimbleModel:::set_instr_idx(instrs[[loc$j]], loc$k)
    )
  }
  expect_error(nimbleModel:::instrList_find_singleton(instrList, 0), "outside")
  expect_error(nimbleModel:::instrList_find_singleton(instrList, 20), "outside")
})

# For each step of an instruction list, check that the one-step methods act on exactly the
# node of `z` that `set_instr_idx` names, in model `m` (compiled or not).
check_indexed_steps <- function(m, nodes) {
  instrList <- makeInstrList(m, nodes)
  n <- instrList$nNodes_total
  for (step in seq_len(n)) {
    idx <- nimbleModel:::set_instr_idx_from_instrList(instrList, step)
    m$z <- array(0, dim(m$z))
    m$calculate_indexed(instrList, step)
    changed <- which(m$z != 0, arr.ind = TRUE)
    expect_identical(nrow(changed), 1L, info = paste("step", step))
    expect_equal(as.numeric(changed), idx, info = paste("step", step))
    # idx holds 1-based (R) index values, used both to index z and in z's expression.
    expect_equal(m$z[changed], idx[1] * 10 + idx[2], info = paste("step", step))
  }
  # Running every step in turn gives the same result as a full calculate.
  m$z <- array(0, dim(m$z))
  m$calculate(instrList)
  full <- m$z
  m$z <- array(0, dim(m$z))
  for (step in seq_len(n)) m$calculate_indexed(instrList, step)
  expect_equal(m$z, full)
}

test_that("calculate_indexed acts on the node set_instr_idx names, uncompiled and compiled", {
  code <- nimbleCode({
    for (i in 1:4) {
      for (j in 1:3) {
        z[i, j] <- i * 10 + j # ensure that i and j are used in R-based indexing on RHS, even when compiled.
      }
    }
  })
  m <- nimbleModel(code)
  cm <- nimbleModel(code, compile = TRUE)
  for (model in list(m, cm)) {
    check_indexed_steps(model, "z") # 2_seq_seq
    check_indexed_steps(model, "z[2:3, 2]")
    check_indexed_steps(model, "z[c(4, 1), 2:3]") # mat outer range
    check_indexed_steps(model, "z[2:4, c(3, 1)]") # mat inner range
  }
  expect_error(cm$calculate_indexed("z", 13), "outside")
})

test_that("getLogProb_indexed, getParam_indexed, getBound_indexed and simulate_indexed act on the k-th node", {
  code <- nimbleCode({
    for (i in 1:3) {
      for (j in 1:2) {
        y[i, j] ~ dunif(lo[i, j], lo[i, j] + w[i, j])
      }
    }
  })
  lo <- matrix(1:6, 3, 2)
  w <- matrix(c(2, 4, 5, 8, 10, 20), 3, 2)
  y <- lo + 1
  m <- nimbleModel(code, data = list(lo = lo, w = w), inits = list(y = y))
  cm <- nimbleModel(code, data = list(lo = lo, w = w), inits = list(y = y), compile = TRUE)
  for (model in list(m, cm)) {
    model$calculate() # also calculates the lifted node for lo + w
    instrList <- makeInstrList(model, "y")
    for (step in seq_len(instrList$nNodes_total)) {
      idx <- nimbleModel:::set_instr_idx_from_instrList(instrList, step)
      i <- idx[1]
      j <- idx[2]
      expect_equal(model$getParam_indexed(instrList, step, "min"), lo[i, j])
      expect_equal(model$getBound_indexed(instrList, step, "upper"), lo[i, j] + w[i, j])
      expect_equal(model$getLogProb_indexed(instrList, step), -log(w[i, j]))
      before <- model$y
      model$simulate_indexed(instrList, step)
      changed <- which(model$y != before, arr.ind = TRUE)
      expect_identical(nrow(changed), 1L)
      expect_equal(as.numeric(changed), c(i, j))
    }
  }
})

# Until compile-time handling maps `model$calculate_indexed(nodes, i)` and its siblings to the
# `*_indexed_impl` methods, compiled code calls those methods directly. This checks that the new
# virtual methods and `instrList_nClass` work from generated C++. (The uncompiled `*_impl`
# methods are stubs, so this is compiled only.)
test_that("compiled code can call the *_indexed_impl methods with an instrList_nClass", {
  code <- nimbleCode({
    for (i in 1:3) {
      for (j in 1:2) {
        y[i, j] ~ dunif(lo[i, j], lo[i, j] + w[i, j])
      }
    }
  })
  lo <- matrix(1:6, 3, 2)
  w <- matrix(c(2, 4, 5, 8, 10, 20), 3, 2)
  y <- lo + 1
  mclass <- nimbleModel(code, data = list(lo = lo, w = w), inits = list(y = y), returnClass = TRUE)
  indexedCaller <- nClass(
    classname = "indexedCaller",
    Cpublic = list(
      model = "nimbleModel:::modelBase_nClass()",
      nodes = "nimbleModel:::instrList_nClass()",
      calc = nFunction(
        function(i = "integerScalar") {
          return(model$calculate_indexed_impl(nodes, i))
        },
        returnType = "numericScalar"
      ),
      calcDiff = nFunction(
        function(i = "integerScalar") {
          return(model$calculateDiff_indexed_impl(nodes, i))
        },
        returnType = "numericScalar"
      ),
      logProb = nFunction(
        function(i = "integerScalar") {
          return(model$getLogProb_indexed_impl(nodes, i))
        },
        returnType = "numericScalar"
      ),
      sim = nFunction(
        function(i = "integerScalar") {
          model$simulate_indexed_impl(nodes, i)
        },
        returnType = "void"
      ),
      param = nFunction(
        function(i = "integerScalar", paramID = "integerScalar") {
          return(model$getParam_indexed_impl(nodes, i, paramID))
        },
        returnType = "ETaccessor()"
      ),
      bound = nFunction(
        function(i = "integerScalar", boundID = "integerScalar") {
          return(model$getBound_indexed_impl(nodes, i, boundID))
        },
        returnType = "numericScalar"
      )
    )
  )
  comp <- nCompile(mclass, indexedCaller)
  cm <- comp$mclass$new()
  caller <- comp$indexedCaller$new()
  caller$model <- cm
  instrList <- makeInstrList(cm, "y")
  caller$nodes <- cm$makeCompiledInstrList(instrList)
  minID <- cm$getParamID(instrList, "min")
  cm$calculate()
  for (step in seq_len(instrList$nNodes_total)) {
    idx <- nimbleModel:::set_instr_idx_from_instrList(instrList, step)
    i <- idx[1]
    j <- idx[2]
    expect_equal(caller$calc(step), -log(w[i, j]))
    expect_equal(caller$logProb(step), -log(w[i, j]))
    expect_equal(caller$calcDiff(step), 0)
    expect_equal(caller$param(step, minID), lo[i, j])
    expect_equal(caller$bound(step, 2L), lo[i, j] + w[i, j]) # boundID 2 is the upper bound
    before <- cm$y
    caller$sim(step)
    changed <- which(cm$y != before, arr.ind = TRUE)
    expect_identical(nrow(changed), 1L)
    expect_equal(as.numeric(changed), c(i, j))
  }
  expect_error(caller$calc(7L), "outside")
})
