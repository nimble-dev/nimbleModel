library(nimbleModel)
library(testthat)
library(nCompiler)

# values(model, nodes[i], mode = "node"): the values of node i, in sorted order
# (as for calculate_indexed), flattened. This uses getValues_one/setValues_one in each declFun.

values_node_code <- nimbleCode({
  mu ~ dnorm(0, 1) # scalar, no loop index
  tau ~ dunif(0, 10)
  for (i in 1:3) {
    y[i] ~ dnorm(mu + x[i], tau = tau) # x is RHS-only; tau gives a lifted node
    z[i] <- 2 * y[i]
  }
  for (i in 1:2) {
    v[i, 1:3] <- m0[1:3] * i # one range and a fixed index (D = 1)
    W[i, 1:2, 1:2] <- B[1:2, 1:2] * i # two ranges and a fixed index (D = 2)
  }
})

make_values_node_inputs <- function() {
  list(
    data = list(x = c(0.1, 0.2, 0.3), m0 = c(1, 2, 3), B = matrix(1:4, 2)),
    inits = list(mu = 0.5, tau = 2, y = c(1, 2, 3))
  )
}

check_values_node <- function(m) {
  m$calculate()
  liftedNames <- grep("^lifted", m$getVarNames(), value = TRUE)
  expect_true(length(liftedNames) > 0)

  # get
  expect_equal(values_indexed(m, "mu", 1, "node"), m$mu)
  expect_equal(values(m, c("y[2:3]")[1], mode = "node"), m$y[2])
  nodes <- "y[2:3]"
  expect_equal(values(m, nodes[2], mode = "node"), m$y[3])
  expect_equal(values_indexed(m, "v", 2, "node"), m$v[2, ])
  expect_equal(values_indexed(m, "W", 1, "node"), as.numeric(m$W[1, , ]))
  expect_equal(values_indexed(m, "W", 2, "node"), as.numeric(m$W[2, , ]))
  for (ln in liftedNames) {
    expect_equal(values_indexed(m, ln, 1, "node"), m[[ln]][1], info = ln)
  }
  # a partial overlap gives the whole node
  expect_equal(values_indexed(m, "v[1, 2]", 1, "node"), m$v[1, ])
  # sorted order: mu comes before z
  expect_equal(values_indexed(m, c("z[1:2]", "mu"), 1, "node"), m$mu)
  expect_equal(values_indexed(m, c("z[1:2]", "mu"), 3, "node"), m$z[2])
  # varRange and instrList inputs
  expect_equal(values_indexed(m, varRangeClass$new("v[1:2, 1:3]"), 1, "node"), m$v[1, ])
  il <- makeInstrList(m, c("z[1:2]", "mu"))
  expect_equal(values_indexed(m, il, 2, "node"), m$z[1])

  # set
  values(m, nodes[1], mode = "node") <- 4.5
  expect_equal(m$y[2], 4.5)
  values_indexed(m, "mu", 1, "node") <- -1
  expect_equal(m$mu, -1)
  values_indexed(m, "v", 2, "node") <- c(7, 8, 9)
  expect_equal(m$v[2, ], c(7, 8, 9))
  expect_equal(m$v[1, ], c(1, 2, 3))
  newW <- c(11, 12, 13, 14)
  values_indexed(m, "W", 2, "node") <- newW
  expect_equal(as.numeric(m$W[2, , ]), newW)
  expect_equal(as.numeric(m$W[1, , ]), as.numeric(matrix(1:4, 2)))

  # errors
  expect_error(values_indexed(m, "x[1:2]", 1, "node"), "RHS-only")
  expect_error(values_indexed(m, "logProb_y[1:2]", 1, "node"), "logProb")
  expect_error(values_indexed(m, "W", 3, "node"))
  expect_error(values_indexed(m, "W", 1, "node") <- 1:3, "wrong length")
  expect_error(values_indexed(m, "mu", 1, "node") <- c(1, 2), "wrong length")
  expect_error(values_indexed(m, "v", 1, "node") <- c(1, 2), "wrong length")
}

test_that("values(model, nodes[i], mode = \"node\") works, uncompiled", {
  inputs <- make_values_node_inputs()
  m <- nimbleModel(values_node_code, data = inputs$data, inits = inputs$inits)
  check_values_node(m)
})

test_that("values(model, nodes[i], mode = \"node\") works, compiled", {
  inputs <- make_values_node_inputs()
  cm <- nimbleModel(values_node_code, data = inputs$data, inits = inputs$inits, compile = TRUE)
  check_values_node(cm)
})

test_that("compiled code can call model$getValues_indexed and model$setValues_indexed", {
  inputs <- make_values_node_inputs()
  mclass <- nimbleModel(values_node_code, data = inputs$data, inits = inputs$inits, returnClass = TRUE)
  valuesCaller <- nClass(
    classname = "valuesCaller",
    Cpublic = list(
      model = "nimbleModel:::modelBase_nClass()",
      nodes = "nimbleModel:::instrList_nClass()",
      get = nFunction(
        function(i = "integerScalar") {
          return(model$getValues_indexed(nodes, i))
        },
        returnType = "numericVector"
      ),
      set = nFunction(
        function(i = "integerScalar", v = "numericVector") {
          model$setValues_indexed(nodes, i, v)
        },
        returnType = "void"
      ),
      getAll = nFunction(
        function() {
          return(model$getValues_nodes(nodes))
        },
        returnType = "numericVector"
      ),
      setAll = nFunction(
        function(v = "numericVector") {
          model$setValues_nodes(nodes, v)
        },
        returnType = "void"
      )
    )
  )
  comp <- nCompile(mclass, valuesCaller)
  cm <- comp$mclass$new()
  caller <- comp$valuesCaller$new()
  caller$model <- cm
  instrList <- makeInstrList(cm, c("W", "mu"))
  caller$nodes <- cm$makeCompiledInstrList(instrList)
  cm$calculate()
  # sorted order: mu, W[1, , ], W[2, , ]
  expect_equal(caller$get(1L), cm$mu)
  expect_equal(caller$get(2L), as.numeric(cm$W[1, , ]))
  caller$set(3L, c(4, 3, 2, 1))
  expect_equal(as.numeric(cm$W[2, , ]), c(4, 3, 2, 1))
  caller$set(1L, 2.5)
  expect_equal(cm$mu, 2.5)
  expect_error(caller$set(1L, c(1, 2)), "wrong length")
  expect_error(caller$set(2L, c(1, 2)), "wrong length")
  expect_error(caller$get(4L), "outside")
  # all the nodes: mu, W[1, , ], W[2, , ]
  expect_equal(caller$getAll(), c(cm$mu, as.numeric(cm$W[1, , ]), as.numeric(cm$W[2, , ])))
  caller$setAll(1:9)
  expect_equal(cm$mu, 1)
  expect_equal(as.numeric(cm$W[1, , ]), 2:5)
  expect_equal(as.numeric(cm$W[2, , ]), 6:9)
  expect_error(caller$setAll(1:8), "length")
})

# Alignment: node i of values(model, nodes[i], mode = "node") is the node that calculate(nodes[i])
# and getLogProb(nodes[i]) act on. The reference is the sorted, expanded node names.
check_values_node_alignment <- function(m, nodes) {
  nodeNames <- m$topologicallySortNodes(nodes)
  instrList <- makeInstrList(m, nodes, includeData = TRUE)
  expect_equal(instrList$nNodes_total, length(nodeNames))
  for (k in seq_along(nodeNames)) {
    info <- paste("node", k, nodeNames[k])
    expect_equal(values_indexed(m, nodes, k, "node"), values(m, nodeNames[k]), info = info)
    expect_equal(m$getLogProb_indexed(nodes, k), m$getLogProb(nodeNames[k]), info = info)
    # Setting node k changes exactly that node.
    old <- lapply(nodeNames, \(nn) values(m, nn))
    new_k <- old[[k]] + 100
    values_indexed(m, nodes, k, "node") <- new_k
    expected <- old
    expected[[k]] <- new_k
    expect_equal(lapply(nodeNames, \(nn) values(m, nn)), expected, info = info)
    # calculate_indexed acts on the same node: its logProb now matches a calculate of that node.
    lp <- m$calculate_indexed(nodes, k)
    expect_equal(lp, m$getLogProb(nodeNames[k]), info = info)
    values_indexed(m, nodes, k, "node") <- old[[k]]
    m$calculate(nodeNames[k])
  }
  # Unindexed node mode: all the nodes, concatenated in the same order.
  allOld <- lapply(nodeNames, \(nn) values(m, nn))
  expect_equal(values(m, nodes, mode = "node"), unlist(allOld, use.names = FALSE))
  newAll <- unlist(allOld, use.names = FALSE) + 1000
  values(m, nodes, mode = "node") <- newAll
  expect_equal(unlist(lapply(nodeNames, \(nn) values(m, nn)), use.names = FALSE), newAll)
  # A wrong length is an error before anything is set.
  expect_error(values(m, nodes, mode = "node") <- newAll[-1], "length")
  expect_error(values(m, nodes, mode = "node") <- c(newAll, 1), "length")
  expect_equal(values(m, nodes, mode = "node"), newAll)
  values(m, nodes, mode = "node") <- unlist(allOld, use.names = FALSE)
  expect_equal(lapply(nodeNames, \(nn) values(m, nn)), allOld)
  m$calculate(nodeNames)
}

check_values_edge_cases <- function(m) {
  y <- m$y
  # duplicates and overlaps: raw and scalar modes keep them, node mode has each node once
  expect_equal(values(m, c("mu", "mu")), c(m$mu, m$mu))
  expect_equal(values_indexed(m, c("y[1:2]", "y[2:3]"), 2, "raw"), y[2:3])
  expect_equal(values_indexed(m, c("y[1:2]", "y[2:3]"), 3, "scalar"), y[2])
  dupNodes <- c("mu", "mu", "y[1:2]", "y[2:3]")
  expect_equal(makeInstrList(m, dupNodes, includeData = TRUE)$nNodes_total, 4)
  expect_equal(values_indexed(m, dupNodes, 3, "node"), y[2])
  expect_error(values_indexed(m, dupNodes, 5, "node"))
  # raw assignment with an overlap: later entries win
  values(m, c("y[1:2]", "y[2:3]")) <- c(11, 12, 13, 14)
  expect_equal(m$y, c(11, 13, 14))
  m$y <- y

  # i given as an expression, in each mode
  nodes <- c("y[1:2]", "mu", "y[3]")
  j <- 1
  expect_equal(values(m, nodes[j + 1]), m$mu)
  expect_equal(values(m, nodes[j + 1], mode = "scalar"), y[2])
  expect_equal(values(m, nodes[2 * j + 1], mode = "node"), y[2]) # sorted: mu, y[1], y[2], y[3]
  values(m, nodes[j + 2], mode = "raw") <- 30
  expect_equal(m$y[3], 30)
  values(m, nodes[j + 1], mode = "scalar") <- 20
  expect_equal(m$y[2], 20)
  values(m, nodes[j], mode = "node") <- -2
  expect_equal(m$mu, -2)
  m$y <- y
}

test_that("values(nodes[i], mode = \"node\") aligns with calculate and getLogProb, and edge cases work", {
  data <- list(x = c(0.1, 0.2, 0.3), m0 = c(1, 2, 3), B = matrix(1:4, 2), y = c(1, 2, 3)) # y is data
  inits <- list(mu = 0.5, tau = 2)
  for (compile in c(FALSE, TRUE)) {
    m <- nimbleModel(values_node_code, data = data, inits = inits, compile = compile)
    m$calculate()
    check_values_node_alignment(m, m$getNodeNames()) # every node, including lifted and data nodes
    check_values_node_alignment(m, c("z", "W[2, 1:2, 1:2]", "y[2:3]", "mu")) # input order is not sorted
    check_values_edge_cases(m)
  }
})

test_that("nimbleModelOptions(check_values_node_inputs = FALSE) skips check_values_node_input", {
  inputs <- make_values_node_inputs()
  m <- nimbleModel(values_node_code, data = inputs$data, inits = inputs$inits)
  expect_error(nimbleModel:::check_values_node_input(m, "x[1:2]"), "RHS-only")
  expect_true(nimbleModel:::check_values_node_input(m, "mu"))
  setNimbleModelOption("check_values_node_inputs", FALSE)
  on.exit(setNimbleModelOption("check_values_node_inputs", TRUE))
  expect_true(nimbleModel:::check_values_node_input(m, "x[1:2]"))
  expect_true(nimbleModel:::check_values_node_input(m, "logProb_y[1:2]"))
})
