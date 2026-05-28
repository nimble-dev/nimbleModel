# Test code needed for new nimbleModel system.

library(nCompiler)
library(nimbleModel)
library(testthat)

## TODO: will location and access to predefined nClasses be as described below given they will live
## in `nimbleModel` package? How will dependence on nCompiler work?

## # To update the set of predefined nClasses
## # generate new predef/instr_nC. Move that directly to package code inst/nimbleModel/predef/instr_nC
## nCompile(instr_nClass, control=list(generate_predefined=TRUE))
## test <- nCompile(instr_nClass)
## #
## # generate new predef/declFunBase_nC. Move to package and add
## # "#include <nimbleModel/predef/declFunBase_nC_/declFunBase_nC_.h>" in the hContent
## # after declaration of declFunBase_nClass
## nCompile(declFunBase_nClass, control=list(generate_predefined=TRUE))
## test <- nCompile(declFunBase_nClass)
## #
## # generate new predef/modelBase_nC. Move to package and add
## # "#include <nimbleModel/predef/modelBase_nClass/modelBase_nCl_.h>" to that file,
## # after the declaration of modelBase_nClass.
## nCompile(modelBase_nClass, control=list(generate_predefined=TRUE))
## test <- nCompile(modelBase_nClass)
## #nCompile(instr_nClass, modelBase_nClass, declFunBase_nClass, control=list(generate_predefined=TRUE))

## TODO: revise these tests for instrClass (flattened approach)

test_that("initial tests/examples of nimble model using flattened approach", {

    code <- quote({
        tau ~ dunif(0, 100)
        mu ~ dnorm(0,1)
        for(i in 1:5) {
            y[i] ~ dnorm(mu, var = tau)
        }
    })

    inits <- list(tau = 25, mu = 0)
    data <- list(y = rnorm(5))

    ## "Manual" workflow not using `nimbleModel()`.
    nm <- modelClass$new(code, inits = inits, data = data)
    mclass <- nimbleModel:::make_modelClass_from_nimbleModel(nm)
    m <- mclass$new()

    expect_identical(m$calculate('tau'), dunif(m$tau, 0, 100, log = TRUE))

    instrList <- makeInstrList(m, 'tau')
    expect_identical(m$calculate(instrList), dunif(m$tau, 0, 100, log = TRUE))

    deps <- m$getDependencies('tau', self = FALSE)
    lp <-m$calculate(deps)
    expect_identical(m$lifted_sqrt_oPtau_cP, 5)
    expect_identical(lp, sum(dnorm(m$y, 0, 5, log = TRUE)))

    ## Check that instrList is in correct order.
    instrList <- makeInstrList(m, c('y','lifted_sqrt_oPtau_cP'))
    expect_identical(instrList[[1]]$lens, 1)  # lifted node first
    lp <- m$calculate(instrList)
    expect_identical(m$lifted_sqrt_oPtau_cP, 5)
    expect_identical(lp, sum(dnorm(m$y, 0, 5, log = TRUE)))

    expect_identical(m$logProb_y, dnorm(m$y, 0, 5, log = TRUE))

    m$tau <- 1
    lp <- m$calculate(c('y','lifted_sqrt_oPtau_cP'))  # Ordering should be done internally.
    expect_equal(lp, sum(dnorm(m$y, 0, 1, log = TRUE))) # Why not identical?

    expect_equal(m$calculate(), sum(dnorm(m$y, 0, 1, log = TRUE)) + dunif(m$tau, 0, 100, log = TRUE) + dnorm(m$mu, log = TRUE))

    ## NOTE: `simulate` currently simulates data nodes by default.
    set.seed(1)
    m$simulate()
    expect_identical(m$lifted_sqrt_oPtau_cP, sqrt(m$tau))
    expect_equal(m$mu, -0.326233360706)
    m$mu <- 100
    m$tau <- 1
    m$simulate(m$getDependencies('tau', self = FALSE))
    expect_true(all(m$y > 95))

    ## Use of nimbleModel
    mclass <- nimbleModel(code, data = data, inits = inits)
    m <- mclass$new()
    expect_identical(m$calculate('tau'), dunif(m$tau, 0, 100, log = TRUE))

    m <- nimbleModel(code, data = data, inits = inits, returnClass = FALSE)
    expect_identical(m$calculate('tau'), dunif(m$tau, 0, 100, log = TRUE))

    ## Override init value when creating model instance.
    mclass <- nimbleModel(code, data = data, inits = inits)
    m <- mclass$new(inits = list(tau = 7))
    expect_identical(m$tau, 7)
    
})

test_that("basic creation of list of instr_nClass objects", {

    code <- quote({
        for(i in 1:5) {
            mu ~ dnorm(0, 1)
            y[i] ~ dnorm(mu, 1)
        }
    })

    data <- list(y = rnorm(5))

    m <- nimbleModel(code, data = data, returnClass = FALSE)

    instr0 <- makeInstrList(m, 'mu')[[1]]
    expect_identical(instr0$lens, 1)
    expect_identical(length(instr0$values), 0L)
    expect_identical(instr0$index_types, 0)
    expect_identical(instr0$type, 0)

    instr1 <- makeInstrList(m, 'y[3:4]')[[1]]
    expect_identical(instr1$lens, 2)
    expect_identical(instr1$values[[1]], 2) # offset
    expect_identical(instr1$index_types, 1)
    expect_identical(instr1$type, 1)
    
    instr2 <- makeInstrList(m, c('y[c(2,5)]'))[[1]]
    expect_identical(instr2$lens, 2)
    expect_identical(instr2$values[[1]], c(2,5))
    expect_identical(instr2$index_types, 2)
    expect_identical(instr2$type, 2)

    instr2 <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(2,5), ncol=1))), varName='y'))[[1]]
    expect_identical(instr2$lens, 2)
    expect_identical(instr2$values[[1]], c(2,5))
    expect_identical(instr2$index_types, 2)
    expect_identical(instr2$type, 2)

    ## TODO: flesh this out with multiple index cases.
})

## TODO: modify tests below in light of flattened approach.

test_that("nimble model prototype works", {
  nodeVarInfo <- list(list(name = "x", nDim  = 1), list(name = "mu", nDim = 1),
                      list(name = "sd", nDim = 0))
  calc_one <- nFunction(
    name = "calc_one",
    fun = function(inds) {
      ans <- model$x[inds[1]]
      return(ans)
    },
    compileInfo = list(
      C_fun = function(inds = 'integerVector') {
        returnType('numericScalar')
        ans <- x[inds[1]]
        return(ans)
      }
    )
  )
  my_nodeFxn <- make_node_nClass(nodeVarInfo, list(calc_one=calc_one), "test_node")
  my_nodeInfo <- make_node_info_for_model_nClass("beta_NF1", "my_nodeFxn", "test_node", nodeVarInfo)

  modelVarInfo <- list(list(name="x", nDim = 1),
                       list(name = "mu", nDim = 1),
                       list(name = "sd", nDim = 0),
                       list(name = "gamma", nDim = 2))
  #debug(makeModel_nClass)
  ncm1 <- makeModel_nClass(modelVarInfo, list(my_nodeInfo), classname = "my_model", env=environment())
  #undebug(addGenericInterface_impl)
  #undebug(nCompile_finish_nonpackage)
  for(package in c(FALSE, TRUE)) {
    Cncm1 <- nCompile(ncm1, returnList=TRUE, package=package)
    #Cncm1 <- nCompile(modelBase_nClass, nodeFxnBase_nClass, calcInstrList_nClass, calcInstr_nClass, nodeInstr_nClass, ncm1, my_nodeFxn)
    for(mode in c("uncompiled", "compiled")) {
      if(mode=="compiled") {
        obj <- Cncm1$ncm1$new()
      } else {
        obj <- ncm1$new()
      }
      # obj$do_setup_node_mgmt()
      nodeObj <- obj$beta_NF1
      obj$x <- 1:3
      expect_equal(obj$x, 1:3)

      obj$set_from_list(list(x = 10:11))
      # expect Problem msg: (alpha is not a field in the class)
      obj$set_from_list(list(mu = 110, x = 11:20, alpha = 101))
      obj$mu

      obj$resize_from_list(list(x = 7))
      # expect Problem msg:
      obj$resize_from_list(list(alpha = 5, mu = 3, gamma = c(2, 4)))
      expect_equal(length(obj$mu), 3)
      expect_equal(dim(obj$gamma), c(2, 4))
      obj$resize_from_list(list(x = 5, gamma = c(3, 5)))
      expect_equal(length(obj$x), 5)
      expect_equal(dim(obj$gamma), c(3, 5))

      obj$x <- 11:15
      expect_equal(nodeObj$calc_one(c(3)), 13)
      rm(obj, nodeObj); gc()
    }
  }
})

test_that("nodeInstr_nClass and calcInstr_nClass basics work", {
  for(package in c(FALSE, TRUE)) {
    test <- nCompile(nodeInstr_nClass, calcInstr_nClass, calcInstrList_nClass, control=list(generate_predefined=FALSE), package = package)
    calcInstrList <- test$nList_calcInstr_nClass$new()
    calcInstr <- test$calcInstr_nClass$new()
    expect_equal(calcInstr$nodeInstrVec, NULL)
    ni1 <- test$nodeInstr_nClass$new()
    ni2 <- test$nodeInstr_nClass$new()
    ni1$methodInstr <- 1
    ni2$methodInstr <- 2
#    nList("integerVector")$new()
#    ni1$indsInstrVec <- nList("integerVector")$new()
    ni1$indsInstrVec[1:2] <- list(1:2, 3:4)
    ni2$indsInstrVec
    ni2$indsInstrVec[1:2] <- list(11:12, 13:14)
    calcInstr$nodeInstrVec
    calcInstr$nodeInstrVec[1:2] <- list(ni1, ni2)

    expect_true(length(calcInstr$nodeInstrVec)==2)
    expect_identical(calcInstr$nodeInstrVec[[1]]$indsInstrVec |> as.list(), list(1:2, 3:4))
    expect_identical(calcInstr$nodeInstrVec[[2]]$indsInstrVec |> as.list(), list(11:12, 13:14))
    calcInstrList[1] <- list(calcInstr)
    expect_equal(calcInstrList |> as.list(), list(calcInstr))
    rm(calcInstrList, calcInstr, ni1, ni2); gc()
  }
})

######

## This is somewhat redundant with the first test
test_that("nimble model variables are set up", {
  library(nimbleModel)
  code <- quote({
    sd ~ dunif(0, 10)
    for(i in 1:5) {
      y[i] ~ dnorm(x[i+1], sd = sd)
    }
  })
  m <- modelClass$new(code)
  varInfo <- get_varInfo_from_nimbleModel(m)
  modelVars <- varInfo$vars
  # Try making a model with no nodeFxns
  ncm1 <- makeModel_nClass(modelVars, list(), classname = "my_model", env = environment())
  Cncm1 <- nCompile(ncm1, returnList=TRUE)
  #Cncm1 <- nCompile(modelBase_nClass, nodeFxnBase_nClass, calcInstrList_nClass, calcInstr_nClass, nodeInstr_nClass, ncm1)
  obj <- Cncm1$ncm1$new()
  obj$resize_from_list(varInfo$sizes)
  expect_equal(length(obj$x), 6)
  expect_equal(length(obj$y), 5)
  expect_equal(length(obj$logProb_y), 5)
})

########
# nOptions(pause_after_writing_files=TRUE)
# Try automating the whole model creation including nodeFxns
# Ditto: this works but relies on nimbleModel
test_that("nimble model with stochastic and deterministic nodes is created and compiles", {
  library(nimbleModel)
  code <- quote({
    sd ~ dunif(0, 10)
    for(i in 1:5) {
      z[i] <- x[i+1] + 10
      y[i] ~ dnorm(x[i+1], sd = sd)
    }
  })
  m <- modelClass$new(code)

  ## Check that a separate R implementation was created
  mDef_ <- m$modelDef
  dI <- mDef_$declInfo[[2]]
  nFxn <- make_node_methods_from_declInfo(dI)
  expect_true(!is.null(NFinternals(nFxn[[1]])$R_fun))
  dI <- mDef_$declInfo[[3]]
  nFxn <- make_node_methods_from_declInfo(dI)
  expect_true(!is.null(NFinternals(nFxn[[1]])$R_fun))

  for(mode in c("uncompiled", "compiled")) {
    package_options <- if(mode=="compiled") c(FALSE, TRUE) else TRUE
    for(package in package_options) {
      nMod <- make_model_from_nimbleModel(m, compile=FALSE)
      if(mode=="compiled") {
        expect_no_error(CnMod <- nCompile(nMod, package = package))
        nMod <- CnMod
      }
      expect_no_error(obj <- nMod$new())
      obj$y <- 1:5
      expect_equal(obj$y, 1:5)
      vals <- list(x = 2:7, y = 11:15, sd = 8)
      obj$set_from_list(vals)
      expect_equal(obj$x, vals$x)
      rm(obj); gc()
    }
  }
})

message("test-nimbleModel does not have tests of calculate etc.")

if(FALSE) {
  nodeFxn_2_nodeIndex <- c(nodeFxn_1 = 1, nodeFxn_3 = 2)

  calcInputList <- list(list(nodeFxn="nodeFxn_1",        # which declaration (nodeFxn)
                             nodeInputVec = list(list(methodInput=1,  # which index iteration method
                                                      indsInputVec=list(1))))) # input(s) to index iterations

  calcInstrList <- calcInputList_to_calcInstrList(calcInputList, test)

  obj$calculate(calcInstrList)
}
