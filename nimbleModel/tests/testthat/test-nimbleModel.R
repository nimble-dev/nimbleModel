# Test code needed for new nimbleModel system.

library(nCompiler)
library(nimbleModel)
library(testthat)

test_that("initial test of compiled model", {
    code <- quote({
        tau ~ dunif(0, 100)
        mu ~ dnorm(0,1)
        for(i in 1:5) {
            y[i] ~ dnorm(mu, var = tau)
        }
    })

    inits <- list(tau = 25, mu = 0)
    data <- list(y = rnorm(5))

    ## Manual workflow
    mclass <- nimbleModel(code, inits = inits, data = data, returnClass = TRUE)
    obj <- mclass$new()
    cmclass <- nCompile(mclass)
    Cobj <- cmclass$new()

    # Check a first calculation on a simple node
    Cans <- Cobj$calculate('tau')
    ans <- obj$calculate('tau')
    check <- dunif(Cobj$tau, 0, 100, log = TRUE)
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Check entire model, also getting lifted sd node computed
    Cans <- Cobj$calculate()
    ans <- obj$calculate()
    expect_equal(Cans, ans)

    # Check a sequence
    Cans <- Cobj$calculate('y[1:3]')
    ans <- obj$calculate('y[1:3]')
    check <- dnorm(Cobj$y[1:3], Cobj$mu, sqrt(Cobj$tau), log=TRUE) |> sum()
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Check a non-contiguous pair of nodes (a mat case)
    nodes <- c('y[2]','y[4]')
    Cans <- Cobj$calculate(nodes)
    ans <- obj$calculate(nodes)
    check <- dnorm(Cobj$y[c(2, 4)], Cobj$mu, sqrt(Cobj$tau), log=TRUE) |> sum()
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Check getLogProb
    Cans <- Cobj$getLogProb('y[1:4]')
    ans <- obj$calculate('y[1:4]')
    check <- dnorm(Cobj$y[1:4], Cobj$mu, sqrt(Cobj$tau), log=TRUE) |> sum()
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Prepare for calculateDiff test below
    old_logProb <- dnorm(Cobj$y[3:4], Cobj$mu, sqrt(Cobj$tau), log=TRUE) |> sum()

    # Check simulate
    set.seed(1)
    Cobj$simulate('y[3:4]')
    set.seed(1)
    obj$simulate('y[3:4]')
    expect_equal(Cobj$y, obj$y)

    # Check getLogProb
    # Do this assignment in case the previous test of repeatability fails
    obj$y[3:4] <- Cobj$y[3:4]
    Cans <- Cobj$calculateDiff('y[3:4]')
    ans <- obj$calculateDiff('y[3:4]')
    new_logProb <- dnorm(Cobj$y[3:4], Cobj$mu, sqrt(Cobj$tau), log=TRUE) |> sum()
    check <- new_logProb - old_logProb
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Always end compiled tests with removing and garbage collecting
    # to ensure gc() happens while the DLL is still in place.
    rm(Cobj, obj); gc()
})

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

    ## "Manual" workflow
    mclass <- nimbleModel(code, inits = inits, data = data, returnClass = TRUE)

    # Begin Perry
    Cmclass <- nCompile(mclass)
    Cobj <- Cmclass$new()
    #Cobj$calculate_impl
    #Cobj$calculate
    #debug(Cobj$calculate)
    Cobj$calculate('tau')
    Cobj$calculate()
    Cobj$calculate('y[1]')
    dnorm(Cobj$y[1], Cobj$mu, sqrt(Cobj$tau), log=TRUE)
    Cobj$calculate('y[1:3]')
    dnorm(Cobj$y[1:3], Cobj$mu, sqrt(Cobj$tau), log=TRUE) |> sum()


    NULL

    obj <- mclass$new()
    obj$calculate()
    #debug(obj$calculate)
    obj$calculate('y[1]')
    obj$calculate('y[1:3]')
    NULL
    # PROBLEM, in nList_<>::set_from_list for uncompiled list input.
    # I guess set_all_values should skip NULLs? Or maybe only for non-R targets?
    # Give a better message than "Bad type". Pass the name? Check for NULL?

    # Next steps
    # initialize the instrs from uncompiled if needed
    #

    # check technique of building and copying nList(instr_nClass) as a method:
    inC <- nimbleModel:::instr_nClass
    test1 <- nFunction(
      function(Robj = 'SEXP') {
        ans <- inC$new()
        cppLiteral("ans->set_all_values(Robj);")
        cppLiteral("std::cout<<ans->nDim<<std::endl;")
        return(ans)
      },
      returnType = 'inC'
    )
    ctest1 <- nCompile(test1)
    #check_obj <- function(x) {browser(); NULL;}
    ctest1(list())
    obj1 <- ctest1(list(dim = 2L, dims = c(3L, 3L)))
    obj1$nDim
    obj1$dims
    obj1$values

    #####
    inC <- nimbleModel:::instr_nClass
    test1 <- nFunction(
      function(Robj = 'SEXP') {
        ans <- nList(inC)$new()
        cppLiteral("ans->set_all_values(Robj);")
#        cppLiteral("std::cout<<ans->dims<<std::endl;")
        return(ans)
      },
      returnType = 'nList(inC)'
    )
    ctest1 <- nCompile(test1)
    #check_obj <- function(x) {browser(); NULL;}
    ctest1(list())
    obj1 <- ctest1(list(list(dim = 2L, dims = c(3L, 3L))))
    obj1[[1]]$nDim
    obj1[[1]]$dims
    obj1[[1]]$values
    obj1[[1]]$lens

    # End Perry


    m <- mclass$new()

    lp_tau <- dunif(m$tau, 0, 100, log = TRUE)
    expect_identical(m$calculate('tau'), lp_tau)
    expect_identical(m$getLogProb('tau'), lp_tau)

    instrList <- makeInstrList(m, 'tau')
    expect_identical(m$calculate(instrList), lp_tau)
    expect_identical(m$getLogProb(instrList), lp_tau)

    deps <- m$getDependencies('tau', self = FALSE)
    lp_y <- sum(dnorm(m$y, 0, 5, log = TRUE))
    lp <- m$calculate(deps)
    expect_identical(m$lifted_sqrt_oPtau_cP, 5)
    expect_equal(lp, lp_y)
    expect_identical(m$getLogProb('y'), lp)

    ## Check that instrList is in correct order.
    instrList <- makeInstrList(m, c('y','lifted_sqrt_oPtau_cP'))
    expect_identical(instrList[[1]]$lens, 1)  # lifted node first
    lp <- m$calculate(instrList)
    expect_identical(m$lifted_sqrt_oPtau_cP, 5)
    expect_equal(lp, lp_y)
    expect_identical(m$getLogProb(c('y','lifted_sqrt_oPtau_cP')), lp_y)

    expect_identical(m$logProb_y, dnorm(m$y, 0, 5, log = TRUE))

    m$tau <- 1
    lp <- m$calculate(c('y','lifted_sqrt_oPtau_cP'))  # Ordering should be done internally.
    expect_equal(lp, sum(dnorm(m$y, 0, 1, log = TRUE))) # Why not identical?

    lp <- sum(dnorm(m$y, 0, 1, log = TRUE)) + dunif(m$tau, 0, 100, log = TRUE) + dnorm(m$mu, log = TRUE)
    expect_equal(m$calculate(), lp)
    expect_equal(m$getLogProb(), lp)

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

test_that("no indices", {
    code <- quote({
        mu ~ dnorm(0, 1)
        sigma ~ dgamma(1,1)
    })
    inits <- list(mu = 1, sigma = .7)
    mclass <- nimbleModel(code, inits = inits, returnClass = TRUE)
    m <- mclass$new()
    vr <- varRangeClass$new('mu')
    expect_equal(m$calculate(vr), dnorm(inits$mu,log=TRUE))
    expect_equal(m$calculate(), dnorm(inits$mu,log=TRUE) + dgamma(inits$sigma,1,1,log=TRUE))
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()
    expect_equal(cm$calculate(vr), dnorm(inits$mu,log=TRUE))
    expect_equal(cm$calculate(), dnorm(inits$mu,log=TRUE) + dgamma(inits$sigma,1,1,log=TRUE))

    set.seed(1)
    result <- rnorm(1)
    set.seed(1)
    m$simulate('mu')
    expect_equal(m$mu, result)
    set.seed(1)
    cm$simulate('mu')
    expect_equal(cm$mu, result)
})

test_that("one index", {
    code <- quote({
        for(i in 3:10)
            y[i] ~ dnorm(0,1)
    })
    data <- list(y = rnorm(10))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()

    ## Scalar
    vr <- varRangeClass$new(list(newIndexRange(matrix(5,ncol=1))), varName = 'y')
    truth <- dnorm(m$y[5], log=TRUE)
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[5], result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(m$y[5], result)

    ## Sequence
    vr <- varRangeClass$new(list(newIndexRange(quote(4:6))), varName = 'y')
    truth <- sum(dnorm(m$y[4:6], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(3)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[4:6], result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(cm$y[4:6], result)

    ## Matrix
    inds <- c(4,6,9)
    vr <- varRangeClass$new(list(newIndexRange(matrix(inds,ncol=1))), varName = 'y')
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(3)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[inds], result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(cm$y[inds], result)
})

test_that("two index slots", {
    code <- quote({
        for(i in 1:4)
            for(j in 1:3)
                y[i,j] ~ dnorm(0,1)
    })
    data <- list(y = matrix(rnorm(12),4))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()

    inds <- matrix(c(3,1, 4,2, 2,3), ncol=2, byrow=TRUE)
    vr <- varRangeClass$new(list(newIndexRange(inds)), varName = 'y')
    inds <- vr$indexRanges[[1]]$values   # Rows have been shuffled...
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(3)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[inds], result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(cm$y[inds], result)

    ## Reverse indices
    inds <- matrix(c(1,3, 2,4, 3,2), ncol=2, byrow=TRUE)
    vr <- varRangeClass$new(list(newIndexRange(inds)), rangeToIndexSlot = list(c(2,1)), varName = 'y')
    tmp <- vr$indexRanges[[1]]$values[,2:1]
    inds <- tmp[order(tmp[,1]),]    # Rows have been shuffled...
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(3)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[inds], result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(cm$y[inds], result)

    ## seq-seq
    vr <- varRangeClass$new(list(newIndexRange(quote(2:4)), newIndexRange(quote(1:3))), varName = 'y')
    truth <- sum(dnorm(m$y[2:4,1:3], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(9)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(t(m$y[2:4,1:3])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(t(cm$y[2:4,1:3])), result)
    
    ## seq-mat
    vr <- varRangeClass$new(list(newIndexRange(quote(2:4)), newIndexRange(matrix(c(3,1),ncol=1))), varName = 'y')
    truth <- sum(dnorm(m$y[2:4,c(1,3)], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(t(m$y[2:4,c(1,3)])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(t(cm$y[2:4,c(1,3)])), result)
    
    ## mat-seq
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(3,1),ncol=1)), newIndexRange(quote(2:4))),
                            varName = 'y')
    truth <- sum(dnorm(m$y[c(1:3),2:4], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(t(m$y[c(1:3),2:4])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(t(cm$y[c(1:3),2:4])), result)

    ## mat-mat
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(3,1),ncol=1)), newIndexRange(matrix(c(2,4), ncol=1))),
                            varName = 'y')
    truth <- sum(dnorm(m$y[c(1:3),c(2,4)], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(4)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(t(m$y[c(1:3),c(2,4)])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(t(cm$y[c(1:3),c(2,4)])), result)

})

test_that("three index slots", {
    code <- quote({
        for(i in 1:5)
            for(j in 1:4)
                for(k in 1:3)
                   y[k,j,i] ~ dnorm(0,1)  # This changes order of use of indices.
    })
    data <- list(y = array(rnorm(60),c(3,4,5)))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()
    
    inds <- matrix(c(3,1,5, 3,4,1, 1,2,4), ncol=3, byrow=TRUE)
    vr <- varRangeClass$new(list(newIndexRange(inds)), varName = 'y')
    inds <- vr$indexRanges[[1]]$values   # Rows have been shuffled...
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(3)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[inds], result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(cm$y[inds], result)

    ## Reverse indices
    inds <- matrix(c(5,3,1, 1,3,4, 4,1,2), ncol=2, byrow=TRUE)
    vr <- varRangeClass$new(list(newIndexRange(inds)), rangeToIndexSlot = list(c(3,1,2)), varName = 'y')
    tmp <- vr$indexRanges[[1]]$values[,2:1]
    inds <- tmp[order(tmp[,1]),]    # Rows have been shuffled...
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(3)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[inds], result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(cm$y[inds], result)

    ## seq-seq-seq
    vr <- varRangeClass$new(list(newIndexRange(quote(2:3)), newIndexRange(quote(1:4)),
                               newIndexRange(quote(2:5))), varName = 'y')
    truth <- sum(dnorm(m$y[2:3,1:4,2:5], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(3,4,5))
    for(i in 1:3)
        for(j in 1:4)
            for(k in 1:5)
                result[i,j,k] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[2:3,1:4,2:5]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[2:3,1:4,2:5]), result)

    ## seq-matp
    vr <- varRangeClass$new(list(newIndexRange(quote(2:3)),
                                 newIndexRange(matrix(c(4,1,1,3,4,5),ncol=2,byrow=TRUE))), varName = 'y')
    inds <- rbind(c(2,1,3),c(2,4,1),c(2,4,5),c(3,1,3),c(3,4,1),c(3,4,5)) 
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds])), result)

    ## seq-matp, reordered
    vr <- varRangeClass$new(list(newIndexRange(quote(2:3)),
                                 newIndexRange(matrix(c(4,1,1,3,4,5),ncol=2,byrow=TRUE))),
                            rangeToIndexSlot = list(2,c(1,3)), varName = 'y')
    inds <- rbind(c(1,2,3),c(1,3,3),c(4,2,1),c(4,3,1),c(4,2,5),c(4,3,5)) 
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds])), result)

    ## mat-matp
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,3),ncol=1)),
                                 newIndexRange(matrix(c(4,1,1,3,4,5),ncol=2,byrow=TRUE))), varName = 'y')
    inds <- rbind(c(2,1,3),c(2,4,1),c(2,4,5),c(3,1,3),c(3,4,1),c(3,4,5)) 
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds])), result)
    
    ## matp-mat
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,3),ncol=1)),,
                                 newIndexRange(matrix(c(4,1,1,3,4,5),ncol=2,byrow=TRUE))),
                            rangeToIndexSlot = list(2,c(1,3)), varName = 'y')
    inds <- rbind(c(1,2,3),c(1,3,3),c(4,2,1),c(4,3,1),c(4,2,5),c(4,3,5)) 
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds])), result)

    # seq-mat-seq (3-generic)
    vr <- varRangeClass$new(list(newIndexRange(quote(2:3)), newIndexRange(matrix(c(1,4), ncol=1)),
                               newIndexRange(quote(2:5))), varName = 'y')
    truth <- sum(dnorm(m$y[2:3,c(1,4),2:5], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(2,2,4))
    for(i in 2:3)
        for(j in c(1,4))
            for(k in 2:5)
                result[i,j,k] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[2:3,c(1,4),2:5]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[2:3,c(1,4),2:5]), result)

})

test_that("four index slots", {
    code <- quote({
        for(i in 1:5)
            for(j in 1:4)
                for(k in 1:3)
                    for(l in 1:2)
                        y[l,k,j,i] ~ dnorm(0,1)  # This changes order of use of indices.
    })
    data <- list(y = array(rnorm(120),c(2,3,4,5)))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()

    # all seq
    vr <- varRangeClass$new(list(newIndexRange(quote(1:2)), newIndexRange(quote(2:3)), newIndexRange(quote(1:4)),
                               newIndexRange(quote(2:5))), varName = 'y')
    truth <- sum(dnorm(m$y[1:2,2:3,1:4,2:5], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(2,2,4,4))
    for(i in 1:2)
        for(j in 2:3)
            for(k in 1:4)
                for(l in 2:5)
                result[i,j,k,l] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[1:2,2:3,1:4,2:5]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[1:2,2:3,1:4,2:5]), result)

    # seq-mat-seq-seq (4-generic)
    vr <- varRangeClass$new(list(newIndexRange(quote(1:2)), newIndexRange(quote(2:3)), newIndexRange(matrix(c(1,4), ncol=1)),
                               newIndexRange(quote(2:5))), varName = 'y')
    truth <- sum(dnorm(m$y[2:3,c(1,4),2:5], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(2,2,2,4))
    for(i in 1:2)
        for(j in 2:3)
            for(k in c(1,4)
                for(l in 2:5)
                result[i,j,k,l] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[1:2,2:3,c(1,4),2:5]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[1:2,2:3,c(1,4),2:5]), result)

})

test_that("five index slots", {
    code <- quote({
        for(i in 1:5)
            for(j in 1:4)
                for(k in 1:3)
                    for(l in 1:2)
                        for(m in 1:5)
                            y[m,l,k,j,i] ~ dnorm(0,1)  # This changes order of use of indices.
    })
    data <- list(y = array(rnorm(120*5),c(5,2,3,4,5)))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()

    # all seq
    vr <- varRangeClass$new(list(newIndexRange(quote(3:5)), newIndexRange(quote(1:2)), newIndexRange(quote(2:3)), newIndexRange(quote(1:4)),
                               newIndexRange(quote(2:5))), varName = 'y')
    truth <- sum(dnorm(m$y[3:5,1:2,2:3,1:4,2:5], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(3,2,2,4,4))
    for(m in 3:5)
        for(i in 1:2)
            for(j in 2:3)
                for(k in 1:4)
                    for(l in 2:5)
                        result[m,i,j,k,l] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[3:5,1:2,2:3,1:4,2:5]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[3:5,1:2,2:3,1:4,2:5]), result)

    # 5-generic
    vr <- varRangeClass$new(list(newIndexRange(quote(3:5)), newIndexRange(quote(1:2)),
                                 newIndexRange(quote(2:3)), newIndexRange(matrix(c(1,4), ncol=1)),
                               newIndexRange(quote(2:5))), varName = 'y')
    truth <- sum(dnorm(m$y[2:3,c(1,4),2:5], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(3,2,2,2,4))
    for(m in 3:5)
        for(i in 1:2)
            for(j in 2:3)
                for(k in c(1,4)
                    for(l in 2:5)
                        result[i,j,k,l] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[3:5,1:2,2:3,c(1,4),2:5]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[3:5,1:2,2:3,c(1,4),2:5]), result)

})

## TODO: some of the older tests below probably duplicate those above.
test_that("multiple index slots, single indexRange case", {
    code <- quote({
        for(i in 1:5) 
            for(j in 1:3)
                y[i,j] ~ dnorm(0,1)
    })
    data <- list(y = matrix(rnorm(15),5))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,4,3,1), ncol=2))), varName='y')
    expect_equal(m$calculate(vr), dnorm(data$y[2,3],log=TRUE) + dnorm(data$y[4,1],log=TRUE))
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()
    expect_equal(cm$calculate(vr), dnorm(data$y[2,3],log=TRUE) + dnorm(data$y[4,1],log=TRUE))

    set.seed(1)
    m$simulate(vr)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(m$y, cm$y)

    ## Now with slot reordering.
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,4,3,1), ncol=2))),
                            rangeToIndexSlot = list(2:1), varName='y')
    expect_equal(m$calculate(vr), dnorm(data$y[3,2],log=TRUE) + dnorm(data$y[1,4],log=TRUE))
    expect_equal(cm$calculate(vr), dnorm(data$y[3,2],log=TRUE) + dnorm(data$y[1,4],log=TRUE))

    ## 3-d case for more robust ordering check
    code <- quote({
        for(i in 1:5) 
            for(j in 1:4)
                for(k in 1:3)
                y[i,j,k] ~ dnorm(0,1)
    })
    data <- list(y = array(rnorm(60),c(5,4,3)))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,3,1,3,5,2,1,2,4), ncol=3))),
                            rangeToIndexSlot = list(c(3,1,2)), varName='y')
    truth <- dnorm(data$y[3,1,2],log=TRUE) + dnorm(data$y[5,2,3],log=TRUE) + dnorm(data$y[2,4,1],log=TRUE)
    expect_equal(m$calculate(vr), truth)
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()
    expect_equal(cm$calculate(vr), truth)
    
    
})

test_that("two sequences case", {
    code <- quote({
        for(i in 1:5) 
            for(j in 1:3)
                y[i,j] ~ dnorm(0,1)
    })
    data <- list(y = matrix(rnorm(15),5))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    truth <- sum(dnorm(data$y[2:4,1:3],0,1,log=TRUE))
    m <- mclass$new()
    expect_equal(m$calculate('y[2:4,1:3]'), truth)
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()
    expect_equal(cm$calculate('y[2:4,1:3]'), truth)

    set.seed(1)
    m$simulate('y[2:4,1:3]')
    set.seed(1)
    cm$simulate('y[2:4,1:3]')
    expect_equal(m$y, cm$y)

    ## 2-d case for ordering check.
    ## This does not test calc_2_seq_seq_ord because rule application in creating instr
    ## already re-sorts the indexRanges.
    code <- quote({
        for(i in 1:5) 
            for(j in 1:2)
                y[i,j] ~ dnorm(0,1)
    })
    data <- list(y = matrix(rnorm(10),5))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    vr <- varRangeClass$new(list(newIndexRange(quote(1:2)), newIndexRange(quote(1:5))),
                                 rangeToIndexSlot = list(2,1), varName='y')
    truth <- sum(dnorm(data$y, log = TRUE))
    expect_equal(m$calculate(vr), truth)
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()
    expect_equal(cm$calculate(vr), truth)
    
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
