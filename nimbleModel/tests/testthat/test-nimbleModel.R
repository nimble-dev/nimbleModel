# Test code needed for new nimbleModel system.

library(nCompiler)
library(nimbleModel)
library(testthat)

test_that("basic testing of models, compiled and uncompiled", {
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
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()

    # Check a first calculation on a simple node
    Cans <- cm$calculate('tau')
    ans <- m$calculate('tau')
    check <- dunif(cm$tau, 0, 100, log = TRUE)
    expect_equal(Cans, ans)
    expect_equal(Cans, check)
    expect_identical(ans, m$getLogProb('tau'))
    expect_identical(Cans, cm$getLogProb('tau'))

    # Check entire model, also getting lifted sd node computed
    Cans <- cm$calculate()
    ans <- m$calculate()
    expect_equal(Cans, ans)

    deps <- m$getDependencies('tau', self = FALSE)
    lp_y <- sum(dnorm(m$y, 0, 5, log = TRUE))
    lp <- m$calculate(deps)
    expect_identical(m$lifted_sqrt_oPtau_cP, 5)
    expect_equal(lp, lp_y)
    expect_identical(m$getLogProb('y'), lp)
    clp <- cm$calculate(deps)
    expect_identical(cm$lifted_sqrt_oPtau_cP, 5)
    expect_equal(clp, lp_y)
    expect_identical(cm$getLogProb('y'), clp)

    ## Check that instrList is in correct order.
    instrList <- makeInstrList(m, c('y','lifted_sqrt_oPtau_cP'))
    expect_identical(instrList[[1]]$lens, 1)  # lifted node first
    lp <- m$calculate(instrList)
    expect_identical(m$lifted_sqrt_oPtau_cP, 5)
    expect_equal(lp, lp_y)
    expect_equal(m$getLogProb(c('y','lifted_sqrt_oPtau_cP')), lp_y)
    expect_identical(m$logProb_y, dnorm(m$y, 0, 5, log = TRUE))
    lp <- cm$calculate(instrList)
    expect_identical(cm$lifted_sqrt_oPtau_cP, 5)
    expect_equal(lp, lp_y)
    expect_equal(cm$getLogProb(c('y','lifted_sqrt_oPtau_cP')), lp_y)
    expect_identical(cm$logProb_y, dnorm(cm$y, 0, 5, log = TRUE))

    m$tau <- 1
    lp <- m$calculate(c('y','lifted_sqrt_oPtau_cP'))  # Ordering should be done internally.
    expect_equal(lp, sum(dnorm(m$y, 0, 1, log = TRUE))) # Why not identical?
    cm$tau <- 1
    lp <- cm$calculate(c('y','lifted_sqrt_oPtau_cP'))  # Ordering should be done internally.
    expect_equal(lp, sum(dnorm(cm$y, 0, 1, log = TRUE))) # Why not identical?

    lp <- sum(dnorm(m$y, 0, 1, log = TRUE)) + dunif(m$tau, 0, 100, log = TRUE) + dnorm(m$mu, log = TRUE)
    expect_equal(m$calculate(), lp)
    expect_equal(m$getLogProb(), lp)
    lp <- sum(dnorm(cm$y, 0, 1, log = TRUE)) + dunif(cm$tau, 0, 100, log = TRUE) + dnorm(cm$mu, log = TRUE)
    expect_equal(cm$calculate(), lp)
    expect_equal(cm$getLogProb(), lp)

    ## NOTE: `simulate` currently simulates data nodes by default.
    set.seed(1)
    m$simulate()
    expect_identical(m$lifted_sqrt_oPtau_cP, sqrt(m$tau))
    expect_equal(m$mu, -0.326233360706)
    m$mu <- 100
    m$tau <- 1
    m$simulate(m$getDependencies('tau', self = FALSE))
    expect_true(all(m$y > 95))
    
    set.seed(1)
    cm$simulate()
    expect_identical(cm$lifted_sqrt_oPtau_cP, sqrt(cm$tau))
    expect_equal(cm$mu, -0.326233360706)
    cm$mu <- 100
    cm$tau <- 1
    cm$simulate(cm$getDependencies('tau', self = FALSE))
    expect_true(all(cm$y > 95))

    # Check a sequence
    Cans <- cm$calculate('y[1:3]')
    ans <- m$calculate('y[1:3]')
    check <- dnorm(cm$y[1:3], cm$mu, sqrt(cm$tau), log=TRUE) |> sum()
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Check a non-contiguous pair of nodes (a mat case)
    nodes <- 'y[c(2,4)]'
    Cans <- cm$calculate(nodes)
    ans <- m$calculate(nodes)
    check <- dnorm(cm$y[c(2, 4)], cm$mu, sqrt(cm$tau), log=TRUE) |> sum()
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    nodes <- c('y[2]','y[4]')   # Two instructions
    Cans <- cm$calculate(nodes)
    ans <- m$calculate(nodes)
    check <- dnorm(cm$y[c(2, 4)], cm$mu, sqrt(cm$tau), log=TRUE) |> sum()
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Check getLogProb
    Cans <- cm$getLogProb('y[1:4]')
    ans <- m$calculate('y[1:4]')
    check <- dnorm(cm$y[1:4], cm$mu, sqrt(cm$tau), log=TRUE) |> sum()
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    # Prepare for calculateDiff test below
    old_logProb <- dnorm(cm$y[3:4], cm$mu, sqrt(cm$tau), log=TRUE) |> sum()

    # Check simulate
    set.seed(1)
    cm$simulate('y[3:4]')
    set.seed(1)
    m$simulate('y[3:4]')
    expect_equal(cm$y, m$y)

    # Check getLogProb
    # Do this assignment in case the previous test of repeatability fails
    m$y[3:4] <- cm$y[3:4]
    Cans <- cm$calculateDiff('y[3:4]')
    ans <- m$calculateDiff('y[3:4]')
    new_logProb <- dnorm(cm$y[3:4], cm$mu, sqrt(cm$tau), log=TRUE) |> sum()
    check <- new_logProb - old_logProb
    expect_equal(Cans, ans)
    expect_equal(Cans, check)

    ## Override init value when creating model instance.
    mclass <- nimbleModel(code, data = data, inits = inits, returnClass = TRUE)
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

    ## Use of calc_1_matp
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
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,4),ncol=1)), newIndexRange(quote(1:3))),
                            varName = 'y')
    truth <- sum(dnorm(m$y[c(2,4),1:3], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(t(m$y[c(2,4),1:3])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(t(cm$y[c(2,4),1:3])), result)

    ## mat-mat
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,4),ncol=1)), newIndexRange(matrix(c(1,3), ncol=1))),
                            varName = 'y')
    truth <- sum(dnorm(m$y[c(2,4),c(1,3)], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(4)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(t(m$y[c(2,4),c(1,3)])), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(t(cm$y[c(2,4),c(1,3)])), result)

})

test_that("three index slots (plus different index variable ordering)", {
    code <- quote({
        for(i in 1:5)
            for(j in 1:4)
                for(k in 1:3)
                   y[k,j,i] ~ dnorm(0,1)  # This changes order of use of indices to [idx[3],idx[2],idx[1]].
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
    inds <- matrix(c(5,3,1, 1,3,4, 4,1,2), ncol=3, byrow=TRUE)
    vr <- varRangeClass$new(list(newIndexRange(inds)), rangeToIndexSlot = list(c(3,1,2)), varName = 'y')
    tmp <- vr$indexRanges[[1]]$values[,c(2,3,1)]
    inds <- tmp[order(tmp[,1],tmp[,2]),]    # Rows have been shuffled...
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
    for(i in 2:5)
        for(j in 1:4)
            for(k in 2:3)
                result[k,j,i] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(m$y[2:3,1:4,2:5], result[2:3,1:4,2:5])
    set.seed(1)
    cm$simulate(vr)
    expect_equal(cm$y[2:3,1:4,2:5], result[2:3,1:4,2:5])

    ## matp-seq (matp first because of k,j,i in model code)
    vr <- varRangeClass$new(list(newIndexRange(quote(2:3)),
                                 newIndexRange(matrix(c(4,1,1,3,4,5),ncol=2,byrow=TRUE))), varName = 'y')
    inds <- rbind(c(2,1,3),c(2,4,1),c(2,4,5),c(3,1,3),c(3,4,1),c(3,4,5))
    inds <- inds[order(inds[,2],inds[,3],inds[,1]),]
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds]), result)

    ## seq-matp
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(1,4,3,1,2,4),ncol=2,byrow=TRUE)),
                               newIndexRange(quote(4:5))), varName = 'y')
    inds <- rbind(c(1,4,4),c(3,1,4),c(2,4,4),c(1,4,5),c(3,1,5),c(2,4,5))
    inds <- inds[order(inds[,3],inds[,1],inds[,2]),]
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds]), result)

    ## matp-matp (matp first because of k,j,i in model code)
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(1,3),ncol=1)),
                                 newIndexRange(matrix(c(1,4,3,1,4,5),ncol=2,byrow=TRUE))), varName = 'y')
    inds <- rbind(c(1,1,4),c(1,3,1),c(1,4,5),c(3,1,4),c(3,3,1),c(3,4,5))
    inds <- inds[order(inds[,2],inds[,1],inds[,3]),]
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds]), result)

    ## matp-matp with reordering
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(1,4,3,1,3,2),ncol=2,byrow=TRUE)),
                               newIndexRange(matrix(c(3,5),ncol=1))), varName = 'y')
    inds <- rbind(c(1,4,3),c(3,1,3),c(3,2,3),c(1,4,5),c(3,1,5),c(3,2,5))
    inds <- inds[order(inds[,3],inds[,1]),]
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds]), result)

    # seq-mat-seq (3-generic)
    vr <- varRangeClass$new(list(newIndexRange(quote(2:3)), newIndexRange(matrix(c(1,4), ncol=1)),
                               newIndexRange(quote(2:5))), varName = 'y')
    truth <- sum(dnorm(m$y[2:3,c(1,4),2:5], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(3,4,5))
    for(i in 2:5)
        for(j in c(1,4))
            for(k in 2:3)
                result[k,j,i] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[2:3,c(1,4),2:5]), c(result[2:3,c(1,4),2:5]))
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[2:3,c(1,4),2:5]), c(result[2:3,c(1,4),2:5]))

})

test_that("four index slots", {
    code <- quote({
        for(i in 1:5)
            for(j in 1:4)
                for(k in 1:3)
                    for(l in 1:2)
                        y[i,j,k,l] ~ dnorm(0,1) 
    })
    data <- list(y = array(rnorm(120),c(5,4,3,2)))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()

    # all seq
    vr <- varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(quote(1:3)), newIndexRange(quote(1:3)),
                               newIndexRange(quote(1:2))), varName = 'y')
    truth <- sum(dnorm(m$y[2:5,1:3,1:3,1:2], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(5,4,3,2))
    for(i in 2:5)
        for(j in 1:3)
            for(k in 1:3)
                for(l in 1:2)
                result[i,j,k,l] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[2:5,1:3,1:3,1:2]), c(result[2:5,1:3,1:3,1:2]))
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[2:5,1:3,1:3,1:2]), c(result[2:5,1:3,1:3,1:2]))

    # seq-matp
    vr <- varRangeClass$new(list(newIndexRange(quote(2:4)),
                                 newIndexRange(matrix(c(5,1,2, 2,3,1),ncol=3,byrow=TRUE))),
                            rangeToIndexSlot = list(2, c(1,3,4)),
                            varName = 'y')
    inds <- rbind(c(2,2,3,1), c(2,3,3,1), c(2,4,3,1), c(5,2,1,2),c(5,3,1,2),c(5,4,1,2))
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(6)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds]), result)

    # seq-mat-mat-seq (4-generic)
    vr <- varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(matrix(c(2,4),ncol=1)), newIndexRange(matrix(c(1,3),ncol=1)),
                               newIndexRange(quote(1:2))), varName = 'y')
    truth <- sum(dnorm(m$y[2:5,c(2,4),c(1,3),1:2], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(5,4,3,2))
    for(i in 2:5)
        for(j in c(2,4))
            for(k in c(1,3))
                for(l in 1:2)
                result[i,j,k,l] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[2:5,c(2,4),c(1,3),1:2]), c(result[2:5,c(2,4),c(1,3),1:2]))
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[2:5,c(2,4),c(1,3),1:2]), c(result[2:5,c(2,4),c(1,3),1:2]))

})

test_that("five index slots", {
    code <- quote({
        for(i in 1:5)
            for(j in 1:4)
                for(k in 1:3)
                    for(l in 1:2)
                        for(m in 1:4)
                            y[i,j,k,l,m] ~ dnorm(0,1) 
    })
    data <- list(y = array(rnorm(480),c(5,4,3,2,4)))
    mclass <- nimbleModel(code, data = data, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()

    # all seq
    vr <- varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(quote(1:3)), newIndexRange(quote(1:3)),
                               newIndexRange(quote(1:2)), newIndexRange(quote(2:3))), varName = 'y')
    truth <- sum(dnorm(m$y[2:5,1:3,1:3,1:2,2:3], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- array(0, c(5,4,3,2,4))
    for(i in 2:5)
        for(j in 1:3)
            for(k in 1:3)
                for(l in 1:2)
                    for(mm in 2:3)
                        result[i,j,k,l,mm] <- rnorm(1)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[2:5,1:3,1:3,1:2,2:3]), c(result[2:5,1:3,1:3,1:2,2:3]))
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[2:5,1:3,1:3,1:2,2:3]), c(result[2:5,1:3,1:3,1:2,2:3]))

    # seq-matp-mat
    vr <- varRangeClass$new(list(newIndexRange(quote(2:4)),
                                 newIndexRange(matrix(c(4,1,2, 2,2,4),ncol=3,byrow=TRUE)),
                                newIndexRange(matrix(c(1,3),ncol=1))),
                            rangeToIndexSlot = list(1, c(2,4,5), 3),
                            varName = 'y')
    inds <- rbind(c(2,2,1,2,4),c(2,2,3,2,4),c(2,4,1,1,2),c(2,4,3,1,2),
                  c(3,2,1,2,4),c(3,2,3,2,4),c(3,4,1,1,2),c(3,4,3,1,2),
                  c(4,2,1,2,4),c(4,2,3,2,4),c(4,4,1,1,2),c(4,4,3,1,2))
    truth <- sum(dnorm(m$y[inds], log=TRUE))
    expect_equal(m$calculate(vr), truth)
    expect_equal(cm$calculate(vr), truth)

    set.seed(1)
    result <- rnorm(12)
    set.seed(1)
    m$simulate(vr)
    expect_equal(c(m$y[inds]), result)
    set.seed(1)
    cm$simulate(vr)
    expect_equal(c(cm$y[inds]), result)

})

test_that("calculate with index offset and switched indices", {
  code <- nimbleCode({
    for(i in 1:3)
      for(j in 2:4)
        y[j+1,i] ~ dnorm(x[j], 1)
  })

  set.seed(1)
  mclass <- nimbleModel(code, data = list(y = matrix(rnorm(15),5)), inits = list(x = rnorm(4)),
                        returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompile(mclass)
  cm <- cmclass$new()
  truth <- dnorm(m$y[3:5,1:3], m$x[2:4], 1, log = TRUE)
  expect_equal(m$calculate(), sum(truth))
  expect_equal(cm$calculate(), sum(truth))
  expect_equal(m$logProb_y[3:5,], truth)
  expect_equal(cm$logProb_y[3:5,], truth)
  expect_equal(m$getLogProb('y[5,2]'), dnorm(m$y[5,2], m$x[4],1,log=TRUE))
  expect_equal(cm$getLogProb('y[5,2]'), dnorm(cm$y[5,2], cm$x[4],1,log=TRUE))
  
})

test_that("calculate works correctly for singleton in vector", {
  code <- nimbleCode({
    y[2] ~ dnorm(0,1)
  })
  set.seed(1)
  mclass <- nimbleModel(code, data = list(y = rnorm(2)), returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompile(mclass)
  cm <- cmclass$new()
  truth <- dnorm(m$y[2], 0, 1, log = TRUE)
  expect_equal(m$calculate(), truth)
  expect_equal(m$calculate('y'), truth)
  expect_equal(m$calculate('y[2]'), truth)
  expect_equal(m$logProb_y[2], truth)
  expect_equal(cm$calculate(), truth)
  expect_equal(cm$calculate('y'), truth)
  expect_equal(cm$calculate('y[2]'), truth)
  expect_equal(cm$logProb_y[2], truth)
})

test_that("calculate/simulate work correctly for deterministic node", {
  code <- nimbleCode({
    y <- 3 + x
  })
  set.seed(1)
  mclass <- nimbleModel(code, inits = list(x = 1.5), returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompile(mclass)
  cm <- cmclass$new()

  m$calculate('y')  
  expect_identical(m$y, 4.5)
  m$y <- 0
  m$simulate('y')
  expect_identical(m$y, 4.5)

  cm$calculate('y')  
  expect_identical(cm$y, 4.5)
  cm$y <- 0
  cm$simulate('y')
  expect_identical(cm$y, 4.5)
})



test_that("calculate works correctly for time series/SSM recursion", {
  library(nimbleModel); library(testthat); library(nCompiler)
  code <- nimbleCode({
    for(i in 3:6) {
      y[i] <- y[i-1] + 1.5
    }
    y[2] <- 1
  })
  mclass <- nimbleModel(code, returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompile(mclass)
  cm <- cmclass$new()
  m$calculate()
  cm$calculate()
  truth <- c(NA, 1, 2.5, 4, 5.5, 7)
  expect_identical(m$y, truth)
  truth[1] <- 0
  expect_equal(cm$y, truth)
    

  code <- nimbleCode({
    for(i in 2:5) {
      y[i+1] <- y[i] + 1.5
    }
    y[2] <- 1
  })
  mclass <- nimbleModel(code, returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompile(mclass)
  cm <- cmclass$new()
  m$calculate()
  cm$calculate()
  truth <- c(NA, 1, 2.5, 4, 5.5, 7)
  expect_identical(m$y, truth)
  truth[1] <- 0
  expect_equal(cm$y, truth)

  code <- nimbleCode({
    for(i in 3:7) {
      y[i] <- y[i-2] + 1.5
    }
  })
  mclass <- nimbleModel(code, returnClass = TRUE, inits=list(y=c(1,3,rep(0,5))))
  m <- mclass$new()
  cmclass <- nCompile(mclass)
  cm <- cmclass$new()
  m$calculate()
  cm$calculate()
  truth <- c(1, 3, 2.5, 4.5, 4, 6, 5.5)
  expect_identical(m$y, truth)
  expect_equal(cm$y, truth)


  code <- nimbleCode({
    for(i in 2:3)
      for(j in 2:4)
        y[i,j] <- y[i-1,j] + y[i,j-1] + 1.5
    for(j in 1:4)
      y[1,j] <- 0
    for(i in 1:3)
      y[i,1] <- 0
  })
  mclass <- nimbleModel(code, returnClass = TRUE)
  m <- mclass$new()
  cmclass <- nCompile(mclass)
  cm <- cmclass$new()
  m$calculate()
  cm$calculate()
  truth <- matrix(0, 3, 4)
  truth[2,2:4] <- c(1.5,3,4.5)
  truth[3,2:4] <- c(3,7.5,13.5)
  expect_identical(m$y, truth)
  expect_equal(cm$y, truth)

  # Work on these once we address issue #25.
  if(FALSE) {    
    code <- nimbleCode({
      for(i in 1:5)
        y[i] ~ dnorm(y[i+1], 1)
      # y[6] <- 0
    })
    mclass <- nimbleModel(code, returnClass = TRUE)
    m <- mclass$new()
    cmclass <- nCompile(mclass)
    cm <- cmclass$new()
    # BUG - calculation done in wrong order.
    m$calculate()
    cm$calculate()
    
    
    ## This interleaves the sortID values across different calcRules/ranges.
    code <- nimbleCode({
      for(i in 1:10)
        y[i] ~ dnorm(z[i], 1)
      for(i in 2:10)
        z[i] <- y[i-1]
    })
    mclass <- nimbleModel(code, returnClass = TRUE)
    m <- mclass$new()
    # BUG: need to interweave calculations.
  }  
})
  
 
 

test_that("basic creation of list of instr_nClass objects", {

    code <- quote({
        mu ~ dnorm(0, 1)
        for(i in 1:5) 
            y[i] ~ dnorm(mu, 1)
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
    expect_identical(instr1$values[[1]], 3) # offset
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

    ## This does some testing of multiple index cases but could be fleshed out further.
    code <- quote({
        mu ~ dnorm(0, 1)
        for(i in 1:5)
            for(j in 1:4)
                y[i,j] ~ dnorm(mu, 1)
    })

    data <- list(y = matrix(rnorm(20), 5))
    m <- nimbleModel(code, data = data)
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(quote(1:3))), varName = 'y'))[[1]]
    expect_identical(instr$type, 4)

    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(matrix(c(1,4),ncol=1))), varName = 'y'))[[1]]
    expect_identical(instr$type, 5)
    
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(1,4),ncol=1)), newIndexRange(quote(2:5))), varName = 'y'))[[1]]
    expect_identical(instr$type, 6)
    
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(1,4),ncol=1)), newIndexRange(matrix(c(2,4),ncol=1))), varName = 'y'))[[1]]
    expect_identical(instr$type, 7)

    ## order is shuffled to put first index slot in first range
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(matrix(c(1,4),ncol=1))),
                                                rangeToIndexSlot=list(2,1), varName = 'y'))[[1]]
    expect_identical(instr$type, 6)
    expect_identical(instr$slots, c(1,2))
    expect_identical(instr$index_types, c(2,1))

    ## order is shuffled to put first index slot in first range
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(1,4),ncol=1)), newIndexRange(quote(2:5))),
                                                rangeToIndexSlot=list(2,1), varName = 'y'))[[1]]
    expect_identical(instr$type, 5)
    expect_identical(instr$slots, c(1,2))
    expect_identical(instr$index_types, c(1,2))

    ## Reverse indexing in declaration: decl fun will have y[idx[2],idx[1]],
    ## And instr objects will have ordering shuffled so that idx[1] is first despite being second index in declaration.
    code <- quote({
        mu ~ dnorm(0, 1)
        for(i in 1:5)
            for(j in 1:4)
                y[j,i] ~ dnorm(mu, 1)
    })

    data <- list(y = matrix(rnorm(20), 4))
    m <- nimbleModel(code, data = data)
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(1:3)), newIndexRange(quote(2:5))), varName = 'y'))[[1]]
    expect_identical(instr$type, 4)

    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(1:3)), newIndexRange(matrix(c(2,5),ncol=1))), varName = 'y'))[[1]]
    expect_identical(instr$type, 6)  # shuffled
    
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(1,4),ncol=1)), newIndexRange(quote(2:5))), varName = 'y'))[[1]]
    expect_identical(instr$type, 5)  # shuffled
    
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(1,4),ncol=1)), newIndexRange(matrix(c(2,5),ncol=1))), varName = 'y'))[[1]]
    expect_identical(instr$type, 7)

    ## order is shuffled to put first index slot in first range
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(matrix(c(1,4),ncol=1))),
                                                rangeToIndexSlot=list(2,1), varName = 'y'))[[1]]
    expect_identical(instr$type, 5)
    expect_identical(instr$slots, c(1,2))
    expect_identical(instr$index_types, c(1,2))

    ## order is shuffled to put first index slot in first range
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(1,5),ncol=1)), newIndexRange(quote(2:4))),
                                                rangeToIndexSlot=list(2,1), varName = 'y'))[[1]]
    expect_identical(instr$type, 6)
    expect_identical(instr$slots, c(1,2))
    expect_identical(instr$index_types, c(2,1))

    
    code <- quote({
        mu ~ dnorm(0, 1)
        for(i in 1:5)
            for(j in 1:4)
                for(k in 1:3)
                    y[i,j,k] ~ dnorm(mu, 1)
    })

    data <- list(y = array(rnorm(60),c(5,4,3)))
    m <- nimbleModel(code, data = data)
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(matrix(c(1,2,2,3),ncol=2,byrow=TRUE))),
                                                varName = 'y'))[[1]]
    expect_identical(instr$slots, c(1,2,3))
    expect_identical(instr$index_types, c(1,2))
    expect_identical(instr$values[[2]], c(1,2,2,3))
    expect_identical(instr$type, 8) # seq_matp

    data <- list(y = array(rnorm(60),c(5,4,3)))
    m <- nimbleModel(code, data = data)
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(quote(1:2)),
                                                     newIndexRange(quote(1:2))), 
                                                varName = 'y'))[[1]]
    expect_identical(instr$type, 12) # allseq


    ## indexRange order is shuffled to put first index slot in first indexRange.
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(matrix(c(1,2,2,3),ncol=2,byrow=TRUE))),
                                                varName = 'y', rangeToIndexSlot = list(2,c(1,3))))[[1]]
    expect_identical(instr$slots, c(1,3,2))  # shuffled
    expect_identical(instr$index_types, c(2,1))
    expect_identical(instr$values[[1]], c(1,2,2,3))
    expect_identical(instr$type, 9) # seq_matp

    ## order within the matrix indexRange is shuffled to be ascending.
    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(quote(2:5)), newIndexRange(matrix(c(1,2,2,3),ncol=2,byrow=TRUE))),
                                                varName = 'y', rangeToIndexSlot = list(2,c(3,1))))[[1]]
    expect_identical(instr$slots, c(1,3,2))  # shuffled
    expect_identical(instr$index_types, c(2,1))
    expect_identical(instr$values[[1]], c(2,1,3,2))  # shuffled
    expect_identical(instr$type, 9) # seq_matp

    instr <- makeInstrList(m, varRangeClass$new(list(newIndexRange(matrix(c(1,3),ncol=1)), newIndexRange(matrix(c(1,2,2,3),ncol=2,byrow=TRUE))),
                                                varName = 'y'))[[1]]
    expect_identical(instr$type, 10)

    ## Check technique of building and copying nList(instr_nClass) as a method.
    instr_nClass <- nimbleModel:::instr_nClass  # work-around for scoping
    code <- quote({
        for(i in 1:5) {
            mu ~ dnorm(0, 1)
            y[i] ~ dnorm(mu, 1)
        }
    })

    data <- list(y = rnorm(5))

    cm <- nimbleModel(code, data = data, compile = TRUE)
    instrList <- makeInstrList(cm, 'y[2:5]')
    cinstrList <- cm$makeCompiledInstrList(instrList)
    expect_true(cinstrList$isCompiled())
    expect_true(inherits(cinstrList, 'nList'))
    expect_identical(length(cinstrList), 1L)
    cinstr <- cinstrList[[1]]
    expect_true(cinstr$isCompiled())
    expect_identical(cinstr$nDim, 1L)
    expect_identical(cinstr$dims, 1L)
    expect_identical(cinstr$lens, 4L)
    expect_identical(cinstr$values[[1]], 2L)
    

})

