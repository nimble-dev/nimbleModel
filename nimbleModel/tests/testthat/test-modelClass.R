test_that("dataRules determination works", {
    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(mu, sigma)
        mu ~ dnorm(0, 1)
        y[10] ~ dnorm(0, 1)
        y[11] ~ dnorm(0, 1)
        z ~ dnorm(0, 1)
    })
    data = list(y=c(rnorm(10),NA,3.5), z = 3.5)
    m <- modelClass$new(code, data = data)
    expect_length(m$dataRules, 2)
    expect_length(m$dataRules[[1]]$rules, 2)
    expect_length(m$dataRules[[2]]$rules, 1)
})

test_that("predictive/nonpredictive rule determination works", {
    ## basic cases
    
    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(mu, sigma)
        mu ~ dnorm(0, 1)
    })
    data = list(y=rnorm(5))
    m <- modelClass$new(code, data = data)
    expect_identical(m$predictiveRules, NULL)
    expect_length(m$nonpredictiveRules, 2)
    expect_identical(names(m$nonpredictiveRules), c('mu','y'))
    

    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(mu, sigma)
        mu ~ dnorm(0, 1)
        sigma <- exp(logsigma)
        logsigma ~ dunif(0, 1)
        ## predictive nodes:
        z ~ dnorm(mu, exp(logsigma))
        w_mean <- theta + 1
        w ~ dnorm(w_mean, 1)
        theta ~ dnorm(phi, 1)
    })
    data = list(y=rnorm(5))
    m <- modelClass$new(code, data = data)

    expect_length(m$predictiveRules, 3)
    expect_length(m$predictiveRules$w$rules, 1)
    expect_length(m$predictiveRules$z$rules, 1)
    expect_length(m$predictiveRules$theta$rules, 1)
    expect_equal(m$predictiveRules$w$rules[[1]]$fullRange,
                     varRangeClass$new(list(), varName = 'w'))
    expect_equal(m$predictiveRules$z$rules[[1]]$fullRange,
                     varRangeClass$new(list(), varName = 'z'))
    expect_equal(m$predictiveRules$theta$rules[[1]]$fullRange,
                 varRangeClass$new(list(), varName = 'theta'))

    expect_length(m$nonpredictiveRules, 3)
    expect_identical(names(m$nonpredictiveRules), c('mu','logsigma','y'))
    expect_length(m$nonpredictiveRules$mu$rules, 1)
    expect_length(m$nonpredictiveRules$logsigma$rules, 1)
    expect_length(m$nonpredictiveRules$y$rules, 1)
    expect_equal(m$nonpredictiveRules$mu$rules[[1]]$fullRange,
                     varRangeClass$new(list(), varName = 'mu'))
    expect_equal(m$nonpredictiveRules$logsigma$rules[[1]]$fullRange,
                     varRangeClass$new(list(), varName = 'logsigma'))
    expect_equal(m$nonpredictiveRules$y$rules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y'))
    

    ## more complicated case
    code <- quote({
        for(i in 1:10) {
            z[i] ~ dnorm(mu_z[i], 1)  # partly predictive
            mu_z[i] ~ dnorm(theta_z, 1)  # partly predictive
        }
        for(i in 1:10) {
            y[i] ~ dnorm(mu_y[i], 1)
            mu_y[i] ~ dnorm(theta_y, 1)
        }
        for(i in 1:10) {
            w[i] ~ dnorm(mu_w[i], 1)  # predictive
            mu_w[i] ~ dnorm(theta_w, 1) # predictive
        }
        theta_y ~ dnorm(theta, 1)
        theta_w ~ dnorm(theta, 1) # predictive
        theta_z ~ dnorm(theta, 1)
    })
    y <- z <- w <- rnorm(1:10)
    w[1:10] <- NA
    z[6:8] <- NA
    m <- modelClass$new(code, data = list(z = z, y = y, w = w))

    expect_length(m$dataRules, 2)

    expect_length(m$predictiveRules, 5)
    expect_equal(m$predictiveRules$w$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(1:10))), varName = 'w'))
    expect_equal(m$predictiveRules$mu_w$rules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:10))), varName = 'mu_w'))
    expect_equal(m$predictiveRules$z$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(6:8))), varName = 'z'))
    expect_equal(m$predictiveRules$mu_z$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(6:8))), varName = 'mu_z'))
    expect_equal(m$predictiveRules$theta_w$rules[[1]]$fullRange,
                 varRangeClass$new(list(), varName = 'theta_w'))

    expect_length(m$nonpredictiveRules, 6)
    expect_equal(m$nonpredictiveRules$z$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'z'))
    expect_equal(m$nonpredictiveRules$z$rules[[2]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(9:10))), varName = 'z'))
    expect_equal(m$nonpredictiveRules$mu_z$rules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'mu_z'))
    expect_equal(m$nonpredictiveRules$mu_z$rules[[2]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(9:10))), varName = 'mu_z'))
    expect_equal(m$nonpredictiveRules$y$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(1:10))), varName = 'y'))
    expect_equal(m$nonpredictiveRules$mu_y$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(1:10))), varName = 'mu_y'))
    expect_equal(m$nonpredictiveRules$theta_y$rules[[1]]$fullRange,
                 varRangeClass$new(list(), varName = 'theta_y'))
    expect_equal(m$nonpredictiveRules$theta_z$rules[[1]]$fullRange,
                 varRangeClass$new(list(), varName = 'theta_z'))
    
    
    ## case with dataRule split into two
    code <- quote({
        for(i in 1:50) {
            y[i] ~ dnorm(mu[i], sigma)
            mu[i] ~ dnorm(mu0, 1)
        }
        mu0 ~ dnorm(0, 1)
    })
    data = list(y=rnorm(50))
    data$y[20] <- NA
    data$y[48:50] <- NA
    m <- modelClass$new(code, data = data)
    expect_length(m$predictiveRules, 2)
    expect_identical(names(m$predictiveRules), c('y','mu'))
    expect_length(m$predictiveRules[[1]]$rules, 2)
    expect_length(m$predictiveRules[[2]]$rules, 2)
    expect_equal(m$predictiveRules$y$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(20)), varName = 'y'))
    expect_equal(m$predictiveRules$y$rules[[2]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(48:50))), varName = 'y'))
    expect_equal(m$predictiveRules$mu$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(20)), varName = 'mu'))
    expect_equal(m$predictiveRules$mu$rules[[2]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(48:50))), varName = 'mu'))


    expect_length(m$nonpredictiveRules, 3)
    expect_identical(names(m$nonpredictiveRules), c('mu0','y', 'mu'))
    expect_length(m$nonpredictiveRules[[1]]$rules, 1)
    expect_length(m$nonpredictiveRules[[2]]$rules, 2)
    expect_length(m$nonpredictiveRules[[3]]$rules, 2)

    expect_equal(m$nonpredictiveRules$y$rules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:19))), varName = 'y'))
    expect_equal(m$nonpredictiveRules$y$rules[[2]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(21:47))), varName = 'y'))
    expect_equal(m$nonpredictiveRules$mu$rules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1:19))), varName = 'mu'))
    expect_equal(m$nonpredictiveRules$mu$rules[[2]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(21:47))), varName = 'mu'))
    
})

test_that("getNodes with data or predictive works", {

    code <- quote({
        for(i in 1:10) {
            z[i] ~ dnorm(mu_z[i], 1)  # partly predictive
            mu_z[i] ~ dnorm(theta_z, 1)  # partly predictive
        }
        for(i in 1:10) {
            y[i] ~ dnorm(mu_y[i], 1)
            mu_y[i] ~ dnorm(theta_y, 1)
        }
        for(i in 1:10) {
            w[i] ~ dnorm(mu_w[i], 1)  # predictive
            mu_w[i] ~ dnorm(theta_w, 1) # predictive
        }
        theta_y ~ dnorm(theta, 1)
        theta_w ~ dnorm(theta, 1) # predictive
        theta_z ~ dnorm(theta, 1)
    })
    y <- z <- w <- rnorm(1:10)
    w[1:10] <- NA
    z[6:8] <- NA
    m <- modelClass$new(code, data = list(z = z, y = y, w = w))

    base <- getNodes(m)
    expect_length(base, 9)
    baseNames <- sapply(base, nimbleModel:::getVarName)

    result <- getNodes(m, includeData = FALSE)
    resultNames <- sapply(result, nimbleModel:::getVarName)
    for(nm in c('mu_z','w','mu_w','mu_y','theta_y','theta_w','theta_z')) {
        expect_equal(base[[which(nm == baseNames)]], result[[which(nm == resultNames)]])
    }
    
    expect_length(result, 8)
    expect_false('y' %in% resultNames)
    expect_equal(result[[which(resultNames == 'z')]]$indexRanges,
                 list(newIndexRange(quote(6:8))))
        
    result <- getNodes(m, dataOnly = TRUE)
    resultNames <- sapply(result, nimbleModel:::getVarName)
    expect_length(result, 2)
    expect_equal(result[[which(resultNames == 'y')]]$indexRanges,
                 list(newIndexRange(quote(1:10))))
    expect_equal(result[[which(resultNames == 'z')]]$indexRanges,
                 list(newIndexRange(matrix(c(1:5,9:10), ncol=1))))

    result <- getNodes(m, predictiveOnly = TRUE)
    resultNames <- sapply(result, nimbleModel:::getVarName)
    expect_length(result, 5)
    expect_equal(result[[which(resultNames == 'z')]]$indexRanges,
                 list(newIndexRange(quote(6:8))))
    expect_equal(result[[which(resultNames == 'mu_z')]]$indexRanges,
                 list(newIndexRange(quote(6:8))))
    expect_equal(result[[which(resultNames == 'w')]]$indexRanges,
                 list(newIndexRange(quote(1:10))))
    expect_equal(result[[which(resultNames == 'mu_w')]]$indexRanges,
                 list(newIndexRange(quote(1:10))))
    expect_length(result[[which(resultNames == 'theta_w')]]$indexRanges, 0)

    result <- getNodes(m, includePredictive = FALSE)
    resultNames <- sapply(result, nimbleModel:::getVarName)
    expect_length(result, 8)
    expect_equal(result[[which(resultNames == 'z')][1]]$indexRanges,
                 list(newIndexRange(quote(1:5))))
    expect_equal(result[[which(resultNames == 'z')][2]]$indexRanges,
                 list(newIndexRange(quote(9:10))))
    expect_equal(result[[which(resultNames == 'mu_z')][1]]$indexRanges,
                 list(newIndexRange(quote(1:5))))
    expect_equal(result[[which(resultNames == 'mu_z')][2]]$indexRanges,
                 list(newIndexRange(quote(9:10))))
     expect_equal(result[[which(resultNames == 'y')][2]]$indexRanges,
                 list(newIndexRange(quote(1:10))))
     expect_equal(result[[which(resultNames == 'mu_y')][2]]$indexRanges,
                 list(newIndexRange(quote(1:10))))
    expect_length(result[[which(resultNames == 'theta_y')]]$indexRanges, 0)
    expect_length(result[[which(resultNames == 'theta_z')]]$indexRanges, 0)
   

    ## "interaction" of top/latent with data/predictive.
    code <- quote({
        for(i in 1:40) {
            mu[i] ~ dnorm(mu0[i], 1)
            y[i] ~ dnorm(mu[i], 1)
        }
        for(i in 1:20)
            mu0[i] ~ dnorm(0, 1)
    })
    m <- modelClass$new(code, data = list(y = c(rnorm(10),rep(NA, 20), rnorm(10))))

    base <- getNodes(m)
    expect_length(base, 3)
    baseNames <- sapply(base, nimbleModel:::getVarName)

    result <- getNodes(m, includePredictive = FALSE, latentOnly = TRUE)
    expect_length(result, 1)
    expect_equal(result[[which(resultNames == 'mu')]]$indexRanges,
                 list(newIndexRange(quote(1:10))))

    result <- getNodes(m, includePredictive = FALSE, topOnly = TRUE)
    expect_length(result, 2)
    expect_equal(result[[which(resultNames == 'mu0')]]$indexRanges,
                 list(newIndexRange(quote(1:10))))
    expect_equal(result[[which(resultNames == 'mu')]]$indexRanges,
                 list(newIndexRange(quote(31:40))))

    result <- getNodes(m, predictiveOnly = TRUE, latentOnly = TRUE)
    expect_length(result, 1)
    expect_equal(result[[which(resultNames == 'mu')]]$indexRanges,
                 list(newIndexRange(quote(11:20))))
    
    result <- getNodes(m, predictiveOnly = TRUE, topOnly = TRUE)
    expect_length(result, 2)
    expect_equal(result[[which(resultNames == 'mu')]]$indexRanges,
                 list(newIndexRange(quote(21:30))))
    expect_equal(result[[which(resultNames == 'mu0')]]$indexRanges,
                 list(newIndexRange(quote(11:20))))


    ## mv nodes with mixtures of data/nondata
    code <- quote({
        for(i in 1:3) 
            y[i,1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
    })

    #debugonce(nimbleModel:::excludeFromPredictiveRules)
    
    m <- modelClass$new(code, data = list(y = matrix(c(1,2,NA,3,4,NA,rep(NA,3)), 3, 3)))

    base <- getNodes(m)

    result <- getNodes(m, dataOnly = TRUE)
    
})

    code <- quote({
        for(i in 1:3) 
            y[i,1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
    })
    m <- modelClass$new(code)
