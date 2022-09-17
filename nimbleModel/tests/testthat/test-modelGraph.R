test_that("setting top nodes works", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    context_i <- modelContextClass$new(list(singleContext1))

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(0,1)), 1, context_i)
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), TRUE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(mu, sigma)), 1, context_i, constants = list(mu = 0, sigma = 1))
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), TRUE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(c[i],1)), 1, context_i, constants = list(c = rnorm(7)))
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), TRUE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(mu, sigma)), 1, context_i, constants = list(mu = 0))
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), FALSE)

    declRule <- declRuleClass$new(quote(y[i] ~ dnorm(c[i],1)), 1, context_i)
    calcRule <- calcRuleClass$new(declRule, NULL, 1, declRule$context)

    calcRule$setTop()
    expect_identical(calcRule$is_type('top'), FALSE)

    ## TODO: add some more complicated cases
})

## TODO: be careful that probably will not have replaced constants
## at point we start testing here.

test_that("graph processing for basic model works", {
    code <- quote({
        y ~ dnorm(mu, sigma)
        mu ~ dnorm(mu0, 1)
        sigma ~ dunif(0,1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    expect_identical(length(modelDef$downstreamRules), 3L)
    expect_identical(length(modelDef$downstreamRules[['mu0']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['mu']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['sigma']]), 1L)
    expect_identical(modelDef$downstreamRules[['mu0']][[1]]$childVar, "mu")
    expect_identical(modelDef$downstreamRules[['mu']][[1]]$childVar, "y")
    expect_identical(modelDef$downstreamRules[['sigma']][[1]]$childVar, "y")

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))
    
    expect_identical(modelDef$calcRules[['sigma']][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[['sigma']][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[['sigma']][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[['sigma']][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[['sigma']][[1]]$ID, getElement(ids, "sigma"))
    expect_identical(modelDef$calcRules[['sigma']][[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[['sigma']][[1]]$children, getElement(ids, 'y'))
                     
    expect_identical(modelDef$calcRules[['y']][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[['y']][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[['y']][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[['y']][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[['y']][[1]]$ID, getElement(ids, "y"))
    expect_identical(sort(modelDef$calcRules[['y']][[1]]$parents),
                     sort(c(getElement(ids, "sigma"), getElement(ids, "mu"))))
    expect_identical(modelDef$calcRules[['y']][[1]]$children, numeric(0))

    expect_identical(modelDef$calcRules[['mu']][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[['mu']][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[['mu']][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[['mu']][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[['mu']][[1]]$ID, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[['mu']][[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[['mu']][[1]]$children, getElement(ids, 'y'))

    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[['mu0']][[1]]$varName, "mu0")
})
   
test_that("graph processing for basic model with deterministic nodes works", {
    code <- quote({
        y ~ dnorm(phi, sigma)
        phi <- mu + 1
        mu ~ dnorm(mu0, 1)
        mu0 <- mu00 + 1
        sigma ~ dunif(0,1)
        z <- y + 3
    })
  
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()
    
    expect_identical(length(modelDef$downstreamRules), 6L)
    
    expect_identical(length(modelDef$downstreamRules[['mu00']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['mu0']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['mu']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['phi']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['sigma']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['y']]), 1L)

    expect_identical(modelDef$downstreamRules[['mu00']][[1]]$childVar, "mu0")
    expect_identical(modelDef$downstreamRules[['mu0']][[1]]$childVar, "mu")
    expect_identical(modelDef$downstreamRules[['mu']][[1]]$childVar, "phi")
    expect_identical(modelDef$downstreamRules[['phi']][[1]]$childVar, "y")
    expect_identical(modelDef$downstreamRules[['sigma']][[1]]$childVar, "y")
    expect_identical(modelDef$downstreamRules[['y']][[1]]$childVar, "z")

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    var <- 'sigma'
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]][[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, 'y'))

    var <- 'mu0'
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]][[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, "mu"))

    var <- 'mu'
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, "mu0"))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, "phi"))

    var <- "phi"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]][[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, "y"))

    var <- "y"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 4)
    expect_identical(modelDef$calcRules[[var]][[1]]$ID, getElement(ids, var))
    expect_identical(sort(modelDef$calcRules[[var]][[1]]$parents),
                     sort(c(getElement(ids, "phi"), getElement(ids, "sigma"))))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, "z"))

    var <- "z"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 5)
    expect_identical(modelDef$calcRules[[var]][[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, numeric(0))

    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[['mu00']][[1]]$varName, "mu00")
    
})

test_that("graph processing for model with various parents in a declaration works", {
    code <- quote({
        y ~ dnorm(mu + z, sigma)
        mu ~ dnorm(0, 1)
        sigma ~ dunif(0,1)
        z ~ dnorm(0, 1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    expect_identical(length(modelDef$downstreamRules), 3L)
    expect_identical(length(modelDef$downstreamRules[['mu']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['z']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['sigma']]), 1L)
    expect_identical(modelDef$downstreamRules[['mu']][[1]]$childVar, "y")
    expect_identical(modelDef$downstreamRules[['z']][[1]]$childVar, "y")
    expect_identical(modelDef$downstreamRules[['sigma']][[1]]$childVar, "y")

    for(wh in c('sigma','mu','z'))
        expect_identical(modelDef$calcRules[[wh]][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[['y']][[1]]$sortID, 2)
})


test_that("graph processing with split latent node", {
    code <- quote({
        y ~ dnorm(0, 1)
        z[2] ~ dnorm(y, 1)
        z[1] ~ dnorm(0, 1)
        ## z used as RHS of two declarations, one stoch, one determ
        mu[7:9] <- z[1:3]
        for(i in 3:4)
            mu[i] ~ dnorm(z[i-1], 1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    expect_identical(length(modelDef$downstreamRules), 2L)
    
    expect_identical(length(modelDef$downstreamRules[['y']]), 1L)
    expect_identical(length(modelDef$downstreamRules[['z']]), 2L)

    expect_identical(modelDef$downstreamRules[['y']][[1]]$childVar, "z")
    expect_identical(modelDef$downstreamRules[['z']][[1]]$childVar, "mu")
    expect_identical(modelDef$downstreamRules[['z']][[2]]$childVar, "mu")

    expect_identical(length(modelDef$calcRules), 3L)

    var <- "z"

    ## z[1] (some hard-coding here)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, "4")

    ## z[2] (some hard-coding here)
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[2]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[2]]$parents, "1")
    expect_identical(modelDef$calcRules[[var]][[2]]$children, c("4", "6"))
    
    
    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$varName, "z")
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(3)))

})

test_that("graph processing with split LHS node", {
    code <- quote({
        for(i in 1:10) 
            y[i] ~ dnorm(mu[i], 1)
        for(i in 3:4)
            z[i] ~ dnorm(y[i], 1)      
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    expect_identical(length(modelDef$calcRules), 2L)

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))
    
    var <- "z"
    expect_identical(length(modelDef$calcRules[[var]]), 1L)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, "3")
    expect_identical(modelDef$calcRules[[var]][[1]]$children, numeric(0))

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]), 3L)

    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, 'z'))
    
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[2]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[2]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[2]]$children, numeric(0))

    expect_identical(modelDef$calcRules[[var]][[3]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[3]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[3]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[3]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[3]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[3]]$children, numeric(0))

})

test_that("graph processing with triangular dependency structure", {
    code <- quote({
        w ~ dnorm(theta, 1)
        mu ~ dnorm(theta, 1)
        y ~ dnorm(theta + mu, 1)
        theta ~ dnorm(0, 1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 4L)

    var <- "theta"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))
    expect_identical(sort(modelDef$calcRules[[var]][[1]]$children),
                     sort(c(getElement(ids, 'w'), getElement(ids, 'y'),  getElement(ids, 'mu'))))

    var <- "mu"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, "theta"))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, 'y'))
    
    var <- "y"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 3)
    expect_identical(sort(modelDef$calcRules[[var]][[1]]$parents),
                     sort(c(getElement(ids, "theta"), getElement(ids, 'mu'))))

    var <- "w"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, "theta"))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, numeric(0))
 })


test_that("graph processing and top/end nodes with deterministic nodes", {
    code <- quote({
        z <- z0
        w <- z
        theta ~ dnorm(w, 1)
        y <- theta + 3
        y2 ~ dnorm(theta, 1)
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    var <- "z"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)
    
    var <- "w"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
 
    var <- "theta"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 3)
 
    var <- "y"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 4)

    var <- "y2"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 4)

    code <- quote({
        z <- z0
        w ~ dnorm(0, 1)
        theta ~ dnorm(w, z)
        y <- theta + 3
        y2 <- y + 1
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    var <- "z"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)

    var <- "w"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)

    var <- "theta"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)

    var <- "y"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 3)

    var <- "y2"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 4)

})

test_that("graph processing with various types of multivariate nodes", {
    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        mu[1:2] ~ dmnorm(z[1:2], pr[1:2,1:2])
        mu[3] <- 5
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]), 1L)
    
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[1]]$children, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, c('1','3'))

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]), 2L)

    for(i in 1:2) {
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('top'), TRUE)
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('end'), FALSE)
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]][[i]]$sortID, 1)
        expect_identical(modelDef$calcRules[[var]][[i]]$children, getElement(ids, "y"))
        expect_identical(modelDef$calcRules[[var]][[i]]$parents, numeric(0))
    }

    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        mu[1:4] ~ dmnorm(z[1:4], pr[1:4,1:4])
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]), 1L)
    
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[1]]$children, numeric(0))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, 'mu'))

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]), 1L)

    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))
    
    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        z[1:2] <- y[1:2]
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]), 1L)
    
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, 'z'))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))

    var <- "z"
    expect_identical(length(modelDef$calcRules[[var]]), 1L)

    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, numeric(0))
    
    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        z[1:4] <- y[1:4]
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "z"
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]][[1]]$children, numeric(0))
    
    var <- "y"
    expect_identical(modelDef$calcRules[[var]][[1]]$children, getElement(ids, "z"))
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, numeric(0))

    expect_identical(length(modelDef$rhsOnlyRules), 3L)
    expect_identical(modelDef$rhsOnlyRules[[3]][[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(4)))
})

test_that("graph processing for complicated multiple mv LHS nodes", {
    code <- quote({
        for(i in 1:4)
            y[i, 1:3] ~ dmnorm(mu[1:3, i], pr[1:3,1:3])
        mu[1:2, 1:2] ~ dwish(pr[1:2,1:2], 5)
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    tmp <- unlist(modelDef$calcRules)
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 2L)
    expect_identical(length(modelDef$calcRules[['y']]), 2L)
    expect_identical(length(modelDef$calcRules[['mu']]), 1L)
    
    expect_identical(modelDef$calcRules[['y']][[1]]$parents, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[['y']][[2]]$parents, numeric(0))
    
    expect_identical(length(modelDef$rhsOnlyRules), 2L)
    expect_is(modelDef$rhsOnlyRules[['mu']][[1]]$externalRules$indexRules[[1]],
              'indexRuleClass_arbitrary')

    
    code <- quote({
        for(i in 1:3)
            mu[i, 1:3] ~ dmnorm(mu0[1:3, i], pr[1:3,1:3])
        mu0[1, 1:2] ~ dmnorm(zero[1:2], pr[1:2, 1:2])
        y ~ dnorm(mu[1,1], 1)
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]), 3L)

    expect_identical(modelDef$calcRules[[var]][[1]]$getFullRange()$indexRanges,
                     list(indexRange(1), indexRange(quote(1:3))))
    expect_identical(modelDef$calcRules[[var]][[2]]$getFullRange()$indexRanges,
                     list(indexRange(2), indexRange(quote(1:3))))
    expect_identical(modelDef$calcRules[[var]][[3]]$getFullRange()$indexRanges,
                     list(indexRange(3), indexRange(quote(1:3))))

    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[2]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[3]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[3]]$is_type('end'), TRUE)

})

test_that("graph processing for RHS var used twice", {
    code <- quote({
        y ~ dnorm(mu, mu)
        tmp[1:2,1:2] <- theta[1]*pr[1:2,1:2]
        z ~ dmnorm(theta[1:2], tmp[1:2,1:2])
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()
    
    expect_identical(length(unlist(modelDef$rhsOnlyRules)), 4L)

    ## theta split into two rhsOnlyRules
    expect_identical(length(modelDef$rhsOnlyRules[['theta']]), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]), 1L)
})


test_that("graph processing for basic RHS exclusion and LHS fracturing", {
    code <- quote({
        for(i in 1:10)
            y[i] ~ dnorm(mu[i], 1)
        mu[9] ~ dnorm(0,1)
        for(i in 2:3)
            mu[i] ~ dnorm(0,1)
        mu[6:7] <- z[1:2]
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]), 3L)
    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]), 7L)

    var <- 'mu'
    for(i in 1:3) {
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('top'), TRUE)
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('end'), FALSE)
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]][[i]]$sortID, 1)
        expect_identical(modelDef$calcRules[[var]][[i]]$parents, numeric(0))
    }
    expect_identical(modelDef$calcRules[[var]][[1]]$children, "5")
    expect_identical(modelDef$calcRules[[var]][[2]]$children, "8")
    expect_identical(modelDef$calcRules[[var]][[3]]$children, "11")
    
    var <- 'y'
    for(i in c(1,3,5)) {
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('end'), TRUE)
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]][[i]]$sortID, 2)
        expect_identical(modelDef$calcRules[[var]][[i]]$children, numeric(0))
    }
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[3]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[5]]$is_type('top'), TRUE)
    
    expect_identical(modelDef$calcRules[[var]][[1]]$parents, "1")
    expect_identical(modelDef$calcRules[[var]][[3]]$parents, "2")
    expect_identical(modelDef$calcRules[[var]][[5]]$parents, "4")
    
    for(i in c(2,4,6,7)) {
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('top'), TRUE)
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('end'), TRUE)
        expect_identical(modelDef$calcRules[[var]][[i]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]][[i]]$sortID, 2)
        expect_identical(modelDef$calcRules[[var]][[i]]$children, numeric(0))
        expect_identical(modelDef$calcRules[[var]][[i]]$parents, numeric(0))
    }
    
    expect_identical(length(modelDef$rhsOnlyRules), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]), 4L)
    expect_identical(length(modelDef$rhsOnlyRules[['z']]), 1L)
    expect_identical(modelDef$rhsOnlyRules[['mu']][[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1)))
    expect_identical(modelDef$rhsOnlyRules[['mu']][[2]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(4:5)))
    expect_identical(modelDef$rhsOnlyRules[['mu']][[3]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(8)))
    expect_identical(modelDef$rhsOnlyRules[['mu']][[4]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(10)))
    expect_identical(modelDef$rhsOnlyRules[['z']][[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1:2)))

    ## complete overlap
    code <- quote({
        for(i in 1:3) {
            y[i] ~ dnorm(mu[k1[i]], 1)
            mu[k2[i]] ~ dnorm(0, 1)
        }
    })

    modelDef <- modelDefClass$new(code, constants = list(k1 = c(1,2,4), k2 = c(1,2,4)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()
    
    expect_identical(length(modelDef$calcRules), 2L)
    var <- "mu"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)

    ## no overlap
    code <- quote({
        for(i in 1:3) {
            y[i] ~ dnorm(mu[k1[i]], 1)
            mu[k2[i]] ~ dnorm(0, 1)
        }
    })

    modelDef <- modelDefClass$new(code, constants = list(k1 = c(1,2,4), k2 = c(3,5,7)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()
    
    expect_identical(length(modelDef$calcRules), 2L)
    var <- "mu"
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]][[1]]$is_type('top'), TRUE)
    
})

test_that("graph processing error trapping for cycles", {
    code <- quote({
        y ~ dnorm(mu, 1)
        z ~ dnorm(y, 1)
        mu ~ dnorm(z, 1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    expect_error(modelDef$generateGraphInfo(), "Cycle found")

    code <- quote({
        y ~ dnorm(y, 1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    expect_error(modelDef$generateGraphInfo(), "Cycle found")
})

test_that("graph processing for model with overlapping RHS works", {
    code <- quote({
        y[1:5] <- mu[1:5]
        y[7:10] <- mu[2:5]
        for(i in 11:13)
            y[i] <- mu[i-6]
        y[14] <- mu[7]
        y[15:17] <- mu[10:12]
        for(i in 4:5)
            mu[i] ~ dnorm(0, 1)
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]), 3L)
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1:3)))
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(6:7)))
    expect_identical(modelDef$rhsOnlyRules[[var]][[3]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(10:12)))

    ## Multiple exclusion over a single index
    ## .idx2 is replaced in constants when excluding mu[1:4,c(2,5)] with mu[1:4,4:5]
    code <- quote({
        for(i in 1:3) {
            y[1:4, i] <- mu[1:4, k1[i]]
            z[1:4, i] <- mu[1:4, k2[i]]
        }
        mu[1:4, 4:5] <- theta[1:4]
    })
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(1,3,4), k2 = c(1,2,5)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()
    
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['theta']]), 1L)

    var <- 'mu'
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges,
                     list(indexRange(quote(1:4)), indexRange(matrix(c(1,3)))))
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$getFullRange()$indexRanges,
                     list(indexRange(quote(1:4)), indexRange(quote(2:2))))

    code <- quote({
        for(i in 1:3) {
            y[1:4, i] <- mu[1:4, k1[i]]
            z[1:4, i] <- mu[1:4, k2[i]]
        }
        for(i in 1:2)
            mu[1:4, k3[i]] <- theta[1:4]
    })
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(1,3,4), k2 = c(1,2,5), k3 = c(3,5,7)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()
    
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['theta']]), 1L)

    var <- 'mu'
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges,
                     list(indexRange(quote(1:4)), indexRange(matrix(c(1,4)))))
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$getFullRange()$indexRanges,
                     list(indexRange(quote(1:4)), indexRange(quote(2:2))))

    ## Multiple exclusion over same indices, checking multiple use of .idx constants
    code <- quote({
        y <- mu[1, 1]
        z[1:3, 1:2] <- mu[1:3, 1:2]
        w[3, 2:3] <- mu[3, 2:3]
        mu[1:2, 2] ~ dmnorm(mu0[1:2], pr[1:2, 1:2])
        mu[1, 1] ~ dnorm(0, 1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]), 2L)
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(2,3,3,1,1,2), ncol = 2))))
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(3,3), ncol = 2))))

    
    ## Multiple exclusion over disjoint indices, checking multiple use of .idx constants
    code <- quote({
        y[1:2, 1:3, 1:4] <- mu[1:2, 1:3, 1:4]
        for(i in 1:2)
            for(j in 1:2)
                w[i, 1:3, j] <- mu[k1[i], 1:3, k2[j]]
        mu[2, 3, 1:4] <- mu0[2, 3, 1:4]
    })
    
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(2,4), k2 = c(4,6)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]), 2L)

    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(1,2,1,2,1,1,1,2,2,3), ncol = 2)), indexRange(quote(1:4))))
    expect_is(modelDef$rhsOnlyRules[[var]][[1]]$externalRules$indexRules[[1]],
              'indexRuleClass_block')
    expect_is(modelDef$rhsOnlyRules[[var]][[1]]$externalRules$indexRules[[2]],
              'indexRuleClass_arbitrary')
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$index2setID, c(2,2,1))
    
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(4,2,4,4,6,6), ncol = 2)), indexRange(quote(1:3))))
    expect_is(modelDef$rhsOnlyRules[[var]][[2]]$externalRules$indexRules[[1]],
              'indexRuleClass_block')
    expect_is(modelDef$rhsOnlyRules[[var]][[2]]$externalRules$indexRules[[2]],
              'indexRuleClass_arbitrary')
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$index2setID, c(2,1,2))

    ## With an additional exclusion
    code <- quote({
        y[1:2, 1:3, 1:4] <- mu[1:2, 1:3, 1:4]
        for(i in 1:2)
            for(j in 1:2)
                w[i, 1:3, j] <- mu[k1[i], 1:3, k2[j]]
        mu[2, 3, 1:4] <- mu0[2, 3, 1:4]
        mu[4, 3, 6] <- mu0[4, 3, 6]
    })
    
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(2,4), k2 = c(4,6)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]), 2L)

    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(1,2,1,2,1,1,1,2,2,3), ncol = 2)), indexRange(quote(1:4))))
    expect_is(modelDef$rhsOnlyRules[[var]][[1]]$externalRules$indexRules[[1]],
              'indexRuleClass_block')
    expect_is(modelDef$rhsOnlyRules[[var]][[1]]$externalRules$indexRules[[2]],
              'indexRuleClass_arbitrary')
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$index2setID, c(2,2,1))

    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(4,4,4,2,4,2,4,2,1,2,3,1,1,2,2,3,rep(4,3),rep(6,5)), ncol = 3))))
    expect_is(modelDef$rhsOnlyRules[[var]][[2]]$externalRules$indexRules[[1]],
              'indexRuleClass_arbitrary')
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$index2setID, c(1,1,1))

})




test_that("graph processing for multiple RHS only cases", {
    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(mu[i], sigma[i])
        for(i in 2:3)
            mu[i] ~ dnorm(0,1)
        for(i in 5:7)
            sigma[i] ~ dgamma(1,1)
    })

    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()


    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]), 2L)
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1)))
    expect_identical(modelDef$rhsOnlyRules[[var]][[2]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(4:5)))

    var <- 'sigma'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]), 1L)
    expect_identical(modelDef$rhsOnlyRules[[var]][[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1:4)))
})


