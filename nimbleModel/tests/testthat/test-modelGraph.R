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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars
    
    wh <- which(vars == "sigma")
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, "sigma"))
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, 'y'))
                     
    wh <- which(vars == "y")
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, "y"))
    expect_identical(sort(modelDef$calcRules[[wh]]$parents),
                     sort(c(getElement(ids, "sigma"), getElement(ids, "mu"))))
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))

    wh <- which(vars == "mu")
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, 'y'))

    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[[1]]$varName, "mu0")
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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    var <- "sigma"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 3)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, 'y'))

    var <- "mu0"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, "mu"))

    var <- "mu"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, "mu0"))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, "phi"))

    var <- "phi"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 3)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, "y"))

    var <- "y"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 4)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, var))
    expect_identical(sort(modelDef$calcRules[[wh]]$parents),
                     sort(c(getElement(ids, "phi"), getElement(ids, "sigma"))))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, "z"))

    var <- "z"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 5)
    expect_identical(modelDef$calcRules[[wh]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))

    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[[1]]$varName, "mu00")
    
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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars
    
    for(wh in c(which(vars == "sigma"), which(vars == "mu"), which(vars == "z")))
        expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    wh <- which(vars == "y")
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    expect_identical(length(modelDef$calcRules), 6L)

    var <- "z"
    wh <- which(vars == var)
    expect_identical(length(wh), 2L)

    ## BUG z[1] is not latent
    ## z[1] (some hard-coding here)
    expect_identical(modelDef$calcRules[[wh[1]]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh[1]]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh[1]]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh[1]]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh[1]]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh[1]]]$children, "4")

    ## z[2] (some hard-coding here)
    expect_identical(modelDef$calcRules[[wh[2]]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh[2]]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh[2]]]$is_type('latent'), TRUE)
    expect_identical(modelDef$calcRules[[wh[2]]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh[2]]]$parents, "1")
    expect_identical(modelDef$calcRules[[wh[2]]]$children, c("4", "6"))
    
    
    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[[1]]$varName, "z")
    expect_identical(modelDef$rhsOnlyRules[[1]]$getFullRange()$indexRanges[[1]],
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

    expect_identical(length(modelDef$calcRules), 4L)

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    var <- "z"
    wh <- which(vars == var)
    expect_identical(length(wh), 1L)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh]]$parents, "3")
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))

    var <- "y"
    wh <- which(vars == var)
    expect_identical(length(wh), 3L)

    expect_identical(modelDef$calcRules[[wh[1]]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh[1]]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh[1]]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh[1]]]$sortID, 1)
    expect_identical(modelDef$calcRules[[wh[1]]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh[1]]]$children, getElement(ids, 'z'))
    
    expect_identical(modelDef$calcRules[[wh[2]]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh[2]]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh[2]]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh[2]]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh[2]]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh[2]]]$children, numeric(0))

    expect_identical(modelDef$calcRules[[wh[3]]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh[3]]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh[3]]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh[3]]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh[3]]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[wh[3]]]$children, numeric(0))

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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    expect_identical(length(modelDef$calcRules), 4L)

    var <- "theta"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))
    expect_identical(sort(modelDef$calcRules[[wh]]$children),
                     sort(c(getElement(ids, 'w'), getElement(ids, 'y'),  getElement(ids, 'mu'))))

    var <- "mu"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, "theta"))
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, 'y'))
    
    var <- "y"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 3)
    expect_identical(sort(modelDef$calcRules[[wh]]$parents),
                     sort(c(getElement(ids, "theta"), getElement(ids, 'mu'))))

    var <- "w"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 3)
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, "theta"))
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))
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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    var <- "z"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    
    var <- "w"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
 
    var <- "theta"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 3)
 
    var <- "y"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 4)

    var <- "y2"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 4)

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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars
    
    var <- "z"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)

    var <- "w"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)

    var <- "theta"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)

    var <- "y"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 3)

    var <- "y2"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 4)

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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    expect_identical(length(modelDef$calcRules), 3L)

    var <- "y"
    wh <- which(vars == var)
    expect_identical(length(wh), 1L)
    
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))
    expect_identical(modelDef$calcRules[[wh]]$parents, c('1','3'))

    var <- "mu"
    wh <- which(vars == var)
    expect_identical(length(wh), 2L)

    for(i in 1:2) {
        expect_identical(modelDef$calcRules[[wh[i]]]$is_type('top'), TRUE)
        expect_identical(modelDef$calcRules[[wh[i]]]$is_type('end'), FALSE)
        expect_identical(modelDef$calcRules[[wh[i]]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[wh[i]]]$sortID, 1)
        expect_identical(modelDef$calcRules[[wh[i]]]$children, getElement(ids, "y"))
        expect_identical(modelDef$calcRules[[wh[i]]]$parents, numeric(0))
    }

    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        mu[1:4] ~ dmnorm(z[1:4], pr[1:4,1:4])
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "y"
    wh <- which(vars == var)
    expect_identical(length(wh), 1L)
    
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, 'mu'))

    var <- "mu"
    wh <- which(vars == var)
    expect_identical(length(wh), 1L)

    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))
    
    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        z[1:2] <- y[1:2]
    })
    
    modelDef <- modelDefClass$new(code)
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "y"
    wh <- which(vars == var)
    expect_identical(length(wh), 1L)
    
    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 1)
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, 'z'))
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))

    var <- "z"
    wh <- which(vars == var)
    expect_identical(length(wh), 1L)

    expect_identical(modelDef$calcRules[[wh]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('end'), TRUE)
    expect_identical(modelDef$calcRules[[wh]]$is_type('latent'), FALSE)
    expect_identical(modelDef$calcRules[[wh]]$sortID, 2)
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))
    
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
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[wh]]$children, numeric(0))
    
    var <- "y"
    wh <- which(vars == var)
    expect_identical(modelDef$calcRules[[wh]]$children, getElement(ids, "z"))
    expect_identical(modelDef$calcRules[[wh]]$parents, numeric(0))

    expect_identical(length(modelDef$rhsOnlyRules), 3L)
    expect_identical(modelDef$rhsOnlyRules[[3]]$getFullRange()$indexRanges[[1]],
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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    expect_identical(length(modelDef$calcRules), 3L)
    expect_identical(modelDef$calcRules[[2]]$parents, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[[3]]$parents, numeric(0))
    
    expect_identical(length(modelDef$rhsOnlyRules), 2L)
    expect_is(modelDef$rhsOnlyRules[[1]]$externalRules$indexRules[[1]],
              'indexRuleClass_arbitrary')
                     
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
    
    expect_identical(length(modelDef$rhsOnlyRules), 4L)

    ## theta split into two rhsOnlyRules
    vars <- sapply(modelDef$rhsOnlyRules, function(rule) rule$varName)
    expect_identical(sum(vars == "theta"), 2L)
    expect_identical(sum(vars == "mu"), 1L)
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

    vars <- sapply(modelDef$calcRules, function(rule) rule$varName)
    ids <- names(modelDef$calcRules)
    names(ids) <- vars

    expect_identical(length(modelDef$calcRules), 10L)

    var <- "mu"
    wh <- which(vars == var)
    expect_identical(length(wh), 3L)

    for(i in 1:3) {
        expect_identical(modelDef$calcRules[[wh[i]]]$is_type('top'), TRUE)
        expect_identical(modelDef$calcRules[[wh[i]]]$is_type('end'), FALSE)
        expect_identical(modelDef$calcRules[[wh[i]]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[wh[i]]]$sortID, 1)
        expect_identical(modelDef$calcRules[[wh[i]]]$parents, numeric(0))
    }
    expect_identical(modelDef$calcRules[[wh[1]]]$children, "5")
    expect_identical(modelDef$calcRules[[wh[2]]]$children, "8")
    expect_identical(modelDef$calcRules[[wh[3]]]$children, "11")
    
    var <- "y"
    wh <- which(vars == var)
    expect_identical(length(wh), 7L)

    for(i in c(4,6,8)) {
        expect_identical(modelDef$calcRules[[i]]$is_type('end'), TRUE)
        expect_identical(modelDef$calcRules[[i]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[i]]$sortID, 2)
        expect_identical(modelDef$calcRules[[i]]$children, numeric(0))
    }
    expect_identical(modelDef$calcRules[[4]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[6]]$is_type('top'), FALSE)
    expect_identical(modelDef$calcRules[[8]]$is_type('top'), TRUE)
    
    expect_identical(modelDef$calcRules[[4]]$parents, "1")
    expect_identical(modelDef$calcRules[[6]]$parents, "2")
    expect_identical(modelDef$calcRules[[8]]$parents, "4")
    
    for(i in c(5,7,9,10)) {
        expect_identical(modelDef$calcRules[[i]]$is_type('top'), TRUE)
        expect_identical(modelDef$calcRules[[i]]$is_type('end'), TRUE)
        expect_identical(modelDef$calcRules[[i]]$is_type('latent'), FALSE)
        expect_identical(modelDef$calcRules[[i]]$sortID, 2)
        expect_identical(modelDef$calcRules[[i]]$children, numeric(0))
        expect_identical(modelDef$calcRules[[i]]$parents, numeric(0))
    }
    
    expect_identical(length(modelDef$rhsOnlyRules), 5L)
    expect_identical(sapply(modelDef$rhsOnlyRules, function(rule) rule$varName),
                     c(rep('mu', 4), 'z'))
    expect_identical(modelDef$rhsOnlyRules[[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1)))
    expect_identical(modelDef$rhsOnlyRules[[2]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(4:5)))
    expect_identical(modelDef$rhsOnlyRules[[3]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(8)))
    expect_identical(modelDef$rhsOnlyRules[[4]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(10)))
    expect_identical(modelDef$rhsOnlyRules[[5]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1:2)))
    
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

    expect_identical(length(modelDef$rhsOnlyRules), 3L)
    expect_identical(modelDef$rhsOnlyRules[[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1:3)))
    expect_identical(modelDef$rhsOnlyRules[[2]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(6:7)))
    expect_identical(modelDef$rhsOnlyRules[[3]]$getFullRange()$indexRanges[[1]],
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
    
    expect_identical(length(modelDef$rhsOnlyRules), 3L)

    expect_identical(modelDef$rhsOnlyRules[[1]]$getFullRange()$indexRanges,
                     list(indexRange(quote(1:4)), indexRange(matrix(c(1,3)))))
    expect_identical(modelDef$rhsOnlyRules[[2]]$getFullRange()$indexRanges,
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
    
    expect_identical(length(modelDef$rhsOnlyRules), 3L)

    expect_identical(modelDef$rhsOnlyRules[[1]]$getFullRange()$indexRanges,
                     list(indexRange(quote(1:4)), indexRange(matrix(c(1,4)))))
    expect_identical(modelDef$rhsOnlyRules[[2]]$getFullRange()$indexRanges,
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

    expect_identical(length(modelDef$rhsOnlyRules), 4L)
    expect_identical(modelDef$rhsOnlyRules[[1]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(2,3,3,1,1,2), ncol = 2))))
    expect_identical(modelDef$rhsOnlyRules[[2]]$getFullRange()$indexRanges,
                     list(indexRange(matrix(c(3,3), ncol = 2))))

    
    ## Multiple exclusion over disjoint indices, checking multiple use of .idx constants
    code <- quote({
        y[1:2, 1:3, 1:4] <- mu[1:2, 1:3, 1:4]
        for(i in 1:2)
            for(j in 1:2)
                w[i, 1:3, j] <- mu[k1[i], 1:3, k2[j]]
        mu[2, 3, 1:4] <- mu0[2, 3, 1:4]
    })
    ## TODO might need to modify the example to draw out particular issues
    
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(2,3), k2 = c(4,5)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()

    ## intersection of RHS of ((3,4),(2,5),(3,5)),(1:3) with LHS of 2,3,1:4 is messed up - should have no overlap

    ## try to create graphRule for ((3,4),(2,5),(3,5)),(1:3) <- ((3,4),(2,5),(3,5)),(1:3)
    
## In addition: Warning message:
## In indexID_2_rangeID[index2setID != 0] <<- externalRange$indexID_2_rangeID :
##   number of items to replace is not a multiple of replacement length

    ## probably do this instead as (2,3), (4,5) are converted to seq
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(2,4), k2 = c(4,6)))
    modelDef$processModelCode()
    modelDef$processDecls()
    modelDef$generateGraphInfo()
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

    expect_identical(length(modelDef$rhsOnlyRules), 3L)
    expect_identical(sapply(modelDef$rhsOnlyRules, function(rule) rule$varName),
                     c(rep('mu', 2), 'sigma'))
    expect_identical(modelDef$rhsOnlyRules[[1]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1)))
    expect_identical(modelDef$rhsOnlyRules[[2]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(4:5)))
    expect_identical(modelDef$rhsOnlyRules[[3]]$getFullRange()$indexRanges[[1]],
                     indexRange(quote(1:4)))
})


