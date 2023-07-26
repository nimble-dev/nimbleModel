getElements <- function(x, name) {
    result <- x[names(x) == name]
    names(result) <- NULL
    return(result)
}

extractRuleElement <- function(vr, nm) {
    tmp <- sapply(vr$rules, function(rule) rule[[nm]])
    if(is.matrix(tmp))
        tmp <- c(tmp)
    names(tmp) <- NULL
    for(i in seq_along(tmp))
        names(tmp[[i]]) <- NULL
    return(tmp)
}

test_that("graph processing for basic model works", {
    code <- quote({
        y ~ dnorm(mu, sd = sigma)
        mu ~ dnorm(mu0, 1)
        sigma ~ dunif(0,1)
    })

    modelDef <- modelDefClass$new(code)
    
    expect_identical(length(modelDef$downstreamRules), 3L)
    expect_identical(length(modelDef$downstreamRules[['mu0']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['mu']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['sigma']]$rules), 1L)
    expect_identical(modelDef$downstreamRules[['mu0']]$rules[[1]]$toVarName, "mu")
    expect_identical(modelDef$downstreamRules[['mu']]$rules[[1]]$toVarName, "y")
    expect_identical(modelDef$downstreamRules[['sigma']]$rules[[1]]$toVarName, "y")

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))
    
    expect_identical(modelDef$calcRules[['sigma']]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[['sigma']]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[['sigma']]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[['sigma']]$rules[[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[['sigma']]$rules[[1]]$ID, getElement(ids, "sigma"))
    expect_identical(modelDef$calcRules[['sigma']]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[['sigma']]$rules[[1]]$children, getElement(ids, 'y'))
                     
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$ID, getElement(ids, "y"))
    expect_identical(sort(modelDef$calcRules[['y']]$rules[[1]]$parents),
                     sort(c(getElement(ids, "sigma"), getElement(ids, "mu"))))
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$children, numeric(0))

    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$ID, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$children, getElement(ids, 'y'))

    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[['mu0']]$rules[[1]]$varName, "mu0")
})
   
test_that("graph processing for basic model with deterministic nodes works", {
    code <- quote({
        y ~ dnorm(phi, sd = sigma)
        phi <- mu + 1
        mu ~ dnorm(mu0, 1)
        mu0 <- mu00 + 1
        sigma ~ dunif(0,1)
        z <- y + 3
    })
  
    modelDef <- modelDefClass$new(code)
    
    expect_identical(length(modelDef$downstreamRules), 6L)
    
    expect_identical(length(modelDef$downstreamRules[['mu00']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['mu0']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['mu']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['phi']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['sigma']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['y']]$rules), 1L)

    expect_identical(modelDef$downstreamRules[['mu00']]$rules[[1]]$toVarName, "mu0")
    expect_identical(modelDef$downstreamRules[['mu0']]$rules[[1]]$toVarName, "mu")
    expect_identical(modelDef$downstreamRules[['mu']]$rules[[1]]$toVarName, "phi")
    expect_identical(modelDef$downstreamRules[['phi']]$rules[[1]]$toVarName, "y")
    expect_identical(modelDef$downstreamRules[['sigma']]$rules[[1]]$toVarName, "y")
    expect_identical(modelDef$downstreamRules[['y']]$rules[[1]]$toVarName, "z")

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    var <- 'sigma'
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, 'y'))

    var <- 'mu0'
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, "mu"))

    var <- 'mu'
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, getElement(ids, "mu0"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, "phi"))

    var <- "phi"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, getElement(ids, "mu"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, "y"))

    var <- "y"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 4)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$ID, getElement(ids, var))
    expect_identical(sort(modelDef$calcRules[[var]]$rules[[1]]$parents),
                     sort(c(getElement(ids, "phi"), getElement(ids, "sigma"))))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, "z"))

    var <- "z"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 5)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$ID, getElement(ids, var))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, numeric(0))

    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[['mu00']]$rules[[1]]$varName, "mu00")
    
})

test_that("graph processing for model with various parents in a declaration works", {
    code <- quote({
        y ~ dnorm(mu + z, sd = sigma)
        mu ~ dnorm(0, 1)
        sigma ~ dunif(0,1)
        z ~ dnorm(0, 1)
    })

    modelDef <- modelDefClass$new(code)

    expect_identical(length(modelDef$downstreamRules), 4L)
    expect_identical(length(modelDef$downstreamRules[['mu']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['z']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['sigma']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['lifted_mu_plus_z']]$rules), 1L)
    expect_identical(modelDef$downstreamRules[['mu']]$rules[[1]]$toVarName, "lifted_mu_plus_z")
    expect_identical(modelDef$downstreamRules[['z']]$rules[[1]]$toVarName, "lifted_mu_plus_z")
    expect_identical(modelDef$downstreamRules[['sigma']]$rules[[1]]$toVarName, "y")
    expect_identical(modelDef$downstreamRules[['lifted_mu_plus_z']]$rules[[1]]$toVarName, "y")

    for(wh in c('mu','z'))
        expect_identical(modelDef$calcRules[[wh]]$rules[[1]]$sortID, 1)
    for(wh in c('sigma', 'lifted_mu_plus_z'))
        expect_identical(modelDef$calcRules[[wh]]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$sortID, 3)
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
 
    expect_identical(length(modelDef$downstreamRules), 2L)
    
    expect_identical(length(modelDef$downstreamRules[['y']]$rules), 1L)
    expect_identical(length(modelDef$downstreamRules[['z']]$rules), 2L)

    expect_identical(modelDef$downstreamRules[['y']]$rules[[1]]$toVarName, "z")
    expect_identical(modelDef$downstreamRules[['z']]$rules[[1]]$toVarName, "mu")
    expect_identical(modelDef$downstreamRules[['z']]$rules[[2]]$toVarName, "mu")

    expect_identical(length(modelDef$calcRules), 3L)

    var <- "z"

    ## z[1] (some hard-coding here)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, "4")

    ## z[2] (some hard-coding here)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$parents, "1")
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$children, c("4", "6"))
    
    
    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(modelDef$rhsOnlyRules[[var]]$rules[[1]]$varName, "z")
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(3)))

})

test_that("graph processing with split LHS node", {
    code <- quote({
        for(i in 1:10) 
            y[i] ~ dnorm(mu[i], 1)
        for(i in 3:4)
            z[i] ~ dnorm(y[i], 1)      
    })

    modelDef <- modelDefClass$new(code)

    expect_identical(length(modelDef$calcRules), 2L)

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))
    
    var <- "z"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 1L)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, "3")
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, numeric(0))

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 3L)

    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, 'z'))
    
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$children, numeric(0))

    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$children, numeric(0))

})


test_that("graph processing with triangular dependency structure", {
    code <- quote({
        w ~ dnorm(theta, 1)
        mu ~ dnorm(theta, 1)
        y ~ dnorm(theta + mu, 1)
        theta ~ dnorm(0, 1)
    })

    modelDef <- modelDefClass$new(code)

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 5L)

    var <- "theta"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 1)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, numeric(0))
    expect_identical(sort(modelDef$calcRules[[var]]$rules[[1]]$children),
                     sort(c(getElement(ids, 'w'),
                            getElement(ids, 'lifted_theta_plus_mu'),  getElement(ids, 'mu'))))

    var <- "mu"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, getElement(ids, "theta"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, 'lifted_theta_plus_mu'))
    
    var <- "y"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 4)
    expect_identical(sort(modelDef$calcRules[[var]]$rules[[1]]$parents),
                     getElement(ids, 'lifted_theta_plus_mu'))

    var <- "lifted_theta_plus_mu"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)
    expect_identical(sort(modelDef$calcRules[[var]]$rules[[1]]$parents),
                     sort(c(getElement(ids, 'mu'),
                            getElements(ids, 'theta'))))

    var <- "w"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 4)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, getElement(ids, "theta"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, numeric(0))
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

    var <- "z"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 1)
    
    var <- "w"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)
 
    var <- "theta"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)
 
    var <- "y"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 4)

    var <- "y2"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 4)

    code <- quote({
        z <- z0
        w ~ dnorm(0, 1)
        theta ~ dnorm(w, sd = z)
        y <- theta + 3
        y2 <- y + 1
    })

    modelDef <- modelDefClass$new(code)

    var <- "z"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 1)

    var <- "w"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 1)

    var <- "theta"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)

    var <- "y"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)

    var <- "y2"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 4)

})

test_that("graph processing with various types of multivariate nodes", {
    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        mu[1:2] ~ dmnorm(z[1:2], pr[1:2,1:2])
        mu[3] <- 5
    })
    
    modelDef <- modelDefClass$new(code)

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 4L)

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 1L)
    
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, numeric(0))
    expect_identical(sort(modelDef$calcRules[[var]]$rules[[1]]$parents),
                     sort(c(getElements(ids, "mu"),
                            getElements(ids, "lifted_chol_oPpr_oB1to3_comma_1to3_cB_cP"))))

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 2L)

    for(i in 1:2) {
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('top'), TRUE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('end'), FALSE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$sortID, 2)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$children, getElement(ids, "y"))
    }
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$parents,
                     getElements(ids, "lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP"))

    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        mu[1:4] ~ dmnorm(z[1:4], pr[1:4,1:4])
    })
    
    modelDef <- modelDefClass$new(code)

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 4L)

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 1L)
    
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, numeric(0))
    expect_identical(sort(modelDef$calcRules[[var]]$rules[[1]]$parents),
                     sort(c(getElement(ids, 'mu'),
                            getElements(ids, 'lifted_chol_oPpr_oB1to3_comma_1to3_cB_cP'))))

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 1L)

    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents,
                     getElements(ids, 'lifted_chol_oPpr_oB1to4_comma_1to4_cB_cP'))
    
    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], pr[1:3,1:3])
        z[1:2] <- y[1:2]
    })
    
    modelDef <- modelDefClass$new(code)

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 3L)

    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 1L)
    
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 2)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, 'z'))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents,
                     getElement(ids, "lifted_chol_oPpr_oB1to3_comma_1to3_cB_cP"))

    var <- "z"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 1L)

    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$sortID, 3)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, numeric(0))
    
    code <- quote({
        y[1:3] ~ dmnorm(mu[1:3], cholesky = pr[1:3,1:3], prec_param = 1)
        z[1:4] <- y[1:4]
    })
    
    modelDef <- modelDefClass$new(code)

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 2L)

    var <- "z"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, getElement(ids, "y"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, numeric(0))
    
    var <- "y"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, getElement(ids, "z"))
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, numeric(0))

    expect_identical(length(modelDef$rhsOnlyRules), 3L)
    expect_equal(modelDef$rhsOnlyRules[[3]]$rules[[1]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(4)))
})

test_that("graph processing for complicated multiple mv LHS nodes", {
    code <- quote({
        for(i in 1:4)
            y[i, 1:3] ~ dmnorm(mu[1:3, i], pr[1:3,1:3])
        mu[1:2, 1:2] ~ dwish(pr[1:2,1:2], 5)
    })
    
    modelDef <- modelDefClass$new(code)

    tmp <- unlist(sapply(modelDef$calcRules, function(rule) rule$rules))
    ids <- sapply(tmp, function(x) x$ID)
    names(ids) <- sub("\\..*", "", names(ids))

    expect_identical(length(modelDef$calcRules), 4L)
    expect_identical(length(modelDef$calcRules[['y']]$rules), 2L)
    expect_identical(length(modelDef$calcRules[['mu']]$rules), 1L)
    
    expect_identical(sort(modelDef$calcRules[['y']]$rules[[1]]$parents),
                     sort(c(getElement(ids, "mu"),
                            getElement(ids, "lifted_chol_oPpr_oB1to3_comma_1to3_cB_cP"))))
    expect_identical(modelDef$calcRules[['y']]$rules[[2]]$parents,
                     getElement(ids, "lifted_chol_oPpr_oB1to3_comma_1to3_cB_cP"))
    
    expect_identical(length(modelDef$rhsOnlyRules), 2L)
    expect_is(modelDef$rhsOnlyRules[['mu']]$rules[[1]]$externalRule$indexRules[[1]],
              'indexRuleArbitraryClass')

    
    code <- quote({
        for(i in 1:3)
            mu[i, 1:3] ~ dmnorm(mu0[1:3, i], cholesky = pr[1:3,1:3], prec_param = 1)
        mu0[1, 1:2] ~ dmnorm(zero[1:2], cholesky = pr[1:2, 1:2], prec_param = 1)
        y ~ dnorm(mu[1,1], 1)
    })
    
    modelDef <- modelDefClass$new(code)

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 3L)

    expect_equal(modelDef$calcRules[[var]]$rules[[1]]$fullRange$indexRanges,
                     list(newIndexRange(1), newIndexRange(quote(1:3))))
    expect_equal(modelDef$calcRules[[var]]$rules[[2]]$fullRange$indexRanges,
                     list(newIndexRange(2), newIndexRange(quote(1:3))))
    expect_equal(modelDef$calcRules[[var]]$rules[[3]]$fullRange$indexRanges,
                     list(newIndexRange(3), newIndexRange(quote(1:3))))

    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('latent'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$isOfType('end'), TRUE)

})

test_that("graph processing for RHS var used twice", {
    code <- quote({
        y ~ dnorm(mu, mu)
        tmp[1:2,1:2] <- theta[1]*pr[1:2,1:2]
        z ~ dmnorm(theta[1:2], tmp[1:2,1:2])
    })

    modelDef <- modelDefClass$new(code)
    
    expect_identical(length(modelDef$rhsOnlyRules), 3L)

    ## theta split into two rhsOnlyRules
    expect_identical(length(modelDef$rhsOnlyRules[['theta']]$rules), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]$rules), 1L)
    expect_identical(length(modelDef$rhsOnlyRules[['pr']]$rules), 1L)
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
    
    expect_identical(length(modelDef$calcRules), 2L)

    var <- "mu"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 3L)
    var <- "y"
    expect_identical(length(modelDef$calcRules[[var]]$rules), 7L)

    var <- 'mu'
    for(i in 1:3) {
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('top'), TRUE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('end'), FALSE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$sortID, 1)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$parents, numeric(0))
    }
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$children, "5")
    expect_identical(modelDef$calcRules[[var]]$rules[[2]]$children, "8")
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$children, "11")
    
    var <- 'y'
    for(i in c(1,3,5)) {
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('end'), TRUE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$sortID, 2)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$children, numeric(0))
    }
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$isOfType('top'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[5]]$isOfType('top'), TRUE)
    
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$parents, "2")
    expect_identical(modelDef$calcRules[[var]]$rules[[3]]$parents, "3")
    expect_identical(modelDef$calcRules[[var]]$rules[[5]]$parents, "4")
    
    for(i in c(2,4,6,7)) {
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('top'), TRUE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('end'), TRUE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$isOfType('latent'), FALSE)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$sortID, 2)
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$children, numeric(0))
        expect_identical(modelDef$calcRules[[var]]$rules[[i]]$parents, numeric(0))
    }
    
    expect_identical(length(modelDef$rhsOnlyRules), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]$rules), 4L)
    expect_identical(length(modelDef$rhsOnlyRules[['z']]$rules), 1L)
    expect_equal(modelDef$rhsOnlyRules[['mu']]$rules[[1]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(1)))
    expect_equal(modelDef$rhsOnlyRules[['mu']]$rules[[2]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(4:5)))
    expect_equal(modelDef$rhsOnlyRules[['mu']]$rules[[3]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(8)))
    expect_equal(modelDef$rhsOnlyRules[['mu']]$rules[[4]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(10)))
    expect_equal(modelDef$rhsOnlyRules[['z']]$rules[[1]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(1:2)))

    ## complete overlap
    code <- quote({
        for(i in 1:3) {
            y[i] ~ dnorm(mu[k1[i]], 1)
            mu[k2[i]] ~ dnorm(0, 1)
        }
    })

    modelDef <- modelDefClass$new(code, constants = list(k1 = c(1,2,4), k2 = c(1,2,4)))
    
    expect_identical(length(modelDef$calcRules), 2L)
    var <- "mu"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)

    ## no overlap
    code <- quote({
        for(i in 1:3) {
            y[i] ~ dnorm(mu[k1[i]], 1)
            mu[k2[i]] ~ dnorm(0, 1)
        }
    })

    modelDef <- modelDefClass$new(code, constants = list(k1 = c(1,2,4), k2 = c(3,5,7)))
    
    expect_identical(length(modelDef$calcRules), 2L)
    var <- "mu"
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[[var]]$rules[[1]]$isOfType('top'), TRUE)
    
})

test_that("graph processing error trapping for cycles", {
    code <- quote({
        y ~ dnorm(mu, 1)
        z ~ dnorm(y, 1)
        mu ~ dnorm(z, 1)
    })

    expect_message(expect_error(modelDef <- modelDefClass$new(code), "cycle found"),
                  "Detected state-space")

    code <- quote({
        y ~ dnorm(y, 1)
    })

    expect_message(expect_error(modelDef <- modelDefClass$new(code), "cycle found"),
                  "Detected state-space")
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
    
    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]$rules), 3L)
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(1:3)))
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[2]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(6:7)))
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[3]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(10:12)))

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
    
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]$rules), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['theta']]$rules), 1L)

    var <- 'mu'
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges,
                     list(newIndexRange(quote(1:4)), newIndexRange(matrix(c(1,3)))))
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[2]]$fullRange$indexRanges,
                     list(newIndexRange(quote(1:4)), newIndexRange(quote(2))))

    code <- quote({
        for(i in 1:3) {
            y[1:4, i] <- mu[1:4, k1[i]]
            z[1:4, i] <- mu[1:4, k2[i]]
        }
        for(i in 1:2)
            mu[1:4, k3[i]] <- theta[1:4]
    })
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(1,3,4), k2 = c(1,2,5), k3 = c(3,5,7)))
    
    expect_identical(length(modelDef$rhsOnlyRules[['mu']]$rules), 2L)
    expect_identical(length(modelDef$rhsOnlyRules[['theta']]$rules), 1L)

    var <- 'mu'
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges,
                     list(newIndexRange(quote(1:4)), newIndexRange(matrix(c(1,4)))))
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[2]]$fullRange$indexRanges,
                     list(newIndexRange(quote(1:4)), newIndexRange(quote(2))))

    ## Multiple exclusion over same indices, checking multiple use of .idx constants
    code <- quote({
        y <- mu[1, 1]
        z[1:3, 1:2] <- mu[1:3, 1:2]
        w[3, 2:3] <- mu[3, 2:3]
        mu[1:2, 2] ~ dmnorm(mu0[1:2], cholesky = pr[1:2, 1:2], prec_param = 1)
        mu[1, 1] ~ dnorm(0, 1)
    })

    modelDef <- modelDefClass$new(code)

    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]$rules), 2L)
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges,
                     list(newIndexRange(matrix(c(2,3,3,1,1,2), ncol = 2))))
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[2]]$fullRange$indexRanges,
                     list(newIndexRange(matrix(c(3,3), ncol = 2))))

    
    ## Multiple exclusion over disjoint indices, checking multiple use of .idx constants
    code <- quote({
        y[1:2, 1:3, 1:4] <- mu[1:2, 1:3, 1:4]
        for(i in 1:2)
            for(j in 1:2)
                w[i, 1:3, j] <- mu[k1[i], 1:3, k2[j]]
        mu[2, 3, 1:4] <- mu0[2, 3, 1:4]
    })
    
    modelDef <- modelDefClass$new(code, constants = list(k1 = c(2,4), k2 = c(4,6)))

    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]$rules), 2L)

    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges,
                     list(newIndexRange(matrix(c(1,2,1,2,1,1,1,2,2,3), ncol = 2)), newIndexRange(quote(1:4))))
    expect_is(modelDef$rhsOnlyRules[[var]]$rules[[1]]$externalRule$indexRules[[1]],
              'indexRuleBlockClass')
    expect_is(modelDef$rhsOnlyRules[[var]]$rules[[1]]$externalRule$indexRules[[2]],
              'indexRuleArbitraryClass')
    expect_identical(modelDef$rhsOnlyRules[[var]]$rules[[1]]$indexSlotToSet, c(2,2,1))
    
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[2]]$fullRange$indexRanges,
                     list(newIndexRange(matrix(c(4,2,4,4,6,6), ncol = 2)), newIndexRange(quote(1:3))))
    expect_is(modelDef$rhsOnlyRules[[var]]$rules[[2]]$externalRule$indexRules[[1]],
              'indexRuleBlockClass')
    expect_is(modelDef$rhsOnlyRules[[var]]$rules[[2]]$externalRule$indexRules[[2]],
              'indexRuleArbitraryClass')
    expect_identical(modelDef$rhsOnlyRules[[var]]$rules[[2]]$indexSlotToSet, c(2,1,2))

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

    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]$rules), 2L)

    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges,
                     list(newIndexRange(matrix(c(1,2,1,2,1,1,1,2,2,3), ncol = 2)), newIndexRange(quote(1:4))))
    expect_is(modelDef$rhsOnlyRules[[var]]$rules[[1]]$externalRule$indexRules[[1]],
              'indexRuleBlockClass')
    expect_is(modelDef$rhsOnlyRules[[var]]$rules[[1]]$externalRule$indexRules[[2]],
              'indexRuleArbitraryClass')
    expect_identical(modelDef$rhsOnlyRules[[var]]$rules[[1]]$indexSlotToSet, c(2,2,1))

    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[2]]$fullRange$indexRanges,
                     list(newIndexRange(matrix(c(4,4,4,2,4,2,4,2,1,2,3,1,1,2,2,3,rep(4,3),rep(6,5)), ncol = 3))))
    expect_is(modelDef$rhsOnlyRules[[var]]$rules[[2]]$externalRule$indexRules[[1]],
              'indexRuleArbitraryClass')
    expect_identical(modelDef$rhsOnlyRules[[var]]$rules[[2]]$indexSlotToSet, c(1,1,1))

})

test_that("graph processing for multiple RHS only cases", {
    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(mu[i], sd = sigma[i])
        for(i in 2:3)
            mu[i] ~ dnorm(0,1)
        for(i in 5:7)
            sigma[i] ~ dgamma(1,1)
    })

    modelDef <- modelDefClass$new(code)
    
    var <- 'mu'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]$rules), 2L)
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(1)))
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[2]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(4:5)))

    var <- 'sigma'
    expect_identical(length(modelDef$rhsOnlyRules[[var]]$rules), 1L)
    expect_equal(modelDef$rhsOnlyRules[[var]]$rules[[1]]$fullRange$indexRanges[[1]],
                     newIndexRange(quote(1:4)))
})

test_that("graph processing for block models with fixed indexing", {
    code <- quote({
        for(i in 1:10) {
            y[i] ~ dnorm(mu[k[i]], 1)
            mu[i] ~ dnorm(mu0, 1)
        }})
    
    k <- c(1,3,2,1,1,2,2,3,3,1)

    modelDef <- modelDefClass$new(code, constants = list(k=k))
    
    expect_identical(length(modelDef$calcRules), 2L)
    expect_identical(length(modelDef$calcRules[['y']]$rules), 1L)
    expect_identical(length(modelDef$calcRules[['mu']]$rules), 2L)

    expect_equal(modelDef$calcRules[['y']]$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(1:10))), varName = 'y'))
    expect_equal(modelDef$calcRules[['mu']]$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'mu'))
    expect_equal(modelDef$calcRules[['mu']]$rules[[2]]$fullRange,
                     varRangeClass$new(list(newIndexRange(quote(4:10))), varName = 'mu'))

    y_ids <- sapply(modelDef$calcRules[['y']]$rules, function(rule) rule$ID)
    names(y_ids) <- NULL
    mu_ids <- sapply(modelDef$calcRules[['mu']]$rules, function(rule) rule$ID)
    names(mu_ids) <- NULL
    
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$children, numeric(0))
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$parents, mu_ids[1])
    
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$children, y_ids[1])
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[['mu']]$rules[[2]]$children, numeric(0))
    expect_identical(modelDef$calcRules[['mu']]$rules[[2]]$parents, numeric(0))

    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$isOfType('top'), FALSE)
   
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$isOfType('end'), FALSE)
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$isOfType('top'), TRUE)
    expect_identical(modelDef$calcRules[['mu']]$rules[[2]]$isOfType('end'), TRUE)
    expect_identical(modelDef$calcRules[['mu']]$rules[[2]]$isOfType('top'), TRUE)
})

test_that("graph processing for dynamic indexing", {
    code <- quote({
        for(i in 1:10) {
            y[i] ~ dnorm(mu[k[i]], 1)
            mu[i] ~ dnorm(mu0, 1)
            k[i] ~ dcat(p[1:10])
        }})
    
    k=c(1:3,1:3,1:3,2)
    
    modelDef <- modelDefClass$new(code, inits = list(k=k))

    expect_identical(length(modelDef$calcRules), 3L)
    expect_identical(length(modelDef$calcRules[['y']]$rules), 1L)
    expect_identical(length(modelDef$calcRules[['mu']]$rules), 1L)
    expect_identical(length(modelDef$calcRules[['k']]$rules), 1L)
    
    y_id <- sapply(modelDef$calcRules[['y']]$rules, function(rule) rule$ID)
    names(y_id) <- NULL
    mu_id <- sapply(modelDef$calcRules[['mu']]$rules, function(rule) rule$ID)
    names(mu_id) <- NULL
    k_id <- sapply(modelDef$calcRules[['k']]$rules, function(rule) rule$ID)
    names(k_id) <- NULL
    
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$children, numeric(0))
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$parents, c(mu_id, k_id))
    
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$children, y_id)
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$parents, numeric(0))
    expect_identical(modelDef$calcRules[['k']]$rules[[1]]$children, y_id)
    expect_identical(modelDef$calcRules[['k']]$rules[[1]]$parents, numeric(0))

    expect_true(is(modelDef$downstreamRules$mu$rules[[1]]$indexRules[[1]], 'indexRuleAllClass'))
    expect_true(is(modelDef$downstreamRules$mu$rules[[1]]$indexConstraints[[1]],
                   'indexConstraintSequenceClass'))
    expect_identical(modelDef$downstreamRules$mu$rules[[1]]$indexConstraints[[1]]$end,
                     as.numeric(.Machine$integer.max))

    expect_true(modelDef$varInfo$mu$anyDynamicallyIndexed)
    expect_false(modelDef$varInfo$y$anyDynamicallyIndexed)
    expect_false(modelDef$varInfo$mu0$anyDynamicallyIndexed)
    expect_false(modelDef$varInfo$k$anyDynamicallyIndexed)
    expect_false(modelDef$varInfo$p$anyDynamicallyIndexed)
    
})


test_that("warning of non-constant indices without priors", {
    code <- quote({
        for(i in 1:10) {
            y[i] ~ dnorm(mu[k[i]], 1)
            mu[i]~dnorm(mu0, 1)
        }})
    
    k=c(1:3,1:3,1:3,2)
    
    expect_message(modelDef <- modelDefClass$new(code, inits = list(k=k)),
                   "Detected use of non-constant indices")

    code = quote({
        for(i in 1:3) {
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        }
        for(i in 1:20)
            mu[i] ~ dnorm(0,1)
    })
    
    expect_error(modelDef <- modelDefClass$new(code,
                                                 constants = list(block=sample(1:20,3))),
                   "dynamic indexing of constants")

    code = quote({
        for(i in 1:3) {
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        }
        for(i in 1:20)
            mu[i] ~ dnorm(0,1)
    })
    
    expect_message(modelDef <- modelDefClass$new(code,
                                                 constants = list(k = 1:3)),
                   "Detected use of non-constant indices")
})

test_that("nested indexing", {

    ## Both internal and external indices constants.
    code <- quote({
        for(i in 1:3) 
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        for(i in 1:10) 
            mu[i] ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(code, constants = list(k = c(2, 4, 5), block = c(3, 5, 7, 6, 6)))
    result <- getDependencies(modelDef, 'mu[6]', self = FALSE)
    expect_equal(result[[1]], varRangeClass$new(list(newIndexRange(quote(2:3))), varName = 'y',
                                                fromStochRule = TRUE))

    ## Internal index constant, external index dynamic.
    code <- quote({
        for(i in 1:3) 
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        for(i in 1:10) 
            mu[i] ~ dnorm(0, 1)
        for(i in 1:5)
            block[i] ~ dcat(p[1:10])
    })
    modelDef <- modelDefClass$new(code, constants = list(k = c(2, 4, 5)))
    result <- getDependencies(modelDef, 'mu[6]', self = FALSE)
    expect_equal(result[[1]], varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y',
                                                fromStochRule = TRUE))

    ## External index constant, internal index dynamic.
    code <- quote({
        for(i in 1:3) 
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        for(i in 1:10) 
            mu[i] ~ dnorm(0, 1)
        for(i in 1:3)
            k[i] ~ dcat(p[1:5])
    })
    expect_error(modelDef <- modelDefClass$new(code, constants = list(block = c(3, 5, 7, 6, 6))),
                 "dynamic indexing of constants is not allowed")

    ##  Both internal and external indices dynamic.

    code <- quote({
        for(i in 1:3) 
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        for(i in 1:10) 
            mu[i] ~ dnorm(0, 1)
        for(i in 1:3)
            k[i] ~ dcat(p[1:5])
        for(i in 1:5)
            block[i] ~ dcat(p[1:10])
    })
    modelDef <- modelDefClass$new(code)

    expect_identical(length(modelDef$declInfo[[1]]$dynamicIndexInfo), 2L)
    expect_false(modelDef$varInfo$y$anyDynamicallyIndexed)
    expect_false(modelDef$varInfo$k$anyDynamicallyIndexed)
    expect_true(modelDef$varInfo$mu$anyDynamicallyIndexed)
    expect_true(modelDef$varInfo$block$anyDynamicallyIndexed)
    
    result <- getDependencies(modelDef, 'mu[6]', self = FALSE)
    expect_equal(result[[1]], varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y',
                                                fromStochRule = TRUE))

    code <- quote({
        for(i in 1:3) 
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        for(i in 1:10) 
            mu[i] ~ dnorm(0, 1)
        for(i in 1:3)
            k[i] ~ dcat(p[1:5])
        for(i in 1:5)
            block[i] ~ dcat(p[1:10])
    })
    modelDef <- modelDefClass$new(code, data = list(block = c(3, 5, 6, 5, 2)))

    ## If `block` is not constant, we still have full set of dependencies set up.
    result <- getDependencies(modelDef, 'mu[9]', self = FALSE)
    expect_equal(result[[1]], varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y',
                                                fromStochRule = TRUE))

    code <- quote({
        for(i in 1:3) 
            y[i] ~ dnorm(mu[block[k[i]]], 1)
        for(i in 1:10) 
            mu[i] ~ dnorm(0, 1)
    })
    expect_message(modelDef <- modelDefClass$new(code), "Detected use of non-constant")
    result <- getDependencies(modelDef, 'mu[6]', self = FALSE)
    expect_equal(result[[1]], varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y',
                                                fromStochRule = TRUE))
                         
})


## Testing of model graph interface functions: getDependencies, getParents, getNodes

test_that("basic check of graph interface", {
    code <- quote({
        y ~ dnorm(theta, 1)
        theta <- mu0 + 2
        mu0 ~ dnorm(0, sd = sigma)        
    })
    modelDef <- modelDefClass$new(code)
    
    result <- getNodes(modelDef)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','theta','mu0'))
    result <- getNodes(modelDef, topOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu0'))
    result <- getNodes(modelDef, endOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y'))
    result <- getNodes(modelDef, latentOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta'))
    
    result <- getNodes(modelDef, stochOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','mu0'))
    result <- getNodes(modelDef, determOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta'))

    result <- getNodes(modelDef, includeRHSonly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','theta','mu0','sigma'))

    result <- getNodes(modelDef, 'theta')
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta'))
    result <- getNodes(modelDef, 'theta', includeRHSonly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta'))
    result <- getNodes(modelDef, c('theta','y'))
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta','y'))

    result <- getDependencies(modelDef, 'mu0')
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu0','theta','y'))
    result <- getDependencies(modelDef, 'mu0', self = FALSE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta','y'))
    
    result <- getDependencies(modelDef, 'mu0', immediateOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu0','theta'))

    result <- getDependencies(modelDef, c('mu0','theta'), self = FALSE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y'))

    result <- getParents(modelDef, 'y')
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta','mu0'))
    result <- getParents(modelDef, 'y', self = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','theta','mu0'))
    result <- getParents(modelDef, 'y', immediateOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta'))

    expect_true(modelDef$declRules[['y']]$rules[[1]]$stoch)
    expect_false(modelDef$declRules[['theta']]$rules[[1]]$stoch)
    expect_true(modelDef$declRules[['mu0']]$rules[[1]]$stoch)

    ## checking downstream/upstream with latent stoch node 
    code <- quote({
        y ~ dnorm(theta, 1)
        theta ~ dnorm(mu0, 1)
        mu0 ~ dnorm(0, sd = sigma)        
    })
    modelDef <- modelDefClass$new(code)
        
    result <- getDependencies(modelDef, 'mu0')
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu0','theta'))

    result <- getDependencies(modelDef, 'mu0', downstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu0','theta','y'))

    result <- getParents(modelDef, 'y')
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta'))

    result <- getParents(modelDef, 'y', upstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta','mu0','sigma'))

})

test_that("getDependencies deals with repeated children", {
    code <- quote({
        y ~ dnorm(mu, sd = tau)
    })
    modelDef <- modelDefClass$new(code)
    
    result <- getDependencies(modelDef, c('tau','mu'))
    expect_identical(sapply(result, function(node) node$varName),
                     c('y'))

    code <- quote({
        for(i in 1:5) {
            y[i] ~ dnorm(mu[i], sd = sigma)
            mu[i] ~ dnorm(mu0, sd = tau)
        }
        mu0 ~ dnorm(0, 1)
        tau ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
    })
    modelDef <- modelDefClass$new(code)
    
    getDependencies(modelDef, c('mu0','sigma','tau'), self = FALSE)
})

test_that("getDependencies deals with repeated parents", {
    code <- quote({
        y ~ dnorm(mu, sd = tau)
        z ~ dnorm(mu, 1)
        w <- z + 3
    })
    modelDef <- modelDefClass$new(code)
    
    result <- getParents(modelDef, c('y','z'))
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu','tau'))

})

test_that("getDependencies with multiple children", {

    code <- quote({
        y ~ dnorm(mu, sd = tau)
        z ~ dnorm(mu, 1)
        w <- z + 3
    })
    modelDef <- modelDefClass$new(code)

    result <- getDependencies(modelDef, 'mu')
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','z'))
    
    result <- getDependencies(modelDef, 'mu', downstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','z','w'))

})

test_that("getParents traversal with mix of stoch/determ edges", {
    code <- quote({
        y ~ dnorm(theta, sd = z)
        theta ~ dnorm(beta, 1)
        z <- w + x
        w ~ dunif(sigma, 2)
        x <- phi + 2
        phi ~ dunif(0,1)
    })
    modelDef <- modelDefClass$new(code)
    
    result <- getParents(modelDef, 'y')
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta','z','w','x','phi'))

})

test_that("same dependent on RHS", {
    code <- quote({
        y ~ dnorm(mu, sd = mu)
    })
    modelDef <- modelDefClass$new(code)

    result <- getParents(modelDef, 'y')
    expect_identical(sapply(result, function(node) node$varName),
                    'mu')
})


test_that("basic hierarchical models", {
    code <- quote({
        for(i in 1:10) {
            y[i] ~ dnorm(mu[i], sd = tau)
            mu[i] ~ dnorm(mu0, sd = sigma)
        }
        tau ~ dunif(0, bnd) 
        sigma ~ dunif(0, 1)
        for(i in 1:3)
            z[k[i]] ~ dnorm(y[k[i]], 1)
        
    })
    k <- c(2,4,7)
    modelDef <- modelDefClass$new(code, constants = list(k = k))

    result <- getNodes(modelDef)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','mu','tau','sigma','z'))

    result <- getNodes(modelDef, c('z[1:5]', 'mu0'))
    expect_identical(sapply(result, function(node) node$varName),
                     c('z'))

    result <- getNodes(modelDef, c('z[1:5]', 'mu'))
    expect_identical(sapply(result, function(node) node$varName),
                     c('z','mu'))

    result <- getNodes(modelDef, topOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('tau','sigma'))
    result <- getNodes(modelDef, endOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','z'))
    expect_equal(result[[1]]$indexRanges,
                     list(newIndexRange(matrix(c(1,3,5,6,8,9,10)))))
    
    result <- getNodes(modelDef, latentOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','mu'))
    expect_equal(result[[1]]$indexRanges,
                     list(newIndexRange(matrix(c(2,4,7)))))
    expect_equal(result[[2]]$indexRanges,
                     list(newIndexRange(quote(1:10))))

    result <- getNodes(modelDef, stochOnly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','mu','tau','sigma','z'))
    result <- getNodes(modelDef, determOnly = TRUE)
    expect_identical(result, NULL)

    result <- getNodes(modelDef, includeRHSonly = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','mu','tau','sigma','z','mu0','bnd'))

    result <- getDependencies(modelDef, 'sigma')
    expect_identical(sapply(result, function(node) node$varName),
                     c('sigma','mu'))
    expect_equal(result[[2]]$indexRanges, 
               list(newIndexRange(quote(1:10))))

    result <- getDependencies(modelDef, 'mu', self = FALSE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y'))
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:10))))
    
    result <- getDependencies(modelDef, 'y', self = FALSE)
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(matrix(k))))
    
    result <- getDependencies(modelDef, 'y[1:3]', self = FALSE)
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(2)))

    result <- getDependencies(modelDef, 'sigma', downstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('sigma','mu','y','z'))

    result <- getParents(modelDef, 'y')
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu','tau'))

    result <- getParents(modelDef, 'y', upstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu','tau','mu0','sigma','bnd'))

    result <- getParents(modelDef, 'y[1:5]', upstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu','tau','mu0','sigma','bnd'))
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:5))))

    result <- getParents(modelDef, varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y'),
                         upstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu','tau','mu0','sigma','bnd'))
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:5))))

    ## testing queries involving parts of nodes

    code <- quote({
        for(i in 1:10) {
            y[i] ~ dnorm(mu[i], sd = tau)
            w[i] <- pr[i, 2]
        }
        mu[1:10] ~ dmnorm(mu0[1:10], cholesky = pr[1:10,1:10], prec_param = 1)
        mu0[1:10] <- mu00*z[1:10]
        mu00 ~ dnorm(0, 1)
        pr[1:10, 1:10] ~ dwish(cholesky = S[1:10, 1:10], df = 5, scale_param = 1)
        tau ~ dunif(0, bnd)
        sigma ~ dunif(0, 1)
    })
    modelDef <- modelDefClass$new(code)
    
    result <- getNodes(modelDef, 'pr[2,2]')
    expect_identical(sapply(result, function(node) node$varName),
                     'pr')
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:10)), newIndexRange(quote(1:10))))
    
    result <- getNodes(modelDef, varRangeClass$new(list(newIndexRange(2),newIndexRange(2)), varName = 'pr'))
    expect_identical(sapply(result, function(node) node$varName),
                     'pr')
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:10)), newIndexRange(quote(1:10))))

    result <- getDependencies(modelDef, 'pr[2,2]', self = FALSE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('w','mu'))
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(2)))
    expect_equal(result[[2]]$indexRanges, 
               list(newIndexRange(quote(1:10))))

    result <- getDependencies(modelDef, 'pr[2,2]', self = FALSE, downstream = TRUE)
    expect_identical(sapply(result, function(node) node$varName),
                     c('w','mu','y'))

    result <- getParents(modelDef, 'pr[2,2]')
    expect_identical(sapply(result, function(node) node$varName),
                     'S')
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:10)), newIndexRange(quote(1:10))))

    result <- getParents(modelDef, 'mu[3]')
    expect_identical(sapply(result, function(node) node$varName),
                     c('mu0','pr','mu00','z'))
    expect_equal(result[[2]]$indexRanges, 
               list(newIndexRange(quote(1:10)), newIndexRange(quote(1:10))))
    
    code <- quote({
        mn[1:3] <- X[1:3, 1:5] %*% beta[1:5]
        for(i in 1:5) {
            y[i, 1:3] ~ dmnorm(mn[1:3], cholesky = pr[1:3,1:3], prec_param = 1)
        }
        pr[1:3, 1:3] ~ dwish(cholesky = S[1:3, 1:3], df = 5, scale_param = 1)
        for(i in 1:5)
            beta[i] ~ dnorm(beta0, sd = tau)
        beta0 ~ dunif(0,1)        
    })
    modelDef <- modelDefClass$new(code)
    
    result <- getNodes(modelDef, 'y')
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:5)), newIndexRange(quote(1:3))))
    expect_identical(result[[1]]$boolExternalIndexRanges, c(TRUE,FALSE))

    result <- getNodes(modelDef, 'beta')
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(1:5))))

    result <- getNodes(modelDef, 'beta[2]')
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(2))))

    result <- getDependencies(modelDef, 'beta[2]')
    expect_identical(sapply(result, function(node) node$varName),
                     c('beta','mn','y'))
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(2)))
    expect_equal(result[[2]]$indexRanges, 
               list(newIndexRange(quote(1:3))))
    expect_equal(result[[3]]$indexRanges, 
               list(newIndexRange(quote(1:5)), newIndexRange(quote(1:3))))

    result <- getParents(modelDef, 'y[1,1:3]')
    expect_identical(sapply(result, function(node) node$varName),
                     c('mn','pr','X','beta'))

    result <- getParents(modelDef, 'y[1,2]')
    expect_identical(sapply(result, function(node) node$varName),
                     c('mn','pr','X','beta'))
    
    result <- getParents(modelDef, c('y[1,2]','y[2,3]'))
    expect_identical(sapply(result, function(node) node$varName),
                     c('mn','pr','X','beta'))
    
})

test_that("mixed-length block dependences", {
    code <- quote({
        for(i in 1:3)
            y[i, n1[i]:n2[i]] ~ dmulti(p[n1[i]:n2[i]], 10)
    })
    modelDef <- modelDefClass$new(code, constants = list(n1 = c(3,1,2), n2 = c(6,3,2)))
    result <- getDependencies(modelDef, 'p[2]')
    expect_equal(result[[1]]$indexRanges, list(newIndexRange(matrix(c(2,2,2,3,1,2,3,2), ncol = 2))))
})


test_that("state-space model", {

    ## basic dependence within a variable
    code <- quote({
        for(i in 2:4)
        y[i]~dnorm(y[i-1],1)
    })
    modelDef <- modelDefClass$new(code)

    expect_identical(length(modelDef$rhsOnlyRules), 1L)
    expect_identical(length(modelDef$rhsOnlyRules[['y']]$rules), 1L)
    expect_equal(modelDef$rhsOnlyRules[['y']]$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(1)), varName = 'y'))

    expect_identical(length(modelDef$calcRules), 1L)
    expect_identical(length(modelDef$calcRules[['y']]$rules), 3L)

    expect_equal(modelDef$calcRules[['y']]$rules[[1]]$fullRange,
                     varRangeClass$new(list(newIndexRange(4)), varName = 'y'))
    expect_equal(modelDef$calcRules[['y']]$rules[[2]]$fullRange,
                     varRangeClass$new(list(newIndexRange(3)), varName = 'y'))
    expect_equal(modelDef$calcRules[['y']]$rules[[3]]$fullRange,
                     varRangeClass$new(list(newIndexRange(2)), varName = 'y'))

    ids <- sapply(modelDef$calcRules[['y']]$rules, function(rule) rule$ID)
    names(ids) <- NULL
    
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$children, numeric(0))
    expect_identical(modelDef$calcRules[['y']]$rules[[1]]$parents, ids[2])
    expect_identical(modelDef$calcRules[['y']]$rules[[2]]$children, ids[1])
    expect_identical(modelDef$calcRules[['y']]$rules[[2]]$parents, ids[3])
    expect_identical(modelDef$calcRules[['y']]$rules[[3]]$children, ids[2])
    expect_identical(modelDef$calcRules[['y']]$rules[[3]]$parents, numeric(0))
    
    sortID <- sapply(modelDef$calcRules[['y']]$rules, function(rule) rule$sortID)
    names(sortID) <- NULL
    expect_identical(sortID, as.numeric(3:1))
    
    result <- getNodes(modelDef, 'y')
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(quote(2:4))))

    result <- getNodes(modelDef, 'y', topOnly = TRUE)
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(2)))
    result <- getNodes(modelDef, 'y', endOnly = TRUE)
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(4)))
    result <- getNodes(modelDef, 'y', latentOnly = TRUE)
    expect_equal(result[[1]]$indexRanges, 
               list(newIndexRange(3)))

    result <- getDependencies(modelDef, 'y[1]')
    expect_length(result, 1)
    expect_equal(result[[1]],
                 varRangeClass$new(list(newIndexRange(2)), varName = 'y', fromStochRule = TRUE))
    
    result <- getDependencies(modelDef, 'y[2]')
    expect_length(result, 2)

    expect_equal(result[[1]],
                 varRangeClass$new(list(newIndexRange(2)), varName = 'y', fromStochRule = TRUE))
    expect_equal(result[[2]],
                 varRangeClass$new(list(newIndexRange(3)), varName = 'y'))

    result <- getDependencies(modelDef, 'y[2]', downstream = TRUE)
    expect_length(result, 3)
    expect_equal(result[[3]],
                 varRangeClass$new(list(newIndexRange(4)), varName = 'y'))

    result <- getParents(modelDef, 'y[3]')
    expect_length(result, 1)
    expect_equal(result[[1]],
                 varRangeClass$new(list(newIndexRange(2)), varName = 'y'))

    result <- getParents(modelDef, 'y[3]', upstream = TRUE)
    expect_length(result, 2)
    expect_equal(result[[2]],
                 varRangeClass$new(list(newIndexRange(1)), varName = 'y'))

    
    code <- quote({
        for(i in 1:5)
            y[i] ~ dnorm(z[i], sd = tau)
        for(i in 2:5)
            z[i] ~ dnorm(z[i-1], sd = sigma)
        z[1] ~ dunif(0, 1)
        tau ~ dunif(0, bnd)
        sigma ~ dunif(0, 1)
    })
    modelDef <- modelDefClass$new(code)

    result <- getNodes(modelDef, topOnly = TRUE)
    expect_length(result, 3)
    expect_identical(sapply(result, function(node) node$varName),
                     c('z','tau','sigma'))      
    expect_equal(result[[1]]$indexRanges, 
                     list(newIndexRange(1)))

    result <- getNodes(modelDef, latentOnly = TRUE)
    expect_length(result, 3)
    expect_identical(sapply(result, function(node) node$varName),
                     rep('z',3))

    result <- getDependencies(modelDef, 'z')
    expect_length(result, 3)
    expect_identical(sapply(result, function(node) node$varName),
                     c('z','z','y'))
    expect_equal(result[[1]]$indexRanges, 
                     list(newIndexRange(quote(2:5))))
    expect_equal(result[[2]]$indexRanges, 
                     list(newIndexRange(1)))

    result <- getDependencies(modelDef, 'z', self = FALSE)
    expect_length(result, 1)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y'))
    
    result <- getDependencies(modelDef, 'z[3]')
    expect_length(result, 3)
    expect_identical(sapply(result, function(node) node$varName),
                     c('z','z','y'))
    expect_equal(result[[1]]$indexRanges, 
                     list(newIndexRange(quote(3))))
    expect_equal(result[[2]]$indexRanges, 
                     list(newIndexRange(quote(4))))
    expect_equal(result[[3]]$indexRanges, 
                     list(newIndexRange(quote(3))))

    result <- getDependencies(modelDef, 'z[3]', self = FALSE)
    expect_length(result, 2)
    expect_identical(sapply(result, function(node) node$varName),
                     c('y','z'))
    expect_equal(result[[1]]$indexRanges, 
                     list(newIndexRange(quote(3))))
    expect_equal(result[[2]]$indexRanges, 
                     list(newIndexRange(quote(4))))

})

test_that("error trapping for unexpected vars/nodes", {

    code <- quote({
        for(i in 2:4)
        y[i]~dnorm(y[i-1],1)
    })
    modelDef <- modelDefClass$new(code)

    expect_null(getNodes(modelDef, 'x'))
    expect_null(getNodes(modelDef, 'x[3]'))
    expect_null(getNodes(modelDef, 'y[20]'))
    
    expect_null(getDependencies(modelDef, 'x'))
    expect_null(getDependencies(modelDef, 'x[1]'))
    expect_null(getDependencies(modelDef, 'y[20]'))

})

test_that("getNodes handles a split variable", {

    code <- quote({
        for(i in 1:4) 
            y[i] ~ dnorm(theta[i], 1)
        theta[1] ~ dnorm(0,1)
        theta[2:4] <- z[1:3]
    })
    modelDef <- modelDefClass$new(code)

    result <- getNodes(modelDef, 'theta[1:3]')
    expect_length(result, 2)
    expect_identical(sapply(result, function(node) node$varName),
                     rep('theta', 2))
    expect_equal(result[[1]]$indexRanges, 
                     list(newIndexRange(1)))
    expect_equal(result[[2]]$indexRanges, 
                     list(newIndexRange(quote(2:4))))
    
    result <- getNodes(modelDef, 'theta[3]')
    expect_length(result, 1)
    expect_identical(sapply(result, function(node) node$varName),
                     'theta')
    expect_equal(result[[1]]$indexRanges, 
                     list(newIndexRange(quote(2:4))))
    
})

test_that("complicated input varRange", {

    code <- quote({
        for(i in 1:4)
            for(j in 1:3)
                for(k in 1:5)
                    y[i+1,j,k] ~ dnorm(theta[i,j,k], 1)
    })
    modelDef <- modelDefClass$new(code)

    vr <- varRangeClass$new(list(newIndexRange(quote(2:3)),
                                 newIndexRange(matrix(c(2,4,5,1), nrow = 2))),
                            rangeToIndexSlot = list(2, c(1,3)),
                            varName = 'theta')

    result <- getNodes(modelDef, vr, includeRHSonly = TRUE)
    expect_length(result, 1)
    expect_identical(sapply(result, function(node) node$varName),
                     'theta')
    expect_identical(result[[1]]$indexSlotToRange, c(1L,2L,1L))
    expect_equal(result[[1]]$indexRanges,
                     vr$indexRanges[c(2,1)])
    
    result <- getDependencies(modelDef, vr)
    expect_length(result, 2)
    expect_identical(sapply(result, function(node) node$varName),
                     c('theta','y'))
    expect_equal(result[[2]],
                 varRangeClass$new(list(newIndexRange(matrix(c(3,5,5,1), nrow = 2)),
                                        newIndexRange(quote(2:3))),
                            rangeToIndexSlot = list(c(1,3), 2),
                            varName = 'y', fromStochRule = TRUE))
})

test_that("duplicated RHS elements", {
    code <- quote({
        y ~ dnorm(mu, sd = mu)
        mu ~ dnorm(0, 1)
    })
    
    modelDef <- modelDefClass$new(code)
    expect_equal(getParents(modelDef, 'y')[[1]],
                 varRangeClass$new('mu', fromStochRule = TRUE))

    deps <- getDependencies(modelDef, 'mu')
    expect_identical(length(deps), 2L)
    expect_equal(deps[[1]],
                 varRangeClass$new('mu'))
    expect_equal(deps[[2]],
                 varRangeClass$new('y', fromStochRule = TRUE))
    

})

test_that("handling of `self` in graph traversal", {
    ## TODO: may want to do more testing of the `self` cases.
    code <- quote({
        for(i in 1:5) {
            y[i] ~ dnorm(mu[i], sd = 1)
            mu[i] ~ dnorm(theta, sd = 1)
        }
        theta ~ dnorm(0, 1)
    })
    
    modelDef <- modelDefClass$new(code)

    deps <- getDependencies(modelDef, c('mu[1:5]','theta'), self = TRUE)
    expect_equal(deps, list(varRangeClass$new(list(), varName = 'theta'),
                            varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'mu', fromStochRule = TRUE),
                            varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y', fromStochRule = TRUE)))

    deps <- getDependencies(modelDef, c('mu[1:5]','theta'), self = FALSE)

    expect_equal(deps, list(varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y', fromStochRule = TRUE)))
    deps <- getDependencies(modelDef, c('mu[1:3]','theta'), self = FALSE)
    expect_equal(deps, list(varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y', fromStochRule = TRUE),
                            varRangeClass$new(list(newIndexRange(quote(4:5))), varName = 'mu')))
    
    deps <- getDependencies(modelDef, c('mu[1:3]','theta'), self = TRUE)
    expect_equal(deps, list(varRangeClass$new(list(), varName = 'theta'),
                            varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'mu', fromStochRule = TRUE),
                            varRangeClass$new(list(newIndexRange(quote(4:5))), varName = 'mu'),
                            varRangeClass$new(list(newIndexRange(quote(1:3))), varName = 'y', fromStochRule = TRUE)))

})



test_that("one-lag Markov structure handled correctly", {
   code <- quote({
       for(i in 2:5)
           mu[i] ~ dnorm(rho*mu[i-1], sd = sigma)
       rho ~ dunif(0, 1)
       sigma ~ dunif(0, 1)
   })
   modelDef <- modelDefClass$new(code)
   
   sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
   topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
   endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
   expect_identical(sortIDs, list('rho' = 1, 'sigma' = 2, 'mu' = list(c(NA, 3, 5, 7), 9),
                                  'lifted_rho_times_mu_oBi_minus_1_cB_L2' = list(c(NA,NA,4,6,8),2)))
   expect_identical(topRules, list('rho' = TRUE, 'sigma' = TRUE, 'mu' = rep(FALSE, 2),
                                   'lifted_rho_times_mu_oBi_minus_1_cB_L2' = rep(FALSE, 2)))
   expect_identical(endRules, list('rho' = FALSE, 'sigma' = FALSE, 'mu' = c(FALSE, TRUE),
                                   'lifted_rho_times_mu_oBi_minus_1_cB_L2' = rep(FALSE, 2)))
   expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$multiSortIDindex, 1L)
   expect_identical(modelDef$calcRules[['lifted_rho_times_mu_oBi_minus_1_cB_L2']]$rules[[1]]$multiSortIDindex, 1L)

   ## now time in other direction
   code <- quote({
       for(i in 1:4)
           mu[i] ~ dnorm(rho*mu[i+1], sd = sigma)
       rho ~ dunif(0, 1)
       sigma ~ dunif(0, 1)
   })
   
   modelDef <- modelDefClass$new(code)
   
   sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
   topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
   endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
   expect_identical(sortIDs, list('rho' = 1, 'sigma' = 2, 'mu' = list(c(NA,7,5,3), 9),
                                   'lifted_rho_times_mu_oBi_plus_1_cB_L2' = list(c(8,6,4),2)))
   expect_identical(topRules, list('rho' = TRUE, 'sigma' = TRUE, 'mu' = rep(FALSE, 2),
                                    'lifted_rho_times_mu_oBi_plus_1_cB_L2' = rep(FALSE, 2)))
   expect_identical(endRules, list('rho' = FALSE, 'sigma' = FALSE, 'mu' = c(FALSE, TRUE),
                                   'lifted_rho_times_mu_oBi_plus_1_cB_L2' = rep(FALSE, 2)))
   expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$multiSortIDindex, 1L)
   expect_identical(modelDef$calcRules[['lifted_rho_times_mu_oBi_plus_1_cB_L2']]$rules[[1]]$multiSortIDindex, 1L)
})

test_that("standard one-lag SSM handled correctly", {
    ## Leaving `rho` out to avoid more complicated lifted node case (tested above).
    code <- quote({
        for(i in 1:5) 
            y[i] ~ dnorm(mu[i], sd = sigma)
        for(i in 2:5)
            mu[i] ~ dnorm(mu[i-1], sd = sigma)
        mu[1] ~ dnorm(0, 1)
        #rho ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
    })
    modelDef <- modelDefClass$new(code)
     
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,5,2,c(NA,NA,3,4)), 'sigma' = 1,
                                   'y' = rep(6, 3)))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 3)), 'sigma' = TRUE,
                                    'y' = rep(FALSE, 3)))
    expect_identical(endRules, list('mu' = rep(FALSE, 4), 'sigma' = FALSE, 
                                    'y' = rep(TRUE, 3)))
    expect_identical(modelDef$calcRules[['mu']]$rules[[4]]$multiSortIDindex, 1L)
})

test_that("one-lag SSM with complicated dependencies handled correctly", {
    ## Not clear this is actually testing all that much, since exact mu->y dependencies
    ## don't matter.
    code <- quote({
        for(i in 1:5) 
            y[i] ~ dnorm(mu[k[i]], sd = tau)
        for(i in 2:5) {
            mu[i] ~ dnorm(mu[i-1], sd = sigma)
        }
        mu[1] ~ dnorm(0, 1)
        sigma ~ dunif(0, 1)
        tau ~ dunif(0, 1)
    })
    modelDef <- modelDefClass$new(code, constants = list(k = c(5,3,1,2,4)))
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,5,2,c(NA,NA,3,4)), 'sigma' = 1, 'tau' = 5,
                                   'y' = rep(6, 4)))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 3)), 'sigma' = TRUE, 'tau' = TRUE,
                                    'y' = rep(FALSE, 4)))
    expect_identical(endRules, list('mu' = rep(FALSE, 4), 'sigma' = FALSE, 'tau' = FALSE,
                                    'y' = rep(TRUE, 4)))
    expect_identical(modelDef$calcRules[['mu']]$rules[[4]]$multiSortIDindex, 1L)
    
})

test_that("one-lag Markov structure with intermediate variable handled correctly", {
    code <- quote({
        for(i in 1:5) 
            y[i] ~ dnorm(mu[i], sd = tau)
        for(i in 2:5) {
            mu[i] ~ dnorm(z[i], sd = sigma)
            z[i] <- rho*mu[i-1]
        }
        mu[1] ~ dnorm(0, 1)
        rho ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
        tau ~ dunif(0, 1)
     })
    modelDef <- modelDefClass$new(code)
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1, c(NA,3,5,7), 9), 'rho' = 1, 'sigma' = 2,
                                   'tau' = 9, 'y' = rep(10, 3), 'z' = list(2, c(NA,NA,4,6,8))))
    expect_identical(topRules, list('mu' = c(TRUE, FALSE, FALSE), 'rho' = TRUE, 'sigma' = TRUE,
                                    'tau' = TRUE, 'y' = rep(FALSE, 3), 'z' = rep(FALSE, 2)))
    expect_identical(endRules, list('mu' = rep(FALSE, 3), 'rho' = FALSE, 'sigma' = FALSE,
                                    'tau' = FALSE, 'y' = rep(TRUE, 3), 'z' = rep(FALSE, 2)))

    expect_identical(modelDef$calcRules[['z']]$rules[[2]]$multiSortIDindex, 1L)
    expect_identical(modelDef$calcRules[['mu']]$rules[[2]]$multiSortIDindex, 1L)
})


test_that("one-lag Markov structure with two intermediate variables, multivariate, and complicated indexing handled correctly", {
    code <- quote({
        for(j in 1:3) {
            tmp[j,1:3,1:3] <- tau[j]*sigma[1:3,1:3]
            for(i in 2:5) {
                z[i,1:3,j] ~ dmnorm(b[j,2:4,i], cholesky = tmp[j,1:3,1:3], prec_param = 1)
                b[j,2:4,i] <- mu[2:4, i, j]
                mu[2:4,i,j] <- rho*z[i-1,1:3,j] + beta[1:3]
            }
            tau[j] ~ dunif(0,1)
        }
        for(j in 1:3)
            beta[j] ~ dnorm(0,1)
    })
    modelDef <- modelDefClass$new(code)
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('tau' = 2, 'beta' = 1, 'tmp' = 3,  'b' = c(NA,3,6,9,12),
                                   'z' = list(c(NA,4,7,10), 13), 'mu' = list(c(NA,NA,5,8,11), 2)))
    expect_identical(topRules, list('tau' = TRUE, 'beta' = TRUE, 'tmp' = FALSE, 'b' = FALSE, 'z' = rep(FALSE, 2),
                                    'mu' = rep(FALSE, 2)))
    expect_identical(endRules, list('tau' = FALSE, 'beta' = FALSE, 'tmp' = FALSE, 'b' = FALSE, 'z' = c(FALSE, TRUE),
                                    'mu' = rep(FALSE, 2)))
    expect_identical(modelDef$calcRules[['mu']]$rules[[1]]$multiSortIDindex, 2L)
    expect_identical(modelDef$calcRules[['z']]$rules[[1]]$multiSortIDindex, 1L)
    expect_identical(modelDef$calcRules[['b']]$rules[[1]]$multiSortIDindex, 3L)
})


test_that("AR(p) cases", {
    ## Note that for these cases, we have more fracturing than actually need
    ## (e.g., mu[9] does not need to be split out.
    code <- quote({
        for(i in 3:10)
            mu[i] ~ dnorm(rho1*mu[i-1] + rho2*mu[i-2], sd = sigma)
        rho1 ~ dunif(0, 1)
        rho2 ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
        mu[1] ~ dnorm(0,1)
        mu[2] ~ dnorm(0,1)
        for(i in 1:10)
            y[i] ~ dnorm(mu[i],1)
    })
    modelDef <- modelDefClass$new(code)

    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('rho1' = 1, 'rho2' = 1, 'sigma' = 2,
                                   'mu' = list(1, 1, 17, c(rep(as.numeric(NA),2), 3,5,7,9,11,13), 15),
                                   'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' =
                                       list(2,4, c(rep(as.numeric(NA),4), 6,8,10,12,14), 16),
                                   'y' = rep(18, 5)))
    expect_identical(topRules, list('rho1' = TRUE, 'rho2' = TRUE, 'sigma' = TRUE,
                                    'mu' = c(rep(TRUE, 2), rep(FALSE, 3)),
                                    'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' = rep(FALSE, 4),
                                    'y' = rep(FALSE, 5)))
    expect_identical(endRules, list('rho1' = FALSE, 'rho2' = FALSE, 'sigma' = FALSE,
                                    'mu' = c(rep(FALSE, 5)),
                                    'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' = rep(FALSE, 4),
                                    'y' = rep(TRUE, 5)))

    code <- quote({
        for(i in 3:10)
            mu[i] ~ dnorm(rho1*mu[i-1] + rho2*mu[i-2], sd = sigma)
        rho1 ~ dunif(0, 1)
        rho2 ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
        mu[1] ~ dnorm(0, sd = sigma)
        mu[2] ~ dnorm(rho1*mu[1], sd = sigma)
        for(i in 1:10)
            y[i] ~ dnorm(mu[i],1)
    })
    modelDef <- modelDefClass$new(code)
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('rho1' = 2, 'rho2' = 4, 'sigma' = 1,
                                   'mu' = list(2,4,20, c(rep(as.numeric(NA),2), 6,8,10,12,14,16), 18),
                                   'lifted_rho1_times_mu_oB1_cB' = 3,
                                   'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' =
                                       list(5,7, c(rep(as.numeric(NA),4), 9,11,13,15,17), 19),
                                   'y' = rep(21, 5)))
    expect_identical(topRules, list('rho1' = TRUE, 'rho2' = TRUE, 'sigma' = TRUE,
                                    'mu' = rep(FALSE, 5),
                                    'lifted_rho1_times_mu_oB1_cB' = FALSE,
                                    'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' =
                                        rep(FALSE, 4),
                                    'y' = rep(FALSE, 5)))
    expect_identical(endRules, list('rho1' = FALSE, 'rho2' = FALSE, 'sigma' = FALSE,
                                    'mu' = c(rep(FALSE, 5)),
                                    'lifted_rho1_times_mu_oB1_cB' = FALSE,
                                     'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' =
                                        rep(FALSE, 4),
                                    'y' = rep(TRUE, 5)))

    ## Now with a block rule for starting value
    code <- quote({
        for(i in 3:10)
            mu[i] ~ dnorm(rho1*mu[i-1] + rho2*mu[i-2], sd = sigma)
        rho1 ~ dunif(0, 1)
        rho2 ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
        mu[1] ~ dnorm(0, sd = sigma)
        for(i in 2:2)
        mu[i] ~ dnorm(rho1*mu[i-1], sd = sigma)
        for(i in 1:10)
            y[i] ~ dnorm(mu[i],1)
    })
    modelDef <- modelDefClass$new(code)
       
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('rho1' = 2, 'rho2' = 4, 'sigma' = 1,
                                   'mu' = list(2,4,20, c(rep(as.numeric(NA),2), 6,8,10,12,14,16), 18),
                                   'lifted_rho1_times_mu_oBi_minus_1_cB_L8' = 3,
                                   'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' =
                                       list(5,7, c(rep(as.numeric(NA),4), 9,11,13,15,17), 19),
                                   'y' = rep(21, 5)))
    expect_identical(topRules, list('rho1' = TRUE, 'rho2' = TRUE, 'sigma' = TRUE,
                                    'mu' = rep(FALSE, 5),
                                    'lifted_rho1_times_mu_oBi_minus_1_cB_L8' = FALSE,
                                    'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' =
                                        rep(FALSE, 4),
                                    'y' = rep(FALSE, 5)))
    expect_identical(endRules, list('rho1' = FALSE, 'rho2' = FALSE, 'sigma' = FALSE,
                                    'mu' = c(rep(FALSE, 5)),
                                    'lifted_rho1_times_mu_oBi_minus_1_cB_L8' = FALSE,
                                     'lifted_rho1_times_mu_oBi_minus_1_cB_plus_rho2_times_mu_oBi_minus_2_cB_L2' =
                                        rep(FALSE, 4),
                                    'y' = rep(TRUE, 5)))

})

test_that("only two-lag SSM", {
    ## Note that for this case, the sortIDs are valid, but there are more unique sortIDs than needed.
    code <- quote({
        for(i in 1:10) 
            y[i] ~ dnorm(mu[i], sd = sigma)
        for(i in 3:10)
            mu[i] ~ dnorm(rho*mu[i-2], sd = sigma)
        mu[1] ~ dnorm(0, 1)
        mu[2] ~ dnorm(0, 1)
        rho ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
    })
    modelDef <- modelDefClass$new(code)
        
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,1,c(rep(as.numeric(NA), 2),3,5,7,9,11,13),15), 'rho' = 1, 'sigma' = 2,
                                   'y' = rep(16, 4),
                                   'lifted_rho_times_mu_oBi_minus_2_cB_L4' =
                                       list(2,2,c(rep(as.numeric(NA), 4), 4,6,8,10,12,14))))
    expect_identical(topRules, list('mu' = c(TRUE, TRUE, rep(FALSE, 2)), 'rho' = TRUE, 'sigma' = TRUE,
                                    'y' = rep(FALSE, 4),
                                    'lifted_rho_times_mu_oBi_minus_2_cB_L4' = rep(FALSE, 3)))
    expect_identical(endRules, list('mu' = rep(FALSE, 4), 'rho' = FALSE, 'sigma' = FALSE, 
                                    'y' = rep(TRUE, 4),
                                     'lifted_rho_times_mu_oBi_minus_2_cB_L4' = rep(FALSE, 3)))
})


test_that("standard one-lag SSM with two parts (two focalRules but on one variable)", {
    ## Note that for this case, the sortIDs have some gaps because of ties due to having the two parts.
    code <- quote({
        for(i in 1:5) 
            y[i] ~ dnorm(mu[i], sd = sigma)
        for(i in 2:5)
            mu[i] ~ dnorm(rho*mu[i-1], sd = sigma)
        mu[1] ~ dnorm(0, 1)
        rho ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
        for(i in 101:105)
            mu[i] ~ dnorm(mu[i-1], 1)
        mu[100] ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(code)
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,4,c(NA,3,5,7),10,11,5,
                                               c(rep(as.numeric(NA),101),6,8,9)),
                                   'rho' = 1, 'sigma' = 2,
                                   'y' = rep(11, 3),
                                   'lifted_rho_times_mu_oBi_minus_1_cB_L4' = list(2, c(rep(NA, 2),4,6,8))))
    expect_identical(topRules, list('mu' = c(rep(TRUE, 2), rep(FALSE, 5)), 'rho' = TRUE, 'sigma' = TRUE,
                                    'y' = rep(FALSE, 3),
                                    'lifted_rho_times_mu_oBi_minus_1_cB_L4' = rep(FALSE, 2)))
    expect_identical(endRules, list('mu' = c(rep(FALSE, 4), TRUE, rep(FALSE, 2)), 'rho' = FALSE, 'sigma' = FALSE, 
                                    'y' = rep(TRUE, 3),
                                    'lifted_rho_times_mu_oBi_minus_1_cB_L4' = rep(FALSE, 2)))
})


test_that("two unrelated cycles", {
    ## Note that for this case, the sortIDs have some gaps because of ties due to having the two parts.
    ## Note that for this case, the sortIDs are valid, but there are more unique sortIDs than needed.
    code <- quote({
        for(i in 1:10) 
            y[i] ~ dnorm(mu[i], sd = exp(log_sigma[i]))
        for(i in 2:10) {
            mu[i] ~ dnorm(rho*mu[i-1], 1)
            log_sigma[i] ~ dnorm(rho*log_sigma[i-1], 1)
        }
        mu[1] ~ dnorm(0, 1)
        rho ~ dunif(0, 1)
        log_sigma[1] ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(code)
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(3, c(NA,5,7,9,11,13,15,17,19),22),
                                   'rho' = 1,
                                   'log_sigma' = list(1,c(NA,3,5,7,9,11,13,15,17),21),
                                   'y' = rep(23, 3),
                                   'lifted_rho_times_mu_oBi_minus_1_cB_L4' = list(4, c(NA,NA,6,8,10,12,14,16,18,20)),
                                   'lifted_exp_oPlog_sigma_oBi_cB_cP_L2' = rep(22, 3),
                                   'lifted_rho_times_log_sigma_oBi_minus_1_cB_L5' = list(2, c(NA,NA,4,6,8,10,12,14,16,18))))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 2)), 'rho' = TRUE,
                                    'log_sigma' = c(TRUE, rep(FALSE, 2)), 'y' = rep(FALSE, 3),
                                    'lifted_rho_times_mu_oBi_minus_1_cB_L4' = rep(FALSE, 2),
                                    'lifted_exp_oPlog_sigma_oBi_cB_cP_L2' = rep(FALSE, 3),
                                    'lifted_rho_times_log_sigma_oBi_minus_1_cB_L5' = rep(FALSE, 2)))
    expect_identical(endRules, list('mu' = rep(FALSE, 3), 'rho' = FALSE,
                                    'log_sigma' = rep(FALSE, 3), 'y' = rep(TRUE, 3),
                                    'lifted_rho_times_mu_oBi_minus_1_cB_L4' = rep(FALSE, 2),
                                    'lifted_exp_oPlog_sigma_oBi_cB_cP_L2' = rep(FALSE, 3),
                                    'lifted_rho_times_log_sigma_oBi_minus_1_cB_L5' = rep(FALSE, 2)))                                   
})



test_that("complicated cyclic dependency", {
    ## Case C
    code <- quote({
        for(i in 2:5) {
            z[i] ~ dnorm(mu[i] + z[i-1], sd = tau)
            mu[i] ~ dnorm(mu[i-1], sd = sigma)
        }
        z[1] ~ dnorm(mu[1], sd = tau)
        mu[1] ~ dnorm(0, 1)
        sigma ~ dunif(0, 1)
        tau ~ dunif(0, 1)
     })
    modelDef <- modelDefClass$new(code)

    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,9,2,c(NA,NA,4,6)),
                                   'sigma' = 1, 'tau' = 1,
                                   'z' = list(2,c(NA,4,6,8),11),
                                   'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L2' = list(3,10,c(NA,NA,5,7))))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 3)), 'sigma' = TRUE, 'tau' = TRUE,
                                    'z' = rep(FALSE, 3), 'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L2' = rep(FALSE, 3)))
    expect_identical(endRules, list('mu' = rep(FALSE, 4), 'sigma' = FALSE, 'tau' = FALSE,
                                    'z' = c(rep(FALSE, 2), TRUE), 'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L2' = rep(FALSE, 3)))

    ## Case C, with cycles processed in different order
    code <- quote({
        for(i in 2:5) {
            ## reverse definition order
            mu[i] ~ dnorm(mu[i-1], sd = sigma)
            z[i] ~ dnorm(mu[i] + z[i-1], sd = tau)
        }
        z[1] ~ dnorm(mu[1], sd = tau)
        mu[1] ~ dnorm(0, 1)
        sigma ~ dunif(0, 1)
        tau ~ dunif(0, 1)
     })
    modelDef <- modelDefClass$new(code)

    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,9,2,c(NA,NA,4,6)),
                                   'sigma' = 1, 'tau' = 1,
                                   'z' = list(2,c(NA,4,6,8),11),
                                   'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L3' = list(3,10,c(NA,NA,5,7))))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 3)), 'sigma' = TRUE, 'tau' = TRUE,
                                    'z' = rep(FALSE, 3), 'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L3' = rep(FALSE, 3)))
    expect_identical(endRules, list('mu' = rep(FALSE, 4), 'sigma' = FALSE, 'tau' = FALSE,
                                    'z' = c(rep(FALSE, 2), TRUE), 'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L3' = rep(FALSE, 3)))


    ## Case B
    code <- quote({
        for(i in 2:5) {
            z[i] ~ dnorm(mu[i] + z[i-1], sd = tau)
            mu[i] ~ dnorm(z[i-1], sd = sigma)
        }
        mu[1] ~ dnorm(0, 1)
        z[1] ~ dnorm(mu[1], 1)
        sigma ~ dunif(0, 1)
        tau ~ dunif(0, 1)
     })
    modelDef <- modelDefClass$new(code)
    
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,3, c(rep(as.numeric(NA),2),6,9,12)),
                                   'sigma' = 2, 'tau' = 4,
                                   'z' = list(2,c(NA,5,8,11),14),
                                   'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L2' = list(4, c(NA,NA,7,10,13))))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 2)), 'sigma' = TRUE, 'tau' = TRUE,
                                    'z' = rep(FALSE, 3), 'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L2' = rep(FALSE, 2)))
    expect_identical(endRules, list('mu' = rep(FALSE, 3), 'sigma' = FALSE, 'tau' = FALSE,
                                    'z' = c(FALSE, FALSE, TRUE),
                                    'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L2' = rep(FALSE, 2)))


    ## Case B, with cycles processed in different order
    code <- quote({
        for(i in 2:5) {
            ## mu and z defined in reverse order
            mu[i] ~ dnorm(z[i-1], sd = sigma)
            z[i] ~ dnorm(mu[i] + z[i-1], sd = tau)
        }
        mu[1] ~ dnorm(0, 1)
        z[1] ~ dnorm(mu[1], 1)
        sigma ~ dunif(0, 1)
        tau ~ dunif(0, 1)
     })
    modelDef <- modelDefClass$new(code)
    

    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = list(1,3, c(rep(as.numeric(NA),2),6,9,12)),
                                   'sigma' = 2, 'tau' = 4,
                                   'z' = list(2,c(NA,5,8,11),14),
                                   'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L3' = list(4, c(NA,NA,7,10,13))))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 2)), 'sigma' = TRUE, 'tau' = TRUE,
                                    'z' = rep(FALSE, 3), 'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L3' = rep(FALSE, 2)))
    expect_identical(endRules, list('mu' = rep(FALSE, 3), 'sigma' = FALSE, 'tau' = FALSE,
                                    'z' = c(FALSE, FALSE, TRUE),
                                    'lifted_mu_oBi_cB_plus_z_oBi_minus_1_cB_L3' = rep(FALSE, 2)))

})


test_that("fully unrolled cases", {
    ## standard one-lag SSM with two parts (two focalRules but on one variable)
    code <- quote({
        for(i in 1:5) 
            y[i] ~ dnorm(mu[i], sd = sigma)
        for(i in 2:5)
            mu[i] ~ dnorm(rho*mu[i-1], sd = sigma)
        mu[1] ~ dnorm(0, 1)
        rho ~ dunif(0, 1)
        sigma ~ dunif(0, 1)
        for(i in 6:10)
            mu[i] ~ dnorm(mu[i-1], 1)
    })

    expect_message(modelDef <- modelDefClass$new(code), "Detected state-space type structure or cycle")

    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')

    expect_identical(sortIDs, list('mu' = c(1,9,14,10,3,11,12,13,5,7),
                                               'rho' = 1, 'sigma' = 2,
                                   'y' = rep(14, 5),
                                   'lifted_rho_times_mu_oBi_minus_1_cB_L4' = c(2,4,6,8)))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 9)), 'rho' = TRUE, 'sigma' = TRUE,
                                    'y' = rep(FALSE, 5), 'lifted_rho_times_mu_oBi_minus_1_cB_L4' = rep(FALSE, 4)))
    expect_identical(endRules, list('mu' = c(rep(FALSE, 2), TRUE, rep(FALSE, 7)), 'rho' = FALSE, 'sigma' = FALSE, 
                                    'y' = rep(TRUE, 5), 'lifted_rho_times_mu_oBi_minus_1_cB_L4' = rep(FALSE, 4)))

    ## This unrolls because mu[2] has sortID of Inf and it is child of z[3:6]
    code <- quote({
        for(i in 2:7) {
            z[i] ~ dnorm(z[i+1], 1)
            mu[i] ~ dnorm(mu[i-1] + z[i],1)
        }
    })

    expect_message(modelDef <- modelDefClass$new(code), "Detected state-space type structure or cycle")

    ## This unrolls because don't know what to set as sign for mu rule
    code <- quote({
        for(i in 2:7) {
            z[i] ~ dnorm(z[i-1], 1)
            mu[i] ~ dnorm(mu[i-1] + z[i+1],1)
        }
    })
    
    expect_message(modelDef <- modelDefClass$new(code), "Detected state-space type structure or cycle")

    code <- quote({
        for(j in 1:5) {
            y[1,j] ~ dnorm(mu[1,j], sd = tau)
            for(i in 2:5) {
                y[i,j] ~ dnorm(mu[i,j] + y[i-1,j], sd = tau)
            }
        }
        for(j in 2:5) {
            for(i in 1:5) {
                mu[i,j] ~ dnorm(mu[i,j-1], sd = 1)
            }}
        tau ~ dunif(0, 1)
    })
    
    expect_message(modelDef <- modelDefClass$new(code), "Detected state-space type structure or cycle")

    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    ## Note that lifted elements swap the indexing so confusing to chart this out by hand.
    expect_identical(sortIDs, list('tau' = 4, 'mu' = c(4,5,1,2,3,2,4,3),
                                   'y' = c(5,5,5,7,7,13,13,5,5,7,9,13,11,9,11,9,11,7,7,9,9,13,13,11,11),
                                   'lifted_mu_oBi_comma_j_cB_plus_y_oBi_minus_1_comma_j_cB_L4' =
                                       c(6,6,6,8,8,8,6,6,8,8,10,12,10,12,10,12,10,12,10,12)))
    expect_identical(topRules, list('tau' = TRUE, 'mu' = c(rep(FALSE, 2), rep(TRUE, 2), rep(FALSE, 4)),
                                    'y' = rep(FALSE, 25),
                                    'lifted_mu_oBi_comma_j_cB_plus_y_oBi_minus_1_comma_j_cB_L4' = rep(FALSE, 20)))
    expect_identical(endRules, list('tau' = FALSE, 'mu' = c(rep(FALSE, 8)),
                                    'y' = c(rep(FALSE, 5),rep(TRUE,2),rep(FALSE,4),TRUE,rep(FALSE,9),rep(TRUE,2),rep(FALSE,2)),
                                    'lifted_mu_oBi_comma_j_cB_plus_y_oBi_minus_1_comma_j_cB_L4' = rep(FALSE, 20)))

})

test_that("SSM using arbitrary indexing", {
    ## This should be handled by unrolling
    code <- quote({
        for(i in 1:5) 
            mu[i] ~ dnorm(mu[k[i]], sd = tau)
        mu[6] ~ dnorm(0, 1)
        tau ~ dunif(0, 1)
    })
    
    expect_message(modelDef <- modelDefClass$new(code, constants = list(k = c(2,3,4,5,6))),
                   "Detected state-space")
    sortIDs <- lapply(modelDef$calcRules, extractRuleElement, 'sortID')
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(sortIDs, list('mu' = c(1,6,2:5),
                                               'tau' = 1))
    expect_identical(topRules, list('mu' = c(TRUE, rep(FALSE, 5)), 'tau' = TRUE))
    expect_identical(endRules, list('mu' = c(FALSE, TRUE, rep(FALSE, 4)), 'tau' = FALSE))
})


test_that("top/end nodes for SSM with deterministic relationships", {
    code <- quote({
        for(i in 1:5) 
            y[i] <- y[i+1]
    })
    modelDef <- modelDefClass$new(code)
    
    
    topRules <- lapply(modelDef$calcRules, extractRuleElement, 'top')
    endRules <- lapply(modelDef$calcRules, extractRuleElement, 'end')
    expect_identical(topRules, list('y' = rep(TRUE, 3)))
    expect_identical(endRules, list('y' = rep(TRUE, 3)))
})

test_that("actual cycle is trapped", {
    code <- quote({
        mu[1] ~ dnorm(mu[2], 1)
        mu[2] ~ dnorm(mu[1], 1)
    })
    
    expect_error(expect_message(modelDef <- modelDefClass$new(code), "Detected state-space"), "cycle found")

    code <- quote({
        for(i in 2:5) 
            y[i] ~ dnorm(mu[i], 1)
        for(i in 2:5)
            mu[i] ~ dnorm(mu[i-1], 1)
        mu[1] ~ dnorm(mu[5], 1)
    })
    
    expect_error(expect_message(modelDef <- modelDefClass$new(code), "Detected state-space"), "cycle found")

    code <- quote({
        for(i in 2:5)
            y[i] ~ dnorm(mu[i-1], 1)
        for(i in 1:4)
            mu[i] ~ dnorm(y[i+1], 1)
    })
    
    expect_error(expect_message(modelDef <- modelDefClass$new(code), "Detected state-space"), "cycle found")
})


test_that("for loops with arbitrary sets", {
    code <- quote({
        for(i in n) 
            y[i] ~ dnorm(mu, 1)
        mu ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(code, constants = list(n=c(2,3,5)))
    
    
    yNodes <- getNodes(modelDef, 'y')
    expect_equal(yNodes[[1]]$indexRanges[[1]],
                 newIndexRange(c(2,3,5)))

    code <- quote({
        for(i in c(2,3,5))
            for(j in c(2,7))
                y[i,j] ~ dnorm(mu, 1)
        mu ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(code)

    yNodes <- getNodes(modelDef, 'y')
    expect_equal(yNodes[[1]]$indexRanges[[1]],
                 newIndexRange(c(2,3,5)))
    expect_equal(yNodes[[1]]$indexRanges[[2]],
                 newIndexRange(c(2,7)))

    code <- quote({
        y[c(2,3,5)] ~ dmnorm(mu[1:3],sigma[1:3,1:3])
    })
    modelDef <- modelDefClass$new(code)
    
    yNodes <- getNodes(modelDef, 'y')
    expect_equal(yNodes[[1]]$indexRanges[[1]],
                 newIndexRange(c(2,3,5)))

    
})

test_that("SSM with additional pieces handled", {

  code <- quote({
        for(i in 1:5) {
            y[i] ~ dnorm(mu[i], sd = sigma)
            z[i] <- y[i]  # deps below the standard SS structure
            }
        for(i in 2:5)
            mu[i] ~ dnorm(mu[i-1], sd = tau)
        mu[1] ~ dnorm(0, 1)
        sigma ~ dunif(0, 1)

        ## mostly unrelated part of model
        for(i in 1:5) {
            y2[i] ~ dnorm(mu2[i], sd = sigma)
            mu2[i] ~ dnorm(0, 1)
        }
        z2[1] ~ dnorm(y2[1], 1)
        
    })
    modelDef <- modelDefClass$new(code)
    
    
    expect_equal(getDependencies(modelDef, 'sigma', self = FALSE),
                 list(varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y',
                                        fromStochRule = TRUE),
                      varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y2',
                                        fromStochRule = TRUE)))
    expect_equal(modelDef$endRules$y2$rules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(2:5))), varName = 'y2'))
    expect_equal(modelDef$latentRules$y2$rules[[1]]$fullRange,
                 varRangeClass$new(list(newIndexRange(quote(1))), varName = 'y2'))
    
})


test_that("duplicated nodes", {
    code <- quote({
        for(i in 1:3) {
            y[i] ~ dnorm(mu[k[i]], 1)
            mu[i] ~ dnorm(0, 1)
        }
    })
    modelDef <- modelDefClass$new(code, constants = list(k = c(1,1,2)))
    expect_equal(getParents(modelDef, 'y')[[1]],
                 varRangeClass$new(list(newIndexRange(quote(1:2))),
                                   varName = 'mu', fromStochRule = TRUE))
    expect_equal(getDependencies(modelDef, 'mu[1]', self = FALSE)[[1]],
                 varRangeClass$new(list(newIndexRange(quote(1:2))),
                                   varName = 'y', fromStochRule = TRUE))
})

test_that("missing indexing", {
    modelCode <- quote({
        y <- sum(mu[])
    })

    modelDef <- modelDefClass$new(modelCode, dimensions = list(mu = 5))

    expect_equal(getParents(modelDef, 'y')[[1]],
                 varRangeClass$new(list(newIndexRange(quote(1:5))),
                                        varName = 'mu', fromStochRule = FALSE))

    expect_equal(getDependencies(modelDef, 'mu[2]')[[1]],
                 varRangeClass$new(list(), varName = 'y', fromStochRule = FALSE))
    
    expect_identical(getDependencies(modelDef, 'mu[9]'), NULL)

})

