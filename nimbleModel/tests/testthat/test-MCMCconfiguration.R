## This doesn't yet have formal tests, but does illustrate
## how to create and add to an MCMC configuration, using
## a nodeRange or varRange.

library(nimbleModel)
code <- quote({
    for(i in 1:5) {
        y[i] ~ dnorm(mu, sd = sigma)
    }
    mu ~ dnorm(0,1)
    sigma ~ dunif(0,5)
})

m <- modelClass$new(code, data = list(y = rnorm(5)))

conf <- nimbleModel:::mcmcConfClass$new(m)

nr=getNodes(m, 'mu')[[1]]
vr=varRangeClass$new('mu')

conf$addSampler('mu', 'conjugate')
conf$addSampler(nr, 'conjugate')
conf$addSampler(vr, 'conjugate')
conf$addSampler('mu', 'slice')
conf$addSampler(nr, 'slice')
conf$addSampler(vr, 'slice')

# There are some bugs preventing some of remaining invocations from working.
# Revisit all this more comprehensively when make this into formal testing.
if(FALSE) {
conf$addSampler(c('mu','sigma'), 'conjugate')

conf$addSampler(c('mu','sigma'), 'RW_block')  


conf$addSampler(getNodes(m, includeData=FALSE), 'conjugate')
conf$addSampler(getNodes(m, includeData=FALSE), 'RW_block')

library(nimbleModel)
code <- quote({
    for(i in 1:5) {
        y[i] ~ dnorm(mu[i], sd = sigma)
        mu[i] ~ dnorm(mu0, 1)
    }
    mu0 ~ dnorm(0,1)
    sigma ~ dunif(0,5)
})

m <- modelClass$new(code, data = list(y = rnorm(5)))
conf <- nimbleModel:::mcmcConfClass$new(m)

conf$addSampler('mu[1]','RW')
conf$addSampler('mu[1]','conjugate')
conf$addSampler('mu[1:5]','conjugate')
nr <- getNodes(m, "mu[1:3]", includeData=FALSE)[[1]]
conf$addSampler(nr,'conjugate')

conf$addSampler('mu[1:5]','RW_block')
conf$addSampler(nr, 'RW_block')  # Note this will assign RW_block to each node (i.e., invalid).

conf$addSampler(nr, "RW", targetAsScalars = TRUE)
conf$addSampler("mu[1:5]", "RW", targetAsScalars = TRUE)


library(nimbleModel)
code <- quote({
    for(i in 1:3) {
        y[i,1:5]~dmnorm(mu[1:5],pr[1:5,1:5])
    }
    mu[1:5]~dmnorm(z[1:5],pr[1:5,1:5])
    mu[6] ~ dnorm(0,1)
})


m <- modelClass$new(code, data = list(y = matrix(rnorm(15),3,5)))
conf <- nimbleModel:::mcmcConfClass$new(m)

conf$addSampler("mu[1:2]",'conjugate')
conf$addSampler("mu[1:2]",'RW_block')
conf$addSampler("mu[3:7]",'conjugate')
conf$addSampler("mu[3:7]",'RW_block')

library(nimbleModel)
code <- quote({
    for(i in 1:3) {
        y[i,1:5]~dmnorm(mu[i,1:5],pr[1:5,1:5])
        mu[i, 1:5]~dmnorm(z[1:5],pr[1:5,1:5])
    }
})


m <- modelClass$new(code, data = list(y = matrix(rnorm(15),3,5)))
conf <- nimbleModel:::mcmcConfClass$new(m)
nr1 <- getNodes(m, "mu[1,1:5]")[[1]]
nr2 <- getNodes(m, "mu[1:2,1:5]")[[1]]

conf$addSampler(nr1, "RW_block") # one sampler
conf$addSampler(nr2, "RW_block") # two samplers, one samplerConf
conf$addSampler("mu[1:2,1:5]", "RW_block")

conf$addSampler("mu[1:2,1:5]", "RW", targetAsScalars=TRUE)
conf$addSampler(nr1, "RW", targetAsScalars = TRUE)
conf$addSampler(nr2, "RW", targetAsScalars = TRUE)


}
