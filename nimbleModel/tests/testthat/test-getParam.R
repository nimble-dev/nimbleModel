library(nCompiler)
library(nimbleModel)

code <- quote({
    for(i in 1:5) {
        y[i] ~ dnorm(mu, var = tau)
    }
  tau ~ dunif(0, 100)
  mu ~ dnorm(0,1)
})

inits <- list(tau = 25, mu = 0)
data <- list(y = rnorm(5))

## "Manual" workflow not using `nimbleModel()`.
nm <- modelClass$new(code, inits = inits, data = data)
mclass <- nimbleModel:::make_modelClass_from_nimbleModel(nm)

nOptions(pause_after_writing_files = FALSE)
test <- nCompile(mclass)

obj <- test$new()
obj$y
obj$calculate()
obj$getParam("tau", 1)
obj$getParam("y[1]", 1)
obj$getParam("y[1]", 3)
obj$getParam("y[1]", 0)
obj$getParam("y[1]", 6)

junk <- nFunction(
  function() {
    a <- 1L
    b <- 2L
    ans <- 1L
    if(a == 1 | b == 3) ans <- 2L
    return(ans)
  },
  argTypes = list(a = "integerScalar", b = "integerScalar"),
  returnType = "integerScalar"
)
cjunk <- nCompile(junk)
cjunk()

altParams <- declInfo$altParamExprs
RHS <- declInfo$calculateCode[[3]]
params <- as.list(declInfo$calculateCode[-c(1:2)])
params

# Direct prototyping

ETsym <- nCompiler:::symbolETaccBase$new(name='')
nc <- nClass(
  Cpublic = list(
    s = 'numericScalar',
    v = 'numericVector',
    m = 'numericMatrix',
    get_inner = nFunction(
      function(i = 'integerScalar', vn = 'string') {
        ans <- self[[vn]]
        return(ans)
        returnType(T(ETsym))
      }
    ),
    get = nFunction(
      function(i = 'integerScalar', vn = 'string') {
        nSwitch(i, 1:2,
                eta <- self[[vn]],
                eta <- self[[vn]])
        res <- as(eta, "numericMatrix")
        return(res)
        returnType("numericMatrix")
      }
    )
  ),
  compileInfo=list(interfaceMembers = c("s","v","m", "get"))
)

cnc <- nCompile(nc)
obj <- cnc$new()
obj$s <- 1.2
obj$v <- c(2.3, 3.4)
obj$m <- matrix(5:10, nrow = 3)
obj$get(1, "s")
obj$get(2, "v")
obj$get(1, "m")

# To-do:
#   Add ETaccess opDef with compileTime copy arg
#   Make generated code avoid a copy for simple model variables.
#
