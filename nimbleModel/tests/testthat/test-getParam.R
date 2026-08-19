library(nimbleModel)
library(testthat)

code <- quote({
  for(i in 3:10)
    y[i] ~ dnorm(0,1)
})
data <- list(y = rnorm(10))
mclass <- nimbleModel(code, data = data, returnClass = TRUE)
m <- mclass$new()
options(error=recover)
cmclass <- nCompiler::nCompile(mclass)
cm <- cmclass$new()

vr <- varRangeClass$new('y[3]')
instr <- instr_nClass$new(vr)
test <- makeInstrList(m, vr)
test |> length()
test[[1]]$declID
debug(cm$getParam)
cm$getParam(test[[1]], "value")
