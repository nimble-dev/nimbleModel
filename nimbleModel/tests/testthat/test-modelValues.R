library(nCompiler)
library(nimbleModel)

test_that("basic modelValues class works", {
  varInfo <- list(
    vars = list(
      mu = list(name = "mu", nDim = 1),
      cov =list(name = "cov", nDim = 2)
    )
  )
  debug(nimbleModel:::make_modelValues_nClass)
  mvClass <- nimbleModel:::make_modelValues_nClass(varInfo)
  CmvClass <- nCompile(mvClass)
  obj <- CmvClass$new()
  obj$mu |> as.list()
  sizes <- list(mu = 2, cov = c(3, 4))
  obj$sizes <- sizes
  obj$sizes
  obj$resize(3)
  obj$mu |> as.list()
  obj$cov |> as.list()
})


modelValues_resize <- function(self, m, sizes) {
  # check preservation issues in resizing
  for(v in names(sizes)) {
    this_sizes <- sizes[[v]]
    length(self[[v]]) <<- m
    if(length(this_sizes)==1) {
      for(i in 1:m) self[[v]][[i]] <- numeric(length = this_sizes)
    } else {
      for(i in 1:m) self[[v]][[i]] <- array(0, dim = this_sizes)
    }
  }

}

# prototype of a generated modelValues class.
nL1D <- nList(numericVector())
nL2D <- nList(numericMatrix())

mv_nC <- nClass(
  classname = "modelValues_demo",
  inherit = nimbleModel:::modelValuesBase_nClass,
  compileInfo = list(
    nClass_inherit = list(base = "modelValuesClass_")
  ),
  Rpublic = list(
    initialize = function(...) {
      super$initialize(...)
      if(!isCompiled()) {
         modelValues_demo()
      }
    }
  ),
  Cpublic = list(

    mu = "nList(numericVector())",
    cov = "nList(numericMatrix())",
    modelValues_demo = nFunction(
      fun = function() {
        mu <<- nL1D$new()
        cov <<- nL2D$new()
      },
      compileInfo = list(
        constructor = TRUE
      )
    ),
    set_sizes = nFunction(
      name = "set_sizes",
      fun = function(new_sizes = "RcppList") {
        self$sizes <<- new_sizes
      }
    ),
    resize = nFunction(
      name = "resize",
      fun = function(m) {
        browser()
        modelValues_resize(self, m, sizes)
        # check preservation issues in resizing
      },
      compileInfo = list(
        C_fun = function(m = 'integerScalar') {
          nCpp("resize_one<1>(mu, m, as<Eigen::Tensor<double, 1> >(this->sizes[\"mu\"]))")
          nCpp("resize_one<2>(cov, m, as<Eigen::Tensor<double, 1> >(this->sizes[\"cov\"]))")
          current_nRow_ <<- m
        }
      )
    ),
    getsize = nFunction(
      function() {return(current_nRow_); returnType('integerScalar')}
    )
  )
)

undebug(nCompiler:::simpleTransformationsEnv$CheckOpAssignment)
debug(nCompiler:::compile_labelAbstractTypes)
comp <- nCompile(mv_nC)

obj <- mv_nC$new()
obj$sizes <- list(mu = 3, cov = c(2, 4))
obj$mu |> as.list()
obj$resize(2)
obj$cov |> as.list()

obj[["mu"]][[2]] <- 1:3
obj["mu", 2]
obj["mu", 2] <- 4:6
obj["mu", 2]
obj[["mu"]][[2]]

obj[["mu"]][[2]][2] <- 10
obj["mu", 2]
obj["mu", 2][2] <- 11
obj["mu", 2]

Cobj <- comp$new()
Cobj$sizes <- list(mu = 3, cov = c(2, 4))
Cobj$mu
Cobj$cov
Cobj$resize(2)
Cobj$mu |> as.list()
Cobj$cov |> as.list()
## comp <- nCompile(mv_nC, sizes_nC, returnList = TRUE)

## obj <- comp$mv_nC$new()
## obj$sizes <- comp$sizes_nC$new()
## obj$mu <- list(1:3, 2:4)
## obj$mu |> as.list()
## obj$sizes$mu
## obj$set_sizes(list(mu = c(3), cov = c(3,3)))
## obj$sizes$mu
## obj$sizes$cov
## class(obj)
## obj[["mu"]][[1]] <- 5:7
## obj[["mu"]] |> as.list()
## obj["mu"]
