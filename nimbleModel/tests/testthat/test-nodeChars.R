# Testing aspects of backwards compatibility: use of chars for nodes and use of getNodeNames and expandNodeNames

test_that("use of nodes as characters", {
  code <- nimbleCode({
    for(i in 1:2)
      for(j in 1:3)
        y[i,j]~dnorm(mu,1)
    mu ~dnorm(0,1)
  })
  
  m <- nimbleModel(code)
  nodeRanges <- m$getNodes()
  expect_true(all(sapply(nodeRanges, \(x) inherits(x, 'nodeRangeClass'))))
  setNimbleModelOption('nodesAsChars', TRUE)
  chars <- m$getNodes()
  expect_identical(chars, c("y[1, 1]","y[1, 2]","y[1, 3]","y[2, 1]","y[2, 2]","y[2, 3]","mu"))
  chars <- m$getNodes(returnScalarComponents = TRUE)
  expect_identical(chars, c("y[1, 1]","y[1, 2]","y[1, 3]","y[2, 1]","y[2, 2]","y[2, 3]","mu"))

  deps <- m$getDependencies('mu')
  expect_identical(deps, c("mu", "y[1:2, 1:3]"))
  deps <- m$getDependencies('mu',returnScalarComponents=TRUE)
  expect_identical(deps, c("mu","y[1, 1]","y[1, 2]","y[1, 3]","y[2, 1]","y[2, 2]","y[2, 3]"))
  setNimbleModelOption('nodesAsChars', FALSE)
  varRanges <- m$getDependencies('mu')
  expect_true(all(sapply(varRanges, \(x) inherits(x, 'varRangeClass'))))

  code <- nimbleCode({
    for(i in 1:3)
      y[i,1:2]~dmnorm(mu[1:2],prec[1:2,1:2])
    for(j in 1:2)
      mu[j] ~dnorm(0,1)
  })
  
  m <- nimbleModel(code, inits = list(prec=diag(2)))
  nodeRanges <- m$getNodes()
  expect_true(all(sapply(nodeRanges, \(x) inherits(x, 'nodeRangeClass'))))
  
  setNimbleModelOption('nodesAsChars', TRUE)
  chars <- m$getNodes()
  expect_identical(chars, c("lifted_chol_oPprec_oB1to2_comma_1to2_cB_cP[1:2, 1:2]","y[1, 1:2]","y[2, 1:2]", "y[3, 1:2]", "mu[1]", "mu[2]"))
  chars <- m$getNodes(returnScalarComponents = TRUE)
  expect_identical(chars, c("lifted_chol_oPprec_oB1to2_comma_1to2_cB_cP[1, 1]","lifted_chol_oPprec_oB1to2_comma_1to2_cB_cP[1, 2]","lifted_chol_oPprec_oB1to2_comma_1to2_cB_cP[2, 1]","lifted_chol_oPprec_oB1to2_comma_1to2_cB_cP[2, 2]","y[1, 1]","y[1, 2]","y[2, 1]","y[2, 2]","y[3, 1]","y[3, 2]","mu[1]","mu[2]"))
  deps <- m$getDependencies('mu')
  expect_identical(deps, c("mu[1:2]", "y[1:3, 1:2]"))
  deps <- m$getDependencies('mu',returnScalarComponents = TRUE)
  expect_identical(deps, c("mu[1]","mu[2]","y[1, 1]","y[1, 2]","y[2, 1]","y[2, 2]","y[3, 1]","y[3, 2]"))
  setNimbleModelOption('nodesAsChars', FALSE)
})

test_that("old model API calls", {
  code <- nimbleCode({
    for(i in 1:2)
      for(j in 1:3)
        y[i,j] ~ dnorm(mu + x,1)
    mu ~dnorm(0,1)
  })

  
  m <- nimbleModel(code, data = list(y=matrix(rnorm(6),2)))
  chars <- m$getNodes(nodesAsChars = TRUE)
  expect_identical(chars, c("lifted_mu_plus_x", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]", "mu"))
  chars <- m$getNodes(includeRHSonly = TRUE, nodesAsChars = TRUE)
  expect_identical(chars, c("lifted_mu_plus_x", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]", "mu", "x"))
  chars <- m$getNodes(.sort = TRUE, nodesAsChars = TRUE)
  expect_identical(chars, c("mu", "lifted_mu_plus_x", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]"))
  chars <- m$getNodes(.sort=TRUE, includeRHSonly = TRUE, nodesAsChars = TRUE)
  expect_identical(chars, c("x", "mu", "lifted_mu_plus_x", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]"))

  chars <- m$getNodeNames()
  expect_identical(chars, c("mu", "lifted_mu_plus_x", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]"))
  chars <- m$getNodeNames(includeRHSonly = TRUE)  
  expect_identical(chars, c("x", "mu", "lifted_mu_plus_x", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]"))

  chars <- m$expandNodeNames(c('mu','y','x'))
  expect_identical(chars, c("mu", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]", "x"))
  chars <- m$expandNodeNames(c('mu','y','x'), sort = TRUE)
  expect_identical(chars, c("x", "mu", "y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]"))

  chars <- m$expandNodeNames(c('y','y[2,1]'))
  expect_identical(chars, c("y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]"))
  chars <- m$expandNodeNames(c('y','y[2,1]'), unique = FALSE)
  expect_identical(chars, c("y[1, 1]","y[1, 2]","y[1, 3]", "y[2, 1]", "y[2, 2]", "y[2, 3]", "y[2, 1]"))

  # Check with y[i+1] type stuff to see if indexing is messed up.
  library(nimbleModel)
  code <- nimbleCode({
    for(i in 1:2)
      for(j in 1:3)
        y[i+1,j]~dnorm(mu+x,1)
    mu ~dnorm(0,1)
  })

  m <- nimbleModel(code, data =list(y=matrix(rnorm(9),3)))
  chars <- m$getNodes(nodesAsChars = TRUE)
  expect_identical(chars, c("lifted_mu_plus_x", "y[2, 1]", "y[2, 2]", "y[2, 3]", "y[3, 1]", "y[3, 2]","y[3, 3]", "mu"))
  chars <- m$getNodes(nodesAsChars = TRUE, includeRHSonly = TRUE)
  expect_identical(chars, c("lifted_mu_plus_x", "y[2, 1]", "y[2, 2]", "y[2, 3]", "y[3, 1]", "y[3, 2]","y[3, 3]", "mu", "x"))
  chars <- m$getNodes(nodesAsChars = TRUE, .sort = TRUE)
  expect_identical(chars, c("mu","lifted_mu_plus_x", "y[2, 1]", "y[2, 2]", "y[2, 3]", "y[3, 1]", "y[3, 2]","y[3, 3]"))
  chars <- m$getNodes(nodesAsChars = TRUE, .sort=TRUE,includeRHSonly = TRUE)
  expect_identical(chars, c("x","mu","lifted_mu_plus_x", "y[2, 1]", "y[2, 2]", "y[2, 3]", "y[3, 1]", "y[3, 2]","y[3, 3]"))

  chars <- m$getNodeNames() 
  expect_identical(chars, c("mu","lifted_mu_plus_x", "y[2, 1]", "y[2, 2]", "y[2, 3]", "y[3, 1]", "y[3, 2]","y[3, 3]"))
  chars <- m$getNodeNames(includeRHSonly = TRUE)  
  expect_identical(chars, c("x","mu","lifted_mu_plus_x", "y[2, 1]", "y[2, 2]", "y[2, 3]", "y[3, 1]", "y[3, 2]","y[3, 3]"))

  # Check time series case where sortID varies amongst nodes in a single nodeRange.
  code <- nimbleCode({
    for(i in 1:3)
      y[i] ~ dnorm(y[i+1],1)
  })
  m <- nimbleModel(code, data =list(y=rnorm(4)))

  chars <- m$getNodes(nodesAsChars = TRUE)
  expect_identical(chars, c("y[1]","y[2]","y[3]"))
  chars <- m$getNodes(nodesAsChars = TRUE, includeRHSonly = TRUE)
  expect_identical(chars, c("y[1]","y[2]","y[3]","y[4]"))
  chars <- m$getNodes(nodesAsChars = TRUE, .sort=TRUE)
  expect_identical(chars, c("y[3]","y[2]","y[1]"))
  chars <- m$getNodeNames()
  expect_identical(chars, c("y[3]","y[2]","y[1]"))
  chars <- m$expandNodeNames('y')
  expect_identical(chars, c("y[1]","y[2]","y[3]","y[4]"))
  chars <- m$expandNodeNames('y',sort=TRUE)
  expect_identical(chars, c("y[4]", "y[3]","y[2]","y[1]"))

  # Check returnScalarComponents.
  code <- nimbleCode({
    for(i in 1:3)
      y[i,1:2]~dmnorm(mu[1:2],pr[1:2,1:2])
    for(j in 1:2)
      mu[j] ~ dnorm(0,1)
  })

  m <- nimbleModel(code, data =list(y=matrix(rnorm(6),3)))
  chars <- m$getNodes(returnScalarComponents=TRUE,nodesAsChars=TRUE)
  expect_identical(chars, c("lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 2]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 2]", "y[1, 1]", "y[1, 2]", "y[2, 1]", "y[2, 2]", "y[3, 1]", "y[3, 2]", "mu[1]","mu[2]"))                                         
  chars <- m$getNodes(returnScalarComponents=TRUE,nodesAsChars=TRUE,.sort=TRUE)
  expect_identical(chars, c("lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 2]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 2]", "mu[1]","mu[2]", "y[1, 1]", "y[1, 2]", "y[2, 1]", "y[2, 2]", "y[3, 1]", "y[3, 2]"))                                         

  chars <- m$getNodeNames(returnScalarComponents=TRUE)
  expect_identical(chars, c("lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 2]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 2]", "mu[1]","mu[2]", "y[1, 1]", "y[1, 2]", "y[2, 1]", "y[2, 2]", "y[3, 1]", "y[3, 2]"))                                         

  chars <- m$expandNodeNames('y') 
  expect_identical(chars, c("y[1, 1:2]", "y[2, 1:2]", "y[3, 1:2]"))
  chars <- m$expandNodeNames('y', sort=TRUE)
  expect_identical(chars, c("y[1, 1:2]", "y[2, 1:2]", "y[3, 1:2]"))
  chars <- m$expandNodeNames('y',returnScalarComponents=TRUE) 
  expect_identical(chars, c("y[1, 1]","y[1, 2]","y[2, 1]","y[2, 2]","y[3, 1]","y[3, 2]"))
  chars <- m$expandNodeNames('y',returnScalarComponents=TRUE,sort=TRUE) 
  expect_identical(chars, c("y[1, 1]","y[1, 2]","y[2, 1]","y[2, 2]","y[3, 1]","y[3, 2]"))

  # Check on sorting with scalar components in multivariate case.
  code <- nimbleCode({
    for(i in 1:3)
      y[i,1:2]~dmnorm(y[i+1,1:2],pr[1:2,1:2])
  })

  m <- nimbleModel(code, data =list(y=matrix(rnorm(8),4)))
  chars <- m$getNodes(returnScalarComponents=TRUE,nodesAsChars=TRUE)
  expect_identical(chars, c("lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 2]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 2]", "y[1, 1]", "y[1, 2]", "y[2, 1]", "y[2, 2]", "y[3, 1]", "y[3, 2]"))                                         
  chars <- m$getNodes(returnScalarComponents=TRUE,nodesAsChars=TRUE,.sort=TRUE)
  expect_identical(chars, c("lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 2]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 2]", "y[3, 1]", "y[3, 2]", "y[2, 1]", "y[2, 2]", "y[1, 1]", "y[1, 2]"))                                         

  chars <- m$getNodeNames(returnScalarComponents=TRUE)
  expect_identical(chars, c("lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1, 2]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 1]","lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[2, 2]","y[3, 1]", "y[3, 2]", "y[2, 1]", "y[2, 2]", "y[1, 1]", "y[1, 2]"))                                         

  chars <- m$expandNodeNames('y')
  # This (and additional results below) is not the same as what nimble would give (it would have `y[4, 1:2]`)
  # (see nimbleModel issue #23).
  expect_identical(chars, c("y[1, 1:2]", "y[2, 1:2]", "y[3, 1:2]", "y[4, 1]", "y[4, 2]"))
  
  chars <- m$expandNodeNames('y', sort=TRUE)
  expect_identical(chars, c("y[4, 1]","y[4, 2]","y[3, 1:2]","y[2, 1:2]","y[1, 1:2]"))
  chars <- m$expandNodeNames('y',returnScalarComponents=TRUE) 
  expect_identical(chars, c("y[1, 1]","y[1, 2]","y[2, 1]","y[2, 2]","y[3, 1]","y[3, 2]", "y[4, 1]", "y[4, 2]"))
  chars <- m$expandNodeNames('y',returnScalarComponents=TRUE,sort=TRUE) 
  expect_identical(chars, c("y[4, 1]","y[4, 2]","y[3, 1]","y[3, 2]", "y[2, 1]","y[2, 2]","y[1, 1]","y[1, 2]"))

  # Check on getDependencies/getParents.
  code <- nimbleCode({
    for(i in 1:5)
      y[i] ~ dnorm(mu, tau)
    tau ~ dunif(0,1)
  })
  m <- nimbleModel(code)
  expect_identical(m$getDependencies(c('y','tau'), .sort = TRUE, nodesAsChars = TRUE),
                   c("tau", "lifted_d1_over_sqrt_oPtau_cP", "y[1:5]"))
  expect_identical(m$getParents('y', .sort = TRUE, nodesAsChars = TRUE),
                   c("tau", "lifted_d1_over_sqrt_oPtau_cP", "y[1:5]"))
  expect_identical(m$getParents('y', .sort = TRUE, nodesAsChars = TRUE, returnScalarComponents = TRUE),
                   c("tau", "lifted_d1_over_sqrt_oPtau_cP", paste0("y[", 1:5, "]")))
})

test_that("Use of .sort in cases with multiple and/or overlapping sortID values", {
  code <- nimbleCode({
    for(i in 2:6)
      y[i] ~ dnorm(y[i-1], tau)
    tau ~ dunif(0,1)
    y[1] ~ dnorm(0,1)
  })
  m <- nimbleModel(code)
  expect_identical(m$getNodes(.sort=TRUE,nodesAsChars=TRUE),
                   c("tau","lifted_d1_over_sqrt_oPtau_cP",paste0("y[", 1:6, "]")))
  expect_identical(m$getParents('y', .sort=TRUE, nodesAsChars = TRUE),
                   c('tau','y[1]','lifted_d1_over_sqrt_oPtau_cP',paste0('y[', 2:6, ']')))
  expect_identical(m$getParents('y[4]', .sort=TRUE, nodesAsChars = TRUE),
                   c('tau','lifted_d1_over_sqrt_oPtau_cP',paste0('y[', 3:4, ']')))
  
  
  code <- nimbleCode({
    for(i in 2:6)
      y[i]~dnorm(rho*y[i-1], tau)  # lifted node introduced
    tau ~ dunif(0,1)
    y[1] ~ dnorm(0,1)
  })
  m <- nimbleModel(code)
  truth <- c("y[1]", "tau", "lifted_rho_times_y_oBi_minus_1_cB_L2[2]","lifted_d1_over_sqrt_oPtau_cP", "y[2]", "lifted_rho_times_y_oBi_minus_1_cB_L2[3]", "y[3]", "lifted_rho_times_y_oBi_minus_1_cB_L2[4]","y[4]" ,"lifted_rho_times_y_oBi_minus_1_cB_L2[5]","y[5]" , "lifted_rho_times_y_oBi_minus_1_cB_L2[6]","y[6]")
  expect_identical(m$getNodes(.sort=TRUE,nodesAsChars=TRUE), truth)
  expect_identical(m$getParents('y', .sort=TRUE, nodesAsChars = TRUE), truth)
  expect_identical(m$getParents('y[4]', .sort=TRUE, nodesAsChars = TRUE), c('tau','lifted_d1_over_sqrt_oPtau_cP','y[3]','lifted_rho_times_y_oBi_minus_1_cB_L2[4]', 'y[4]'))
  

  code <- nimbleCode({
    for(i in 1:5)
      y[i]~dnorm(y[i+1], tau)
    tau ~ dunif(0,1)
    y[6] ~ dnorm(0,1)
  })
  m <- nimbleModel(code)
  expect_identical(m$getNodes(.sort=TRUE,nodesAsChars=TRUE), c('tau','lifted_d1_over_sqrt_oPtau_cP',paste0('y[',6:1,']'))
  expect_identical(m$getParents('y', .sort=TRUE, nodesAsChars = TRUE), c('tau','y[6]','lifted_d1_over_sqrt_oPtau_cP',paste0('y[',5:1,']'))truth)
  expect_identical(m$getParents('y[4]', .sort=TRUE, nodesAsChars = TRUE),
                   c('tau','lifted_d1_over_sqrt_oPtau_cP','y[5]','y[4]'))


  code <- nimbleCode({
    for(i in 1:6)
      y[i] <- z[i] + 1
    for(i in 2:6)
      z[i] <- y[i-1] + .5
    z[1] <- 0
  })
  # m <- nimbleModel(code)  # BUG that is fixed in sortID-vec
  # TODO: work on this when BUG fix is incorporated into this branch.

  # check with mv case to see how written out
  code <- nimbleCode({
    for(i in 2:6)
      y[1:2,i]~dmnorm(y[1:2,i-1], pr[1:2,1:2])
  })
  m <- nimbleModel(code)
  truth <- c("lifted_chol_oPpr_oB1to2_comma_1to2_cB_cP[1:2, 1:2]", paste0("y[1:2, ", 2:6, "]"))
  expect_identical(m$getNodes(.sort=TRUE,nodesAsChars=TRUE), truth)
  expect_identical(m$getParents('y', .sort=TRUE, nodesAsChars = TRUE), truth)
  
})


