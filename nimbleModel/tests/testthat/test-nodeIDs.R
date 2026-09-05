test_that("basic tests of mapping indexing to nodes via originalIndexingRule$apply_reverse", {

  code <- nimbleCode({
    for(i in 1:10)
      y[i] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  indexingRange <- decl$originalIndexingRule$apply('y[1:5]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[1:5]")

  indexingRange <- decl$originalIndexingRule$apply('y[3:5]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[3:5]")

  indexingRange <- decl$originalIndexingRule$apply('y[c(2,4)]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[c(2, 4)]")

  code <- nimbleCode({
    for(i in c(2,4,7,9))
      y[i] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  indexingRange <- decl$originalIndexingRule$apply('y[c(4,9)]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[c(4, 9)]")
  indexingRange <- decl$originalIndexingRule$apply('y[4:9]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[c(4, 7, 9)]")

  code <- nimbleCode({
    for(i in 1:10)
      y[i+2] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  indexingRange <- decl$originalIndexingRule$apply('y[3:7]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[3:7]")

  indexingRange <- decl$originalIndexingRule$apply('y[5:7]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[5:7]")

  indexingRange <- decl$originalIndexingRule$apply('y[c(5,7)]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[c(5, 7)]")

  code <- nimbleCode({
    for(i in c(2,4,7,9))
      y[i+2] ~ dnorm(0,1) # 4,6,9,11
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  indexingRange <- decl$originalIndexingRule$apply('y[3:7]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[c(4, 6)]")

  indexingRange <- decl$originalIndexingRule$apply('y[c(6,9)]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[c(6, 9)]")

  code <- nimbleCode({
    for(i in 1:3)
      for(j in 1:4)
        for(k in 1:5)
          y[i,j+1,k+2] ~ dnorm(0,1)
  })
  
  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  indexingRange <- decl$originalIndexingRule$apply('y[2:3,2:4,3:5]')
  expect_identical(decl$originalIndexingRule$apply_reverse(indexingRange, decl)$toVarRange()$toChar(), "y[2:3, 2:4, 3:5]")

})

test_that("basic tests of nodeIDs", {

  code <- nimbleCode({
    for(i in 1:10)
      y[i] ~ dnorm(0,1)
  })
  
  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]
  
  expect_identical(decl$getIDs(varRangeClass$new('y[5]')), 5)
  expect_identical(decl$getOriginalIndexing(5)$toChar(),
                   'y[5]')
  expect_identical(decl$getIDs(varRangeClass$new('y[1:5]')), 1:5)
  expect_identical(decl$getOriginalIndexing(1:5)$toChar(),
                   'y[1:5]')
  expect_identical(decl$getIDs(varRangeClass$new('y[3:5]')), 3:5)
  expect_identical(decl$getOriginalIndexing(3:5)$toChar(),
                   'y[3:5]')

  expect_identical(decl$getIDs(varRangeClass$new('y[9:14]')), 9:14)
  expect_identical(decl$getOriginalIndexing(9:14)$toChar(),
                   'y[9:14]')
  
  expect_identical(decl$getIDs(varRangeClass$new('y[c(2, 4)]')), c(2,4))
  expect_identical(decl$getOriginalIndexing(c(2,4))$toChar(),
                   'y[c(2, 4)]')

  code <- nimbleCode({
    for(i in c(2,4,7,9))
      y[i] ~ dnorm(0,1)
  })
  
  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]
  
  expect_identical(decl$getIDs(varRangeClass$new('y[1:5]')), 1:2)
  expect_identical(decl$getOriginalIndexing(1:2)$toChar(),
                   'y[c(2, 4)]')

  expect_identical(decl$getIDs(varRangeClass$new('y[c(4, 9)]')), c(2L,4L))
  expect_identical(decl$getOriginalIndexing(c(2,4))$toChar(),
                   'y[c(4, 9)]')

  code <- nimbleCode({
    for(i in 3:10)
      y[i] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  expect_identical(decl$getIDs(varRangeClass$new('y[3:7]')), 1:5)
  expect_identical(decl$getOriginalIndexing(1:5)$toChar(),
                   'y[3:7]')

  expect_identical(decl$getIDs(varRangeClass$new('y[5:7]')), 3:5)
  expect_identical(decl$getOriginalIndexing(3:5)$toChar(),
                   'y[5:7]')

  expect_identical(decl$getIDs(varRangeClass$new('y[c(5,7)]')), c(3,5))
  expect_identical(decl$getOriginalIndexing(c(3,5))$toChar(),
                   'y[c(5, 7)]')

})

test_that("nodeIDs with multiple loops", {

  code <- nimbleCode({
    for(i in 1:3)
      for(j in 1:4)
        for(k in 1:5)
          y[i,j,k] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  expect_identical(decl$getIDs(varRangeClass$new('y[2,4,3]')),
                   38)
  expect_identical(decl$getOriginalIndexing(38)$toChar(),
                   'y[2, 4, 3]')

  ids <- as.numeric(c(28:30,33:35,38:40,48:50,53:55,58:60))
  expect_identical(decl$getIDs(varRangeClass$new('y[2:3,2:4,3:5]')),
                   ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[2:3, 2:4, 3:5]')

  # do with arbitrary subsetting, separable
  ids <- c(8,10,13,15,18,20,48,50,53,55,58,60)
  expect_identical(decl$getIDs(varRangeClass$new('y[c(1,3),2:4,c(3,5)]')),
                   ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[c(1, 3), 2:4, c(3, 5)]')
  
  # do with arbitrary subsetting, nonseparable
  ids <- c(30,35,40,46,51,56)
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(2:4)),
                                          newIndexRange(matrix(c(5,2,1,3),ncol=2,byrow=TRUE))),
                                     rangeToIndexSlot=list(2,c(3,1)), varName = 'y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$extractIndexRange(1:3)$getValuesAsMatrix(),
                   matrix(c(rep(2,3), rep(3,3), 2:4,2:4, rep(5,3), rep(1,3)), ncol = 3)) 


  code <- nimbleCode({
    for(i in 3:5)
      for(j in 2:5)
        for(k in 4:8)
            y[i,j,k] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- as.numeric(c(28:30,33:35,38:40,48:50,53:55,58:60))
  indexingRange <- decl$originalIndexingRule$apply('y[4:5,3:5,6:8]')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[4:5, 3:5, 6:8]')


  # do with arbitrary subsetting, separable
  indexingRange <- decl$originalIndexingRule$apply('y[c(3,5),3:5,c(6,8)]')
  ids <- c(8,10,13,15,18,20,48,50,53,55,58,60)
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[c(3, 5), 3:5, c(6, 8)]')

 
  # do with arbitrary subsetting, nonseparable
  ids <- c(30,35,40,46,51,56)
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(3:5)),
                                          newIndexRange(matrix(c(8,4,4,5),ncol=2,byrow=TRUE))),
                                     rangeToIndexSlot=list(2,c(3,1)), varName = 'y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$extractIndexRange(1:3)$getValuesAsMatrix(),
                   matrix(c(rep(4,3), rep(5,3), 3:5,3:5, rep(8,3), rep(4,3)), ncol = 3)) 

  code <- nimbleCode({
    for(i in c(3,5,7))
      for(j in 2:5)
        for(k in c(10,12,14,16,18))
          y[i,j,k] ~ dnorm(0,1)
  })
  
  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- as.numeric(c(28:30,33:35,38:40,48:50,53:55,58:60))
  indexingRange <- decl$originalIndexingRule$apply('y[c(5,7),3:5,c(14,16,18)]')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[c(5, 7), 3:5, c(14, 16, 18)]')

  # do with arbitrary subsetting, separable
  ids <- c(8,10,13,15,18,20,48,50,53,55,58,60)
  indexingRange <- decl$originalIndexingRule$apply('y[c(3,7),3:5,c(14,18)]')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[c(3, 7), 3:5, c(14, 18)]')

  # do with arbitrary subsetting, nonseparable
  ids <- c(30,35,40,46,51,56)
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(3:5)),
                                          newIndexRange(matrix(c(18,5,10,7),ncol=2,byrow=TRUE))),
                                     rangeToIndexSlot=list(2,c(3,1)), varName = 'y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$extractIndexRange(1:3)$getValuesAsMatrix(),
                   matrix(c(rep(5,3), rep(7,3), 3:5,3:5, rep(18,3), rep(10,3)), ncol = 3))
  
})

test_that("nonseparable loop indexing cases", {

  code <- nimbleCode({
    for(i in 3:12)
      y[i, i+3] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- 3:6
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(5:8))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[5:8]')
  
  ids <- c(3L,6L)
  indexingRange <- varRangeClass$new(list(newIndexRange(matrix(c(5,8),ncol=1))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[c(5, 8)]')

  ids <- 3L
  indexingRange <-  varRangeClass$new(list(newIndexRange(5)),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[5]')


  code <- nimbleCode({
    for(i in c(2,3,5,6,7,8,10,11))
      y[i, i+3] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- 3:6
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(5:8))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[5:8]')

  ids <- c(3L,6L)
  indexingRange <- varRangeClass$new(list(newIndexRange(matrix(c(5,8),ncol=1))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(),
                   'y[c(5, 8)]')

  code <- nimbleCode({
    for(i in 1:3)
      for(j in 100:101)
        y[i+4*j] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]
  
  ids <- c(2L, 4L)
  indexingRange <- varRangeClass$new(list(newIndexRange(matrix(c(1,101,2,101),ncol=2,byrow=TRUE))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$extractIndexRange(1:2)$getValuesAsMatrix(),
                   matrix(c(1:2,101, 101),ncol=2))
   
  ids <- 4L
  indexingRange <- varRangeClass$new(list(newIndexRange(matrix(c(2,101),ncol=2,byrow=TRUE))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(), "y[c(2, 101)]")
  
  # separable and nonseparable
  code <- nimbleCode({
    for(i in 1:3)
      for(j in i:5)
        for(k in 1:2)
          y[i,j,k] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- 1:24
  indexingRange <- decl$originalIndexingRule$apply('y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(11:12)$extractIndexRange(1:3)$getValuesAsMatrix(), matrix(c(2,2,2,2,1,2),ncol=3))
  
  ids <- 12L
  indexingRange <- varRangeClass$new(list(newIndexRange(matrix(c(2,2,2),ncol=3,byrow=TRUE))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)
  expect_identical(decl$getOriginalIndexing(ids)$toChar(), "y[c(2, 2, 2)]")
})

test_that("loop indexing with offsets", {
  code <- nimbleCode({
    for(i in 3:10)
      y[i+2] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- 3:4
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(5:6))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)

  nr <- m$getNodes('y[7:8]')
  expect_identical(nr[[1]]$getIDs(), ids)

  expect_identical(decl$getOriginalIndexing(ids)$toChar(),"y[5:6]")
  
  code <- nimbleCode({
    for(i in 3:10)
      y[i-2] ~ dnorm(0,1)
  })

  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- 3:4
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(5:6))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)

  nr <- m$getNodes('y[3:4]')
  expect_identical(nr[[1]]$getIDs(), ids)

  expect_identical(decl$getOriginalIndexing(ids)$toChar(),"y[5:6]")

  code <- nimbleCode({
      for(i in 3:10)
      for(j in 2:4)
          y[i,j+2] ~ dnorm(0,1)
  })
  
  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]

  ids <- c(8,9,11,12)
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(5:6)), newIndexRange(quote(3:4))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)

  nr <- m$getNodes('y[5:6,5:6]')
  expect_identical(nr[[1]]$getIDs(), ids)

  expect_identical(decl$getOriginalIndexing(ids)$toChar(),"y[5:6, 3:4]")
  
})


test_that("multivariate nodes", {
  code <- nimbleCode({
    for(i in 4:7)
      y[2:4, i-2] ~ dmnorm(mu[1:3], pr[1:3,1:3])
  })
  m <- nimbleModel(code)
  decl <- m$modelDef$declRules$y$rules[[1]]
  
  ids <- 2:3
  indexingRange <- varRangeClass$new(list(newIndexRange(quote(5:6))),varName='y')
  expect_identical(decl$getIDs(indexingRange), ids)

  nr <- m$getNodes('y[2:4, 3:4]')
  expect_identical(nr[[1]]$getIDs(), ids)

  nr <- m$getNodes('y[3:4, 3:4]')
  expect_identical(nr[[1]]$getIDs(), ids)

  expect_identical(decl$getOriginalIndexing(ids)$toChar(),"y[5:6]")
})
