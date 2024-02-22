test_that('varRangeClass', {

    ## erroneous instantiation
    expect_error(varRangeClass$new(newIndexRange(4), varName = 'x'),
                 "must be initialized from a list of `indexRange`s")
    
    ## 0D
    xVar <- varRangeClass$new('x')
    expect_true(xVar$isNone())
    expect_identical(xVar$indexRanges, list())
    expect_identical(xVar$toExpr(), quote(x))
    
    ## 1D

    expr <- quote(x[2])
    xVar1 <- varRangeClass$new(deparse(expr))
    xVar2 <- varRangeClass$new(list(newIndexRange(expr[[3]])), varName = as.name(expr[[2]]))
    expect_true(nimbleModel:::varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     'indexRangeScalarClass')
    expect_identical(xVar2$rangeToIndexSlot, list(1L))
    expect_identical(xVar2$indexSlotToRange, 1L)
    expect_identical(xVar2$toExpr(), expr)
    
    expr <- quote(x[2:4])
    xVar1 <- varRangeClass$new(deparse(expr))
    xVar2 <- varRangeClass$new(list(newIndexRange(expr[[3]])), varName = as.name(expr[[2]]))
    expect_true(nimbleModel:::varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     'indexRangeSequenceClass')
    expect_identical(xVar2$rangeToIndexSlot, list(1L))
    expect_identical(xVar2$indexSlotToRange, 1L)
    expect_identical(xVar2$toExpr(), expr)
    
    expr <- quote(x[c(2,3,5)])
    xVar1 <- varRangeClass$new(deparse(expr))
    xVar2 <- varRangeClass$new(list(newIndexRange(expr[[3]])), varName = as.name(expr[[2]]))
    expect_true(nimbleModel:::varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     'indexRangeMatrixClass')
    expect_identical(xVar2$rangeToIndexSlot, list(1L))
    expect_identical(xVar2$indexSlotToRange, 1L)
    expect_identical(xVar2$toExpr(), expr)

    expr <- quote(x[c(2,3,5,7,9)])
    xVar <- varRangeClass$new(deparse(expr))
    expectedExpr <- quote(x[c(2,3,5,9)])
    expectedExpr[[3]][[4]] <- quote(...)
    expect_identical(xVar$toExpr(), expectedExpr)

    ## 2D
    
    expr <- quote(x[2:4, 3:5])
    xVar1 <- varRangeClass$new(deparse(expr))
    xVar2 <- varRangeClass$new(list(newIndexRange(expr[[3]]), newIndexRange(expr[[4]])),
                               varName = as.name(expr[[2]]))
    expect_true(nimbleModel:::varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     rep('indexRangeSequenceClass', 2))
    expect_identical(xVar2$rangeToIndexSlot, list(1L,2L))
    expect_identical(xVar2$indexSlotToRange, 1:2)
    expect_identical(xVar2$toExpr(), expr)
    
    expr <- quote(x[c(2,3,5), c(1,4)])
    xVar1 <- varRangeClass$new(deparse(expr))
    xVar2 <- varRangeClass$new(list(newIndexRange(expr[[3]]),newIndexRange(expr[[4]])),
                               varName = as.name(expr[[2]]))
    expect_true(nimbleModel:::varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     rep('indexRangeMatrixClass', 2))
    expect_identical(xVar2$rangeToIndexSlot, list(1L,2L))
    expect_identical(xVar2$indexSlotToRange, 1:2)
    expect_identical(xVar2$toExpr(), expr)

    xVar <- varRangeClass$new(list(newIndexRange(matrix(c(2,3,5,1,2,4), ncol = 2))),
                               varName = 'x')
    expect_identical(sapply(xVar$indexRanges, function(x) class(x)[1]),
                     'indexRangeMatrixClass')
    expect_identical(xVar$rangeToIndexSlot, list(1:2))
    expect_identical(xVar$indexSlotToRange, rep(1L,2))
    expr <- quote(x[1,1])
    expr[[3]] <- expr[[4]] <- quote(...)
    expect_identical(xVar$toExpr(), expr)

    ## 3D
    
    xVar <- varRangeClass$new(list(newIndexRange(matrix(c(2,3,5,1,2,4), ncol = 2)),
                                   newIndexRange(quote(2:3))),
                                    varName = 'x')
    expect_identical(sapply(xVar$indexRanges, function(x) class(x)[1]),
                     c('indexRangeMatrixClass', 'indexRangeSequenceClass'))
    expect_identical(xVar$rangeToIndexSlot, list(1:2, 3L))
    expect_identical(xVar$indexSlotToRange, c(1L, 1L, 2L))
    expr <- quote(x[1,1,2:3])
    expr[[3]] <- expr[[4]] <- quote(...)
    expect_identical(xVar$toExpr(), expr)

    xVar <- varRangeClass$new(list(newIndexRange(matrix(c(2,3,5,1,2,4), ncol = 2)),
                                    newIndexRange(quote(2:3))), rangeToIndex = list(c(3,1),2),
                                    varName = 'x')
    expect_identical(sapply(xVar$indexRanges, function(x) class(x)[1]),
                     c('indexRangeMatrixClass', 'indexRangeSequenceClass'))
    expect_identical(xVar$rangeToIndexSlot, list(c(3L,1L), 2L))
    expect_identical(xVar$indexSlotToRange, c(1L, 2L, 1L))
    expr <- quote(x[1,2:3,1])
    expr[[3]] <- expr[[5]] <- quote(...)
    expect_identical(xVar$toExpr(), expr)
})


test_that("extractIndexRange", {
    
    xVar <- varRangeClass$new("x[2:4]")

    result <- xVar$extractIndexRange(2)
    expect_equal(result, newIndexRange(NULL))

    result <- xVar$extractIndexRange(1)
    expect_equal(result, newIndexRange(quote(2:4)))
    
    expr <- quote(x[c(2,3,5), 3:4])
    xVar <- varRangeClass$new(deparse(expr))
    result <- xVar$extractIndexRange(2)
    expect_equal(result, newIndexRange(expr[[4]]))
    
    result <- xVar$extractIndexRange(1:2)
    expect_equal(result, newIndexRange(as.matrix(expand.grid(eval(expr[[3]]), eval(expr[[4]])))))

    vals <- matrix(c(2,3,5,1,2,4), ncol = 2)
    xVar <- varRangeClass$new(list(newIndexRange(vals),
                                   newIndexRange(quote(2:3))), rangeToIndex = list(c(3,1),2),
                                   varName = 'x')
    fullResult <- xVar$extractIndexRange(1:3)
    mat <- expand.grid(seq_len(nrow(vals)), 2:3)
    mat <- cbind(vals[mat[ , 1], ], mat[ , 2])
    fullExpected <- newIndexRange(mat[ , c(2,3,1)])
    expect_equal(fullResult, fullExpected)

    ## various orderings and breaking up matrix range
    inds <- c(3,1,2)
    result <- xVar$extractIndexRange(inds)
    expect_equal(result, newIndexRange(fullExpected$values[ , inds]))

    inds <- c(2,3)
    result <- xVar$extractIndexRange(inds)
    expect_equal(result, newIndexRange(fullExpected$values[ , inds]))

    inds <- c(3,2)
    result <- xVar$extractIndexRange(inds)
    expect_equal(result, newIndexRange(fullExpected$values[ , inds]))    

    inds <- c(1,3)
    result <- xVar$extractIndexRange(inds)
    expect_equal(result, newIndexRange(unique(fullExpected$values[ , inds])))

    inds <- c(3,1)
    result <- xVar$extractIndexRange(inds)
    expect_equal(result, newIndexRange(unique(fullExpected$values[ , inds])))

})


test_that("getMinMax", {
        xVar <- varRangeClass$new(list(newIndexRange(matrix(c(2,3,5,4,1,2), ncol = 2)),
                                       newIndexRange(quote(2:7))),
                                  rangeToIndexSlot = list(c(3,1), 2),
                                  varName = 'x')
        result <- xVar$getMinMax()
        expect_identical(result,
                         matrix(c(1,2,2,4,7,5), 3))
})

test_that("toRule", {
    xVar <- varRangeClass$new(list(newIndexRange(matrix(c(2,3,5,4,1,2), ncol = 2)),
                                   newIndexRange(quote(2:7)),
                                   newIndexRange(3),
                                   newIndexRange(c(7,1,3))),
                                  rangeToIndexSlot = list(c(2,4), 1, 3, 5),
                                  varName = 'x')
    xRule <- xVar$toRule()
    expect_identical(as.integer(xRule$indexSlotToSet), xVar$indexSlotToRange)
    expect_identical(sapply(xRule$fullRule$indexRules, function(rule) class(rule)[1]),
                     c('indexRuleArbitraryClass','indexRuleBlockClass','indexRuleBlockClass','indexRuleArbitraryClass'))
    newVar <- xRule$fullRange
    expect_identical(newVar$indexSlotToRange, c(1L,2L,3L,2L,4L))
    expect_equal(xVar$indexRanges, newVar$indexRanges[c(2,1,3,4)])
})

test_that("toVarChars works correctly", {
    vr <- varRangeClass$new("y[2, 1:5]")
    expect_identical(vr$toVarChars(), "y[2, 1:5]")
    vr <- varRangeClass$new(list(newIndexRange(matrix(c(2,4,5), ncol = 1)),
                                 newIndexRange(quote(3:4))), varName = "y")
    expect_identical(vr$toVarChars(), paste0("y[", c(2,4,5), ", 3:4]"))
    
    code <- quote({
        for(i in 1:4)
            for(j in 1:2)
                for(k in 1:3)
                y[idx[k], i+1,3,j,i,2:4]~ dmnorm(z[1:3],pr[1:3,1:3])
    })
    
    md <- modelDefClass$new(code, constants = list(idx = c(2,5,4)))
    vr <- getDependencies(md, 'z', self=FALSE)[[1]]
    expect_identical(vr$toVarChars(),
                     c("y[2, 2, 3, 1:2, 1, 2:4]", "y[2, 3, 3, 1:2, 2, 2:4]", "y[2, 4, 3, 1:2, 3, 2:4]", "y[2, 5, 3, 1:2, 4, 2:4]", "y[4, 2, 3, 1:2, 1, 2:4]", "y[4, 3, 3, 1:2, 2, 2:4]", "y[4, 4, 3, 1:2, 3, 2:4]", "y[4, 5, 3, 1:2, 4, 2:4]", "y[5, 2, 3, 1:2, 1, 2:4]", "y[5, 3, 3, 1:2, 2, 2:4]", "y[5, 4, 3, 1:2, 3, 2:4]", "y[5, 5, 3, 1:2, 4, 2:4]"))
})

                     
