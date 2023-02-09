## check indexID_2 and rangeID_2 are ints

test_that('varRangeClass', {

    ## 0D
    xVar1 <- varRangeClass$new('x')
    xVar2 <- varRangeClass$new(list(indexRange(NULL)), varName = 'x')
    expect_true(varRange_isEqual(xVar1, xVar2))
    expect_true(xVar1$isNone())
    expect_true(is(xVar1$indexRanges[[1]], "indexRangeNoneClass"))
    expect_identical(xVar1$toExpr, quote(x))
    
    ## 1D

    expr <- quote(x[2])
    xVar1 <- varRangeClass$new(as.character(expr))
    xVar2 <- varRangeClass$new(list(indexRange(expr[[3]])), varName = as.name(expr[[2]])
    expect_true(varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     'indexRangeScalarClass')
    expect_identical(rangeID_2_indexID, list(1L))
    expect_identical(indexID_2_rangeID, 1L)
    expect_identical(xvar2$toExpr(), expr)
    
    expr <- quote(x[2:4])
    xVar1 <- varRangeClass$new(as.character(expr))
    xVar2 <- varRangeClass$new(list(indexRange(expr[[3]])), varName = as.name(expr[[2]])
    expect_true(varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     'indexRangeSequenceClass')
    expect_identical(rangeID_2_indexID, list(1L))
    expect_identical(indexID_2_rangeID, 1L)
    expect_identical(xvar2$toExpr(), expr)
    
    expr <- quote(x[c(2,3,5)])
    xVar1 <- varRangeClass$new(as.character(expr))
    xVar2 <- varRangeClass$new(list(indexRange(matrix(expr[[3]]))), varName = as.name(expr[[2]])
    expect_true(varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     'indexRangeSequenceClass')
    expect_identical(rangeID_2_indexID, list(1L))
    expect_identical(indexID_2_rangeID, 1L)
    expect_identical(xvar2$toExpr(), expr)

    expr <- quote(x[c(2,3,5,7,9)])
    xVar <- varRangeClass$new(as.character(expr))
    expectedExpr <- quote(x[2,3,5,9])
    expectedExpr[[5]] <- quote(...)
    expect_identical(xvar$toExpr(), expectedExpr)

    ## 2D
    
    expr <- quote(x[2:4, 3:5])
    xVar1 <- varRangeClass$new(as.character(expr))
    xVar2 <- varRangeClass$new(list(indexRange(expr[[3]]), indexRange(expr[[4]])), varName = as.name(expr[[2]])
    expect_true(varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     rep('indexRangeSequenceClass', 2))
    expect_identical(rangeID_2_indexID, list(1:2))
    expect_identical(indexID_2_rangeID, 1:2)
    expect_identical(xvar2$toExpr(), expr)
    
    expr <- quote(x[c(2,3,5), c(1,4)])
    xVar1 <- varRangeClass$new(as.character(expr))
    xVar2 <- varRangeClass$new(list(indexRange(expr[[3]]),indexRange(expr[[4]])),
                               varName = as.name(expr[[2]])
    expect_true(varRange_isEqual(xVar1, xVar2))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     rep('indexRangeMatrixClass', 2))
    expect_identical(rangeID_2_indexID, list(1:2))
    expect_identical(indexID_2_rangeID, 1:2)
    expect_identical(xvar2$toExpr(), expr)

    xVar2 <- varRangeClass$new(list(indexRange(matrix(c(2,3,5,1,2,4), ncol = 2))), varName = 'x')
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     'indexRangeMatrixClass')
    expect_identical(rangeID_2_indexID, list(1:2))
    expect_identical(indexID_2_rangeID, rep(1L,2))
    expr <- quote(x[1,1])
    expr[[3]] <- expr[[4]] <- quote(...)
    expect_identical(xvar2$toExpr(), expr)

    ## 3D
    
    xVar2 <- varRangeClass$new(list(indexRange(matrix(c(2,3,5,1,2,4), ncol = 2)),
                                    indexRange(quote(2:3)),
                                    varName = 'x'))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     c('indexRangeMatrixClass', 'indexRangeSequenceClass'))
    expect_identical(rangeID_2_indexID, list(1:2, 3))
    expect_identical(indexID_2_rangeID, c(1L, 1L, 2L))
    expr <- quote(x[1,1,2:3])
    expr[[3]] <- expr[[4]] <- quote(...)
    expect_identical(xvar2$toExpr(), expr)

    xVar2 <- varRangeClass$new(list(indexRange(matrix(c(2,3,5,1,2,4), ncol = 2)),
                                    indexRange(quote(2:3)), indexOrders = list(c(3,1),2)
                                    varName = 'x'))
    expect_identical(sapply(xVar1$indexRanges, function(x) class(x)[1]),
                     c('indexRangeMatrixClass', 'indexRangeSequenceClass'))
    expect_identical(rangeID_2_indexID, list(c(3L,1L), 2L))
    expect_identical(indexID_2_rangeID, c(1L, 2L, 1L))
    expr <- quote(x[1,1,2:3])
    expr[[3]] <- expr[[5]] <- quote(...)
    expect_identical(xvar2$toExpr(), expr)

})


test_that("extractIndexRange", {
    
    xVar <- varRangeClass$new("x[2:4]")

    result <- xVar$extractIndexRange(2)
    expect_identical(result, indexRange(NULL))

    result <- xVar$extractIndexRange(1)
    expect_equal(result, indexRange(quote(2:4)))
    
    expr <- quote(x[c(2,3,5), 3:4])
    xVar <- varRangeClass$new(as.character(expr))
    result <- xVar$extractIndexRange(2)
    expect_equal(result, indexRange(expr[[3]]))
    
    result <- xVar$extractIndexRange(1:2)
    expect_equal(result, indexRange(as.matrix(expand.grid(eval(expr[[3]]), eval(expr[[4]])))))

    vals <- matrix(c(2,3,5,1,2,4), ncol = 2)
    xVar <- varRangeClass$new(list(indexRange(vals),
                                   indexRange(quote(2:3)), indexOrders = list(c(3,1),2)
                                   varName = 'x'))
    fullResult <- xVar$extractIndexRange(1:3)
    fullExpected <- indexRange(matrix(expand.grid(vals, 2:3))[ , c(2,3,1)])

    expect_equal(fullResult, fullExpected)

    ## various orderings and breaking up matrix range
    inds <- c(3,1,2)
    result <- xVar$extractIndexRange(c(1,3))
    expect_equal(result, indexRange(fullExpected$values[ , inds]))

    inds <- c(1,3)
    result <- xVar$extractIndexRange(c(1,3))
    expect_equal(result, indexRange(fullExpected$values[ , inds]))

    inds <- c(3,1)
    result <- xVar$extractIndexRange(c(1,3))
    expect_equal(result, indexRange(fullExpected$values[ , inds]))
    
    inds <- c(2,3)
    result <- xVar$extractIndexRange(c(1,3))
    expect_equal(result, indexRange(fullExpected$values[ , inds]))

    inds <- c(3,2)
    result <- xVar$extractIndexRange(c(1,3))
    expect_equal(result, indexRange(fullExpected$values[ , inds]))    
})


