
test_that('varRangeClass initialized from expr', {

    ## 1D:
    y <- 101:110
    
    xVar <- varRangeClass$new('x')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y)

    xVar <- varRangeClass$new('x[3]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3])

    expect_identical(
        varRange_getSingleIndexRange(xVar, 1),
        xVar$indexRanges[[1]]
    )
    
    xVar <- varRangeClass$new('x[2:10]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[2:10])

    expect_identical(
        varRange_getSingleIndexRange(xVar, 1),
        xVar$indexRanges[[1]]
    )
    
    xVar <- varRangeClass$new('x[]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[])

    ## 2D
    y <- matrix(101:200, nrow = 10)    

    xVar <- varRangeClass$new('x[3, 4]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3, 4])

    expect_identical(
        varRange_getSingleIndexRange(xVar, 1),
        xVar$indexRanges[[1]]
    )

    expect_identical(
        varRange_getSingleIndexRange(xVar, 2),
        xVar$indexRanges[[2]]
    )
    
    xVar <- varRangeClass$new('x[3, 2:4]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3, 2:4])

    expect_identical(
        varRange_getSingleIndexRange(xVar, 1),
        xVar$indexRanges[[1]]
    )

    expect_identical(
        varRange_getSingleIndexRange(xVar, 2),
        xVar$indexRanges[[2]]
    )

    expect_equal(
        varRange_getIndexRangeMatrix(xVar, c(1, 2)),
        indexRange(matrix(c(rep(3, 3), 2:4), ncol = 2))
    )

    expect_equal(
        varRange_getIndexRangeMatrix(xVar, c(1))
        ,
        indexRange(matrix(3))
    )

    expect_equal(
        varRange_getIndexRangeMatrix(xVar, c(2))
        ,
        indexRange(matrix(2:4, ncol = 1))
    )
    
    xVar <- varRangeClass$new('x[3:5, 6]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3:5, 6])

    xVar <- varRangeClass$new('x[2:10, 3:5]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[2:10, 3:5])

    expect_equal(
        varRange_getIndexRangeMatrix(xVar, c(1, 2))
       ,
        indexRange(structure(
            as.matrix(
                expand.grid(2:10, 3:5)
            ),
            dimnames = NULL))
    )
    
    xVar <- varRangeClass$new('x[, 3:5]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[, 3:5])

    xVar <- varRangeClass$new('x[2:5, ]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[2:5, ])

    xVar <- varRangeClass$new('x[, ]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[, ])

})


test_that("varRange initialized with matrix indexRange(s)",{
    expect_silent(xVar <- varRangeClass$new(list(indexRange(matrix(c(2,3,5))), indexRange(matrix(c(3, 7)))),
                                            varName = 'x'))
    ## TODO: flesh out what we want here
    
})

test_that("varRange expr, char, and indexRange replacement", {
    input <- quote(x[3])
    expect_identical(
        varRange2expr( varRangeClass$new(input)),
        input
    )

    input <- quote(x[3, 4:6])
    expect_identical(
        varRange2expr( varRangeClass$new(input)),
        input
    )

    input <- quote(x[3, c(3, 5, 7)])
    expect_identical(
        varRange2expr( varRangeClass$new(input)),
        input
    )

    input <- quote(x[3, 4:6])
    VR <- varRangeClass$new(input)
    VRnew <-  varRangeClass$new(quote(x[c(2, 4, 6), 5])) ## "x" is arbitrary here
    VR$setIndexRanges( VRnew$indexRanges )
    ## equal because the arbitrary index range is evaluated
    ## the replaced case.
    expect_identical(
        VR$indexRanges,
        VRnew$indexRanges
    )
    ## It doesn't appear we can keep indexRangeExprs identical
    expect_identical(VR$indexRangeExprs, list())
    
})

test_that("getIndexRangeMatrix", {
    ## full indices, checking that ordering is correct
    vr <- varRangeClass$new(list(
               indexRange(matrix(c(2,3,1,2), ncol = 2)),
               indexRange(quote(2:3))), indexOrders = list(c(1,3), 2))

    full_result <- matrix(c(2,3,2,3,2,2,3,3,1,2,1,2), ncol = 3)

    inds <- 1:3
    mat <- vr$getIndexRangeMatrix(inds)
    expect_identical(mat, indexRange(full_result[ , inds]))

    ## different order
    inds <- 3:1
    mat <- vr$getIndexRangeMatrix(inds)
    expect_identical(mat, indexRange(full_result[ , inds]))
    
    ## partial indices, not breaking up an indexRange
    inds <- c(1,3)
    mat <- vr$getIndexRangeMatrix(inds)
    expect_identical(mat, indexRange(unique(full_result[ , inds])))

    ## partial indices, not breaking up an indexRange, out of order
    inds <- c(3,1)
    mat <- vr$getIndexRangeMatrix(inds)
    expect_identical(mat, indexRange(unique(full_result[ , inds])))

    ## partial indices, breaking up an indexRange
    inds <- 2:3
    mat <- vr$getIndexRangeMatrix(inds)
    expect_identical(mat, indexRange(full_result[ , inds]))

    ## partial indices, out of order
    inds <- 3:2
    mat <- vr$getIndexRangeMatrix(inds)
    expect_identical(mat, indexRange(full_result[ , inds]))

    ## single index
    inds <- 3
    mat <- vr$getIndexRangeMatrix(inds)
    expect_identical(mat, indexRange(unique(full_result[ , inds, drop = FALSE])))
})

## check for addtional tests
## TODO: test varRange2expr

    xVar <- varRangeClass$new('x[2:10,1:3]')
 xVar <- varRangeClass$new('x[2:10,c(2,4)]')
xVar <- varRangeClass$new(list(indexRange(quote(2:10)),
                               indexRange(matrix(c(2,4)))), varName = 'x')
varRange2expr(xVar)
xVar <- varRangeClass$new(list(indexRange(quote(2:10)),
                               indexRange(matrix(c(2,4,7,9,10)))), varName = 'x')
varRange2expr(xVar)

xVar <- varRangeClass$new(list(indexRange(matrix(1:4,ncol=2))), varName = 'x')
varRange2expr(xVar)

xVar <- varRangeClass$new(list(indexRange(matrix(1:4,ncol=2))), varName = 'x')
varRange2expr(xVar)

xVar <- varRangeClass$new(list(indexRange(matrix(1:4,ncol=2)),
                               indexRange(quote(1:3))), indexOrders = list(c(1,3),2), varName = 'x')
varRange2expr(xVar)
