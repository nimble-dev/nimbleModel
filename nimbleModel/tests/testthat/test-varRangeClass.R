library(R6)
library(testthat)

## source('model_varRangeClass.R')

test_that("matrix_expand_grid",
{
    expect_equal(
        matrix_expand_grid(list(matrix(1:3, ncol = 1))),
        matrix(1:3, ncol = 1)
    )

    expect_equal(
        matrix_expand_grid(matrix(1:4, ncol = 1),
                           matrix(c(11:13, 21:23),
                                  ncol =2)
                           )
       ,
        matrix(
            c(rep(1:4, 3),
              rep(11:13, each = 4),
              rep(21:23, each = 4)
              ),
            ncol = 3
        )
    )

    expect_equal(
        matrix_expand_grid(matrix(c(105:109, 115:119),
                                  ncol =2),
                           matrix(c(11:13, 21:23),
                                  ncol =2)
                           )
       ,
        matrix(
            c(rep(105:109, 3),
              rep(115:119, 3),
              rep(11:13, each = 5),
              rep(21:23, each = 5)
              ),
            ncol = 4
        )
    )

}
)

test_that('indexRange conversions between list and expr', {
    input <- quote(x[]) ## the blank, which is wierd if extracted separately
    expect_identical(input[[3]],
                     indexRange2expr(indexRange(input[[3]])))
    
    input <- quote(2)
    expect_identical(input,
                     indexRange2expr(indexRange(input)))

    
    input <- quote(1:3)
    expect_identical(input,
                     indexRange2expr(indexRange(input)))

    input <- quote(c(2, 4, 6))
    expect_identical(matrix(eval(input)),
                     indexRange2expr(indexRange(input)))

    input <- c(2, 4, 6)
    expect_identical(matrix(input),
                     indexRange2expr(indexRange(input)))

    input <- quote(matrix(c(2, 4, 5, 8, 6, 2), ncol = 2))
    expect_identical(eval(input),
                     indexRange2expr(indexRange(input)))

    input <- matrix(c(2, 4, 5, 8, 6, 2), ncol = 2)
    expect_identical(input,
                     indexRange2expr(indexRange(input)))
 
}
)

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
        matrix(c(rep(3, 3), 2:4), ncol = 2)
    )

    expect_equal(
        varRange_getIndexRangeMatrix(xVar, c(1))
        ,
        matrix(3)
    )

    expect_equal(
        varRange_getIndexRangeMatrix(xVar, c(2))
        ,
        matrix(2:4, ncol = 1)
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
        structure(
            as.matrix(
                expand.grid(2:10, 3:5)
            ),
            dimnames = NULL)
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

test_that("varRange initialized with matrix indexRange(s)",
{
    
}
)

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
    VRnew <-  varRangeClass$new(quote(x[2:4, 5])) ## "x" is arbitrary here
    VR$setIndexRanges( VRnew$indexRanges )
    expect_identical(
        varRange2expr( VR ),
        varRange2expr( VRnew )
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
    
}
)
