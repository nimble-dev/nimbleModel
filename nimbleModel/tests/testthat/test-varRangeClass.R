library(R6)
library(testthat)

## source('model_varRangeClass.R')

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
    expect_identical(eval(input),
                     indexRange2expr(indexRange(input)))

    input <- c(2, 4, 6)
    expect_identical(input,
                     indexRange2expr(indexRange(input)))

    input <- quote(matrix(c(2, 4, 5, 8, 6, 2), ncol = 2))
    expect_identical(eval(input),
                     indexRange2expr(indexRange(input)))

    input <- matrix(c(2, 4, 5, 8, 6, 2), ncol = 2)
    expect_identical(input,
                     indexRange2expr(indexRange(input)))
 
}
)

test_that('varRangeClass', {

    ## 1D:
    y <- 101:110
    
    xVar <- varRangeClass$new('x')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y)

    xVar <- varRangeClass$new('x[3]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3])

    xVar <- varRangeClass$new('x[2:10]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[2:10])

    xVar <- varRangeClass$new('x[]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[])

    ## 2D
    y <- matrix(101:200, nrow = 10)    

    xVar <- varRangeClass$new('x[3, 4]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3, 4])

    xVar <- varRangeClass$new('x[3, 2:4]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3, 2:4])

    xVar <- varRangeClass$new('x[3:5, 6]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[3:5, 6])

    xVar <- varRangeClass$new('x[2:10, 3:5]')
    ans <- evalIndexRange(y, xVar)
    expect_identical(ans, y[2:10, 3:5])

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
    expect_equal(
        varRange2expr( VR ),
        varRange2expr( VRnew )
    )
    
}
)
