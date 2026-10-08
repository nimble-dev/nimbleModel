test_that("indexRange initialization and conversions", {

    expect_error(newIndexRange(-3),
                 "single index must be a positive, integer-valued")
    expect_error(newIndexRange(3.7),
                 "single index must be a positive, integer-valued")
    expect_error(newIndexRange(0),
                 "single index must be a positive, integer-valued")
    
    
    scalarIR <- newIndexRange(5)
    seqIR <- newIndexRange(quote(3:5))
    matIR1d <- newIndexRange(matrix(c(2,3,5)))
    matIR2d <- newIndexRange(matrix(c(2,3,1,5), ncol = 2))

    expect_identical(scalarIR$numElements, 1)
    expect_identical(seqIR$numElements, 3)
    expect_identical(matIR1d$numElements, 3)
    expect_identical(matIR2d$numElements, 2)
    expect_identical(matIR1d$numColumns, 1)
    expect_identical(matIR2d$numColumns, 2)

    expect_identical(scalarIR$value, 5)
    expect_identical(c(seqIR$start, seqIR$end), c(3,5))
    expect_identical(matIR1d$values, matrix(c(2,3,5)))
    expect_identical(matIR2d$values, matrix(c(2,3,1,5), ncol = 2))

    result <- scalarIR$toMatrix()
    expect_equal(result, newIndexRange(matrix(5)))

    result <- seqIR$toMatrix()
    expect_equal(result, newIndexRange(matrix(3:5)))
    result <- seqIR$toMatrixList()
    expect_equal(result, indexRangeMatrixListClass$new(lapply(3:5, as.matrix)))
    
    expect_identical(matIR1d, matIR1d$toSequence())
    expect_identical(matIR2d, matIR2d$toSequence())

    matIR1d <- newIndexRange(matrix(c(2,3,4)))
    expect_equal(matIR1d$toSequence(), newIndexRange(quote(2:4)))
    
})


test_that("matrixExpandGrid", {
    expect_identical(
        nimbleModel:::matrixExpandGrid(list(
                          matrix(1:3, ncol = 1))),
        matrix(1:3, ncol = 1)
    )

    expect_identical(
        nimbleModel:::matrixExpandGrid(list(
                          matrix(1:4, ncol = 1),
                          matrix(c(11:13, 21:23), ncol = 2)
                          )),
        matrix(
            c(rep(1:4, 3),
              rep(11:13, each = 4),
              rep(21:23, each = 4)
              ),
            ncol = 3
        )
    )

    expect_identical(
        nimbleModel:::matrixExpandGrid(list(
                          matrix(c(105:109, 115:119),
                                            ncol = 2),
                          matrix(c(11:13, 21:23),
                                            ncol = 2)
                          )),
        matrix(
            c(rep(105:109, 3),
              rep(115:119, 3),
              rep(11:13, each = 5),
              rep(21:23, each = 5)
              ),
            ncol = 4
        )
    )
})

test_that("crossIndexRanges", {
    result <- nimbleModel:::crossIndexRanges(
                          list(
                              newIndexRange(quote(1:3)),
                              newIndexRange(c(1,3)),
                              newIndexRange(quote(2:3)),
                              newIndexRange(c(2,4))
                          ))
    expected <- newIndexRange(expand.grid(1:3, c(1,3), 2:3, c(2,4)))
    expect_equal(result, expected)
    expect_identical(result$values, expected$values)
    
})

test_that("indexRangeMatrixListsToMatrix", {
    matList1 <- indexRangeMatrixListClass$new(list(
                                             matrix(c(7,1), ncol = 2),
                                             matrix(c(10,1), ncol = 2),
                                             matrix(c(8,2), ncol = 2),
                                             matrix(c(11,2), ncol = 2)))
    matList2 <- indexRangeMatrixListClass$new(lapply(c(1,1,3,3), as.matrix))
    
    expect_error(
        nimbleModel:::indexRangeMatrixListsToMatrix(list(
                          matList1,
                          indexRangeMatrixListClass$new(lapply(c(1,1,3), as.matrix))
                      )),
        "Inconsistent number of elements"
    )

    result <- nimbleModel:::indexRangeMatrixListsToMatrix(list(
                                matList1,
                                matList2
                            ))
    expected <- newIndexRange(matrix(c(7,1,1,
                                    10,1,1,
                                    8,2,3,
                                    11,2,3), byrow = TRUE, ncol =3))
    expect_equal(result, expected)
    expect_identical(result$values, expected$values)

    result <- nimbleModel:::indexRangeMatrixListsToMatrix(list(
                                matList1,
                                newIndexRange(quote(1:4))
                            ))
    expected <- newIndexRange(matrix(c(7,1,1,
                                    10,1,2,
                                    8,2,3,
                                    11,2,4), byrow = TRUE, ncol =3))
    expect_equal(result, expected)
    expect_identical(result$values, expected$values)

    result <- nimbleModel:::indexRangeMatrixListsToMatrix(list(
                                indexRangeMatrixListClass$new(lapply(c(2,1,NA), as.matrix)),
                                indexRangeMatrixListClass$new(list(
                                                              matrix(c(3,4)),
                                                              matrix(c(2,3,4)),
                                                              matrix(c(3,4))
                                                          ))
                            ))
    expected <- newIndexRange(matrix(c(2,3,
                                    2,4,
                                    1,2,
                                    1,3,
                                    1,4,
                                    NA,3,
                                    NA,4), byrow = TRUE, ncol = 2))
    expect_equal(result, expected)
    expect_identical(result$values, expected$values)
})

## TODO: this would need to be converted to testing `toExpr`, but
## I doubt this is needed given how simple that is.

if(FALSE) {
test_that('indexRange conversions between list and expr', {
    input <- quote(x[]) ## the blank, which is weird if extracted separately
    expect_identical(input[[3]],
                     indexRange2expr(newIndexRange(input[[3]])))
    
    input <- quote(2)
    expect_identical(input,
                     indexRange2expr(newIndexRange(input)))

    
    input <- quote(1:3)
    expect_identical(input,
                     indexRange2expr(newIndexRange(input)))

    input <- quote(c(2, 4, 6))
    expect_identical(matrix(eval(input)),
                     indexRange2expr(newIndexRange(input)))

    input <- c(2, 4, 6)
    expect_identical(matrix(input),
                     indexRange2expr(newIndexRange(input)))

    input <- quote(matrix(c(2, 4, 5, 8, 6, 2), ncol = 2))
    expect_identical(eval(input),
                     indexRange2expr(newIndexRange(input)))

    input <- matrix(c(2, 4, 5, 8, 6, 2), ncol = 2)
    expect_identical(input,
                     indexRange2expr(newIndexRange(input)))
 
})
}

