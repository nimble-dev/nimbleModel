test_that("data rules work", {

    ## 1-d
    y <- rnorm(5)
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)
    expect_identical(length(dataRules), 1L)
    expect_identical(length(dataRules[[1]]$rule$indexRules), 1L)
    expect_identical(dataRules[[1]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 1, fromMax = 5))
    expect_identical(nondataRules, NULL)
    expect_equal(
        dataRules[[1]]$apply('y[2:40]'),
        varRangeClass$new(list(newIndexRange(quote(2:5))), varName = 'y')
    )

    y <- rnorm(50)
    y[1] <- NA
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)

    expect_identical(length(dataRules), 1L)
    expect_identical(length(dataRules[[1]]$rule$indexRules), 1L)
    expect_identical(dataRules[[1]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 2, fromMax = 50))

    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(2:50))), varName = 'y')
    )
    expect_equal(
        dataRules[[1]]$apply('y[1:4]'),
        varRangeClass$new(list(newIndexRange(quote(2:4))), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(1)), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y[1:4]'),
        varRangeClass$new(list(newIndexRange(1)), varName = 'y')
    )

    y[1] <- 3.5
    y[3] <- NA
    y[5] <- NA
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)

    expect_identical(length(dataRules), 3L)
    expect_identical(length(nondataRules), 1L)
    expect_identical(dataRules[[1]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 1, fromMax = 2))
    expect_identical(dataRules[[2]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 4, fromMax = 4))
    expect_identical(dataRules[[3]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 6, fromMax = 50))

    expect_equal(
        dataRules[[3]]$apply('y[4:10]'),
        varRangeClass$new(list(newIndexRange(quote(6:10))), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y[4:10]'),
        varRangeClass$new(list(newIndexRange(5)), varName = 'y')
    )
    

    y[5] <- 3.5
    y[4] <- NA
    dataRules <- newDataRules(y, 'y')
    
    expect_identical(length(dataRules), 2L)
    expect_identical(length(nondataRules), 1L)
    expect_identical(dataRules[[1]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 1, fromMax = 2))
    expect_identical(dataRules[[2]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 5, fromMax = 50))

    y <- rnorm(50)
    y[1:3] <- NA
    dataRules <- newDataRules(y, 'y')
    
    expect_identical(length(dataRules), 1L)
    expect_identical(length(nondataRules), 1L)
    expect_identical(dataRules[[1]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 4, fromMax = 50))

    y <- rep(as.numeric(NA), 50)
    y[2] <- 3.5
    y[50] <- 3.5
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)
    
    expect_identical(length(dataRules), 1L)
    expect_identical(length(nondataRules), 2L)
    expect_identical(nondataRules[[1]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 1, fromMax = 1))
    expect_identical(nondataRules[[2]]$rule$indexRules[[1]]$setupResults,
                     list(offset = 0, fromMin = 3, fromMax = 49))
    
    expect_equal(
        dataRules[[1]]$apply('y[1:10]'),
        varRangeClass$new(list(newIndexRange(2)), varName = 'y')
    )
    expect_equal(
        nondataRules[[2]]$apply('y[2:10]'),
        varRangeClass$new(list(newIndexRange(quote(3:10))), varName = 'y')
    )

    ## 2-d
    y <- matrix(rnorm(12), 4, 3)
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)
    expect_null(nondataRules)
    
    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(1:4)), newIndexRange(quote(1:3))), varName = 'y')
    )
    expect_equal(
        dataRules[[1]]$apply('y[1:2,1:3]'),
        varRangeClass$new(list(newIndexRange(quote(1:2)), newIndexRange(quote(1:3))), varName = 'y')
    )

    y[2,1] <- NA
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)

    mat <- expand.grid(1:3,1:4)[,2:1]
    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-4,])), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[4,])), varName = 'y')
    )

    y[3,2] <- NA
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)

    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-c(4,8),])), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[c(4,8),])), varName = 'y')
    )

    ## TODO: we are not smart enough to convert result from matrix indexRange
    ## back to sequence indexRanges.
    y <- matrix(rnorm(12), 4, 3)
    y[,3] <- NA
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)

    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-c(3,6,9,12),])), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[c(3,6,9,12),])), varName = 'y')
    )

    ## 3-d
    y <- array(rnorm(24), c(4,3,2))
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)
    expect_null(nondataRules)
    
    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(1:4)), newIndexRange(quote(1:3)), newIndexRange(quote(1:2))), varName = 'y')
    )
    expect_equal(
        dataRules[[1]]$apply('y[1:2,1:3,1:2]'),
        varRangeClass$new(list(newIndexRange(quote(1:2)), newIndexRange(quote(1:3)), newIndexRange(quote(1:2))), varName = 'y')
    )

    y[2,3,1] <- NA
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)
    
    mat <- expand.grid(1:2,1:3,1:4)[,3:1]
    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-11,])), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[11,])), varName = 'y')
    )
    
    y[4,3,1] <- NA
    dataRules <- newDataRules(y, 'y')
    nondataRules <- newDataRules(y, 'y', nondata = TRUE)
    
    mat <- expand.grid(1:2,1:3,1:4)[,3:1]
    expect_equal(
        dataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-c(11,23),])), varName = 'y')
    )
    expect_equal(
        nondataRules[[1]]$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[c(11,23),])), varName = 'y')
    )

})

