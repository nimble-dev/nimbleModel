test_that("data rules work", {
    ## 1-d
    y <- rnorm(5)
    
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)
    expect_null(nondataRule$rule)
    
    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y')
    )
    expect_equal(
        dataRule$apply('y[1:4]'),
        varRangeClass$new(list(newIndexRange(quote(1:4))), varName = 'y')
    )
            
    y[1] <- NA
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)

    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(2:5))), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(1)), varName = 'y')
    )
    
    y[4] <- NA
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)

    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(matrix(c(2,3,5)))), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(matrix(c(1,4)))), varName = 'y')
    )
    
    y <- rep(NA, 5)
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)

    expect_null(dataRule$rule)
    
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(1:5))), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y[1:4]'),
        varRangeClass$new(list(newIndexRange(quote(1:4))), varName = 'y')
    )

    ## 2-d
    y <- matrix(rnorm(12), 4, 3)
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)
    expect_null(nondataRule$rule)
    
    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(1:4)), newIndexRange(quote(1:3))), varName = 'y')
    )
    expect_equal(
        dataRule$apply('y[1:2,1:3]'),
        varRangeClass$new(list(newIndexRange(quote(1:2)), newIndexRange(quote(1:3))), varName = 'y')
    )

    y[2,1] <- NA
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)

    mat <- expand.grid(1:3,1:4)[,2:1]
    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-4,])), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[4,])), varName = 'y')
    )

    y[3,2] <- NA
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)

    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-c(4,8),])), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[c(4,8),])), varName = 'y')
    )

    ## TODO: we are not smart enough to convert result from matrix indexRange
    ## back to sequence indexRanges.
    y <- matrix(rnorm(12), 4, 3)
    y[,3] <- NA
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)

    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-c(3,6,9,12),])), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[c(3,6,9,12),])), varName = 'y')
    )

    ## 3-d
    y <- array(rnorm(24), c(4,3,2))
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)
    expect_null(nondataRule$rule)
    
    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(quote(1:4)), newIndexRange(quote(1:3)), newIndexRange(quote(1:2))), varName = 'y')
    )
    expect_equal(
        dataRule$apply('y[1:2,1:3,1:2]'),
        varRangeClass$new(list(newIndexRange(quote(1:2)), newIndexRange(quote(1:3)), newIndexRange(quote(1:2))), varName = 'y')
    )

    y[2,3,1] <- NA
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)
    
    mat <- expand.grid(1:2,1:3,1:4)[,3:1]
    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-11,])), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[11,])), varName = 'y')
    )
    
    y[4,3,1] <- NA
    dataRule <- dataRuleClass$new(y, 'y')
    nondataRule <- dataRuleClass$new(y, 'y', nondataRule = TRUE)
    
    mat <- expand.grid(1:2,1:3,1:4)[,3:1]
    expect_equal(
        dataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[-c(11,23),])), varName = 'y')
    )
    expect_equal(
        nondataRule$apply('y'),
        varRangeClass$new(list(newIndexRange(mat[c(11,23),])), varName = 'y')
    )

})

