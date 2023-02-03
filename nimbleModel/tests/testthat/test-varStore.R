test_that('varStoreClass initialization with single value', {

    x <- varStoreClass$new('foo')
    expect_identical(x$type, 'character', info = 'single element character input')

    x <- varStoreClass$new(3.3)
    expect_identical(x$dim, 1, info = 'dimension of single input is 1')
    expect_identical(length(x$value), 1L, info = 'length of single input is 1')
    expect_true(x$allEqual, info = 'single input is allEqual')

    x <- varStoreClass$new(3.3, dim = 0)
    expect_identical(x$dim, 0, info = 'dimension of single input, dim 0, is 1')
    expect_identical(length(x$value), 1L, info = 'length of single input, dim 0, is 1')
    expect_true(x$allEqual, info = 'single input is allEqual')

    x <- varStoreClass$new(3.3, dim = 3)
    expect_identical(x$dim, 3, info = 'dimension of vector')
    expect_identical(length(x$value), 1L, info = 'length when allEqual is 1 for vector')
    expect_true(x$allEqual, info = 'single input as vector is allEqual')
    
    x <- varStoreClass$new(3.3, dim = c(2, 3))
    expect_identical(x$dim, c(2,3), info = 'dimension of matrix')
    expect_identical(length(x$value), 1L, info = 'length when allEqual is 1 for matrix')
    expect_true(x$allEqual, info = 'single input as matrix is allEqual')

    x <- varStoreClass$new(3.3, dim = c(2, 3, 4))
    expect_identical(x$dim, c(2,3,4), info = 'dimension of array')
    expect_identical(length(x$value), 1L, info = 'length when allEqual is 1 for array')
    expect_true(x$allEqual, info = 'single input as array is allEqual')

})

test_that('varStoreClass initialization with duplicated values', {
    x <- varStoreClass$new(rep(3.3, 3))
    expect_identical(x$dim, 3L, info = 'dimension of vector input')
    expect_identical(length(x$value), 1L, info = 'length when allEqual is 1 for vector')
    expect_true(x$allEqual, info = 'duplicated input as vector is allEqual')

    x <- varStoreClass$new(matrix(3.3, 2, 3), dim = c(2,3))
    expect_identical(x$dim, c(2L,3L), info = 'dimension of matrix input')
    expect_identical(length(x$value), 1L, info = 'length when allEqual is 1 for matrix')
    expect_true(x$allEqual, info = 'duplicated input as matrix is allEqual')

    x <- varStoreClass$new(array(3.3, c(2,3,4)), dim = c(2,3,4))
    expect_identical(x$dim, c(2L,3L,4L), info = 'dimension of array input')
    expect_identical(length(x$value), 1L, info = 'length when allEqual is 1 for array')
    expect_true(x$allEqual, info = 'duplicated input as array is allEqual')
})

test_that('varStoreClass initialization with heterogeneous values', {
    x <- varStoreClass$new(c(1,2,3))
    expect_identical(x$dim, 3L, info = 'dimension of vector input')
    expect_identical(length(x$value), 3L, info = 'length when not allEqual for vector')
    expect_false(x$allEqual, info = 'heterogeneous input as vector is not allEqual')

    x <- varStoreClass$new(matrix(rnorm(6), 2, 3))
    expect_identical(x$dim, c(2L, 3L), info = 'dimension of matrix input')
    expect_identical(dim(x$value), c(2L, 3L), info = 'dimension of value of matrix input')
    expect_false(x$allEqual, info = 'heterogeneous input as matrix is not allEqual')

    x <- varStoreClass$new(array(rnorm(24), c(2,3,4)))
    expect_identical(x$dim, c(2L, 3L, 4L), info = 'dimension of array input')
    expect_identical(dim(x$value), c(2L, 3L, 4L), info = 'dimension of value of array input')
    expect_false(x$allEqual, info = 'heterogeneous input as array is not allEqual')
})

test_that('varStoreClass initialization with inconsistent dimension', {
    expect_warning(x <- varStoreClass$new(rep(3.3, 3), dim = 4),
                   'dimension of input not consistent')
    expect_warning(x <- varStoreClass$new(matrix(3.3, 2, 3), dim = 4),
                   'dimension of input not consistent')
    ## we do not coerce dimensions
    expect_warning(x <- varStoreClass$new(1:6, dim = c(2,3)),
                   'dimension of input not consistent')
})

    
test_that('varStoreClass scalar subsetting', {
    x <- varStoreClass$new(3.3, dim = 0)
    expect_error(x[1], 'subscript out of bounds')
    expect_error(x[], 'subscript out of bounds')
})


test_that('varStoreClass vector subsetting', {
    x <- varStoreClass$new(3.3, dim = 3)
    expect_identical(x[2], 3.3)
    expect_identical(x[1:2], 3.3)
    expect_identical(x[c(1,3)], 3.3)
    expect_identical(x[1:2, expand = TRUE], rep(3.3, 2))
    expect_error(x[4], 'subscript out of bounds')
    expect_error(x[4,7], 'incorrect number of dimensions')

    x <- varStoreClass$new(1:3)
    expect_identical(x[2], 2L)
    expect_identical(x[1:2], 1:2)
    expect_identical(x[1:2, expand = TRUE], 1:2)
    expect_identical(x[c(1,3)], c(1L, 3L))
})

test_that('varStoreClass matrix subsetting', {
    x <- varStoreClass$new(3.3, dim = c(2,3))
    expect_identical(x[1,2], 3.3)
    expect_identical(x[2], 3.3)
    expect_identical(x[1, 2:3], 3.3)
    expect_identical(x[2, c(1,3)], 3.3)
    expect_identical(x[2, c(1,3), expand = TRUE], matrix(3.3, 1, 2))
    expect_error(x[4,1], 'subscript out of bounds')
    expect_error(x[4,7,3], 'incorrect number of dimensions')

    x <- varStoreClass$new(matrix(1:6, 2, 3), dim = c(2,3))
    expect_identical(x[1,2], 3L)
    expect_identical(x[2], 2L)
    expect_identical(x[1, 2:3], c(3L, 5L))
    expect_identical(x[2, c(1,3)], c(2L, 6L))
    expect_identical(x[2, c(1,3), expand = TRUE], c(2L, 6L))
    expect_identical(x[1:2, 1:2], matrix(1:4, 2, 2))
    expect_identical(x[1, ], c(1L, 3L, 5L))
})

test_that('varStoreClass array subsetting', {
    x <- varStoreClass$new(3.3, dim = c(2,3, 4))
    expect_identical(x[1,2,3], 3.3)
    expect_identical(x[2], 3.3)
    expect_identical(x[1, 2:3, 4], 3.3)
    expect_identical(x[2, c(1,3), 1:2], 3.3)
    expect_identical(x[2, c(1,3), 1:2, expand = TRUE], array(3.3, c(1,2,2)))
    expect_error(x[4,1,2], 'subscript out of bounds')
    expect_error(x[4,7], 'incorrect number of dimensions')
    expect_error(x[4,7,2,2], 'incorrect number of dimensions')

    arr <- array(1:24, c(2, 3, 4))
    x <- varStoreClass$new(array(1:24, c(2, 3, 4)), dim = c(2,3,4))
    expect_identical(x[1,2,3], 15L)
    expect_identical(x[2], 2L)
    expect_identical(x[1, 2:3, 2], c(9L, 11L))
    expect_identical(x[2, c(1,3), 4], c(20L, 24L))
    expect_identical(x[2, c(1,3), 4, expand = TRUE], c(20L, 24L))
    expect_identical(x[1:2, 1:2, 1], matrix(1:4, 2, 2))
    expect_identical(x[ , 2:3, ], arr[, 2:3, ])
})

test_that('varStoreClass vector subset assignment', {
    x <- varStoreClass$new(3.3, dim = 3)
    x[2] <- 3.3
    expect_true(x$allEqual)
    expect_identical(length(x$value), 1L)

    x[2] <- 2
    expect_false(x$allEqual)
    expect_identical(length(x$value), 3L)
    expect_identical(x$dim, 3)

    expect_error(x[2] <- 'foo', 'input type is not the same as the stored type')
    expect_error(x[4] <- 7, 'subscript out of bounds')
    expect_error(x[4,7] <- 3, 'incorrect number of dimensions')

    x[c(1,3)] <- c(2, 7)
    expect_identical(x$value[c(1,3)], c(2, 7))

    x[3] <- 2
    expect_true(x$allEqual)
    expect_identical(x$value, 2)

    x <- varStoreClass$new(3.3, dim = 4)
    x[1:2] <- 1
    expect_identical(x$value[1:2], rep(1, 2))
    expect_warning(x[1:3] <- c(1,2), 'not a multiple of replacement length')
    expect_warning(x[1:2] <- c(1,2,3), 'not a multiple of replacement length')
})

test_that('varStoreClass matrix subset assignment', {
    x <- varStoreClass$new(matrix(rnorm(6), c(2,3)))
    x[2] <- 2
    expect_identical(x$value[2], 2)
    x[1:2, 3] <- 4
    expect_identical(x$value[1:2, 3], c(4,4))
    x[1:2, 3] <- c(4,5)
    expect_identical(x$value[1:2, 3], c(4,5))
    x[2, ] <- c(1,2,3)
    expect_identical(x$value[2, 1:3], c(1,2,3))
    
    expect_error(x[2] <- 'foo', 'input type is not the same as the stored type')
    expect_error(x[7] <- 7, 'subscript out of bounds')
    expect_error(x[4,7] <- 3, 'subscript out of bounds')
    expect_error(x[4,7,1] <- 3, 'incorrect number of dimensions')

    expect_error(x[ , 3] <- c(1,2,3), 'not a multiple of replacement')
    expect_error(x[ , 3] <- c(1,2,3,4), 'not a multiple of replacement')
    expect_error(x[1, ] <- c(1,2), 'not a multiple of replacement')
})
