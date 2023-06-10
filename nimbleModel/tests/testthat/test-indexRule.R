test_that("getOffset", {

    expect_identical(nimbleModel:::getOffset(quote(i), "i"), list(offset = 0))

    expect_identical(nimbleModel:::getOffset(quote(i+1), "i"), list(offset = 1))
    expect_identical(nimbleModel:::getOffset(quote(i+3), "i"), list(offset = 3))
    expect_identical(nimbleModel:::getOffset(quote(i-1), "i"), list(offset = -1))
    expect_identical(nimbleModel:::getOffset(quote(i-3), "i"), list(offset = -3))

    expect_identical(nimbleModel:::getOffset(quote(3+i), "i"), list(offset = 3))
    expect_identical(nimbleModel:::getOffset(quote(3-i), "i"), NULL)
    expect_identical(nimbleModel:::getOffset(quote(3+i+7), "i"), NULL)
   
    expect_identical(nimbleModel:::getOffset(quote(i*3), "i"), NULL)
    expect_identical(nimbleModel:::getOffset(quote(3*i), "i"), NULL)
    expect_identical(nimbleModel:::getOffset(quote(exp(i)), "i"), NULL)

    env <- new.env(); env$j <- -2
    expect_identical(nimbleModel:::getOffset(quote(i+j), "i", env),
                     list(offset=-2))
    
    expect_identical(nimbleModel:::getOffset(quote(i+3+7), "i"), NULL)
    expect_identical(nimbleModel:::getOffset(quote(i+(3+7)), "i"), list(offset = 10))
    expect_identical(nimbleModel:::getOffset(quote(i+(3-7)), "i"), list(offset = -4))

})
