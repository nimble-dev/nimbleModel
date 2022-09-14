context("indexRule_block")

test_that("indexRule_block works",
{    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    setupResults <- indexRule_block_setup(list(quote(i + 1)),
                                          list(quote(i + 3)),
                                          context_i)

    irEmpty <- nimbleModel:::indexRange_empty()

    ## Direct use of the methods

    expect_equal(indexRule_block_apply_single(13,
                                              setupResults),
                 indexRange(11))
    
    expect_identical(indexRule_block_apply(indexRange(13),
                                       setupResults),
                     indexRange(11))
    
    
    expect_identical(
        indexRule_block_apply(indexRange(quote(4:5)),
                                    setupResults),
        indexRange(quote(2:3))
    )

    expect_identical(
        indexRule_block_apply(indexRange(matrix(4)),
                              setupResults),
        indexRange(matrix(2))
    )

    expect_identical(
        indexRule_block_apply(indexRange(matrix(c(4,6,8), nrow = 3)),
                              setupResults),
        indexRange(matrix(c(2,4,6), nrow = 3))
    )

    ## with duplicates
    expect_identical(
        indexRule_block_apply(indexRange(matrix(c(4,4,8), nrow = 3)),
                              setupResults),
        indexRange(matrix(c(2,2,6), nrow = 3))
    )

    expect_identical(
        indexRule_block_apply(indexRange(quote(4:5)),
                              setupResults),
        indexRange(quote(2:3))
    )

    ## Partially or fully out of range
    expect_identical(
        indexRule_block_apply(indexRange(quote(12:14)),
                              setupResults),
        indexRange(quote(10:11))
    )

    expect_identical(
        indexRule_block_apply(indexRange(quote(2:5)),
                              setupResults),
        indexRange(quote(2:3))
    )

    ## Not sure if we want this to simplify to a scalar.
    expect_identical(
        indexRule_block_apply(indexRange(quote(2:4)),
                              setupResults),
        indexRange(2)
    )

    expect_identical(
        indexRule_block_apply(indexRange(quote(15:16)),
                              setupResults),
        irEmpty
    )

    ## These produce NAs. They need to be kept at this stage
    ## so that if have multiple rules applied to a multi-column
    ## indexRange matrix, we can piece the results of the rules
    ## together element by element.
    expect_identical(
        indexRule_block_apply(indexRange(matrix(c(10,12,14), nrow = 3)),
                              setupResults),
        indexRange(matrix(c(8,10, NA), nrow = 3))
    )

    expect_identical(
        indexRule_block_apply(indexRange(matrix(c(14,16,18), nrow = 3)),
                              setupResults),
        indexRange(matrix(rep(as.numeric(NA), 3), nrow = 3))
    )

    ## use of API
    rule <- nimbleModel:::indexRuleClass_block$new(list(quote(i+1)), list(quote(i+3)), context_i)
    expect_identical(
        rule$apply(indexRange(quote(4:5))),
        indexRange(quote(2:3)))


    ## offset context indexing
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 3:5){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    setupResults <- indexRule_block_setup(list(quote(i + 1)),
                                          list(quote(i + 3)),
                                          context_i)

    expect_identical(
        indexRule_block_apply(indexRange(quote(7:10)),
                                    setupResults),
        indexRange(quote(5:6))
    )

})

