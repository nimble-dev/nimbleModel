context("indexRule_block")

test_that("indexRule_block works",
{    
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    setupResults <- indexRule_block_setup(list(quote(i + 1)),
                                          list(quote(i + 3)),
                                          context_i)
    
    expect_equal(indexRule_block_apply_single(13,
                                              setupResults),
                 matrix(11))
    
    expect_identical(indexRule_block_apply(indexRange(13),
                                       setupResults),
                     indexRange(matrix(11)))
    
    
    expect_identical(
        indexRule_block_apply_block(indexRange(quote(4:5)),
                                    setupResults)
        ,
        indexRange(quote(2:3))
    )

    expect_identical(
        indexRule_block_apply(indexRange(quote(4:5)),
                              setupResults)
       ,
        indexRange(quote(2:3))
    )
}
)
