library(nimbleModel)

singleContext1 <-
    modelSingleContext(forCode = quote(for(i in 1:10){}))

context_i <- modelContextClass$new(list(singleContext1))

debug(indexRule_block_setup)
setupResults <- indexRule_block_setup(list(quote(i + 1)),
                                      list(quote(i + 3)),
                                      context_i)
indexRule_block_apply_single(13,
                             setupResults)

indexRule_block_apply_block(indexRange(quote(4:5)),
                            setupResults)
