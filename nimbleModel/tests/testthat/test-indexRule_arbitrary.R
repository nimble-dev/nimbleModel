context("indexRule_arbitrary tests")

## This function takes the inputs for an arbitrary index rule
## and compares the rule behavior to a brute force evaluation
## done by for-loop execution.
test_arbitraryIndexRule <- function(LHS,
                                    RHS,
                                    context,
                                    constants = list(),
                                    debug = FALSE) {
    if(!debug) debug <- Inf
    if(is.character(LHS))
        LHS <- parse(text = LHS, keep.source = FALSE)[[1]]
    if(is.character(RHS))
        RHS <- parse(text = RHS, keep.source = FALSE)[[1]]

    if(LHS[[1]] != "[") stop("LHS must have indices")
    if(RHS[[1]] != "[") stop("RHS must have indices")

    toIndexExprList <-
        structure(as.list(LHS[-c(1, 2)]),
                  names = paste0("t", seq_len(length(LHS)-2)))

    fromIndexExprList <-
        structure(as.list(RHS[-c(1, 2)]),
                  names = paste0("f", seq_len(length(RHS)-2)))

    constantsEnv <- list2env(constants)
    
    setupRules <- indexRule_arbitrary_setup(
        toIndexExprList = toIndexExprList,
        fromIndexExprList = fromIndexExprList,
        context = context,
        constantsEnv = constantsEnv
    )

    makeBruteForceCalculator <- function(LHS,
                                         RHS,
                                         context,
                                         constantsEnv,
                                         fromInfo,
                                         toInfo) {
        LHSsizes <- unlist(lapply(toInfo, `[[`, 'size')) +
            unlist(lapply(toInfo, `[[`, 'offset'))
        LHSnDim <- length(LHSsizes)
        initLHS <-
            if(LHSnDim == 1)
                rep(FALSE, LHSsizes)
            else if(LHSnDim == 2)
                matrix(FALSE, nrow = LHSsizes[1], ncol = LHSsizes[2])
            else
                array(FALSE, dim = LHSsizes)
        LHSname <- as.character(LHS[[2]])

        RHSsizes <- unlist(lapply(fromInfo, `[[`, 'size')) +
            unlist(lapply(fromInfo, `[[`, 'offset'))
        RHSnDim <- length(RHSsizes)
        initRHS <-
            if(RHSnDim == 1)
                rep(FALSE, RHSsizes)
            else if(RHSnDim == 2)
                matrix(FALSE, nrow = RHSsizes[1], ncol = RHSsizes[2])
            else
                array(FALSE, dim = RHSsizes)
        RHSname <- as.character(RHS[[2]])

        innerLoopCode <-
            substitute(LHS <- any(RHS),
                       list(LHS = LHS,
                            RHS = RHS))
        codeInForLoop <-
            contextClass_embedCodeInForLoop(
                context$singleContexts,
                innerLoopCode)

        force(constantsEnv)
        
        bruteForceCalculator <-
            function(LHSindex) {
                assign(LHSname, initLHS, constantsEnv)
                assign(RHSname, initRHS, constantsEnv)
                constantsEnv[[RHSname]] <-
                    `[<-`(constantsEnv[[RHSname]],
                          matrix(LHSindex, nrow = 1),
                          TRUE)
                eval(codeInForLoop, envir = constantsEnv)
                result <- get(LHSname, envir = constantsEnv)
                rm(list = c(LHSname, RHSname), envir = constantsEnv)
                ans <- which(result, arr.ind = TRUE)
                if(!is.matrix(ans)) ans <- matrix(ans)
                ans
            }

        bruteForceCalculator
    }
    bruteForceCalculator <-
        makeBruteForceCalculator(LHS,
                                 RHS,
                                 context,
                                 constantsEnv,
                                 setupRules$fromInfo,
                                 setupRules$toInfo)

    matrix_sort <- function(m) {
        o <- do.call("order", as.data.frame(m))
        m[o,, drop = FALSE]
    }
    
    fromInfo <- setupRules$fromInfo
    for(i in seq_len(setupRules$from_flatMax)) {
        if(i >= debug) browser()
        rawIndices <- setupRules$from2indicesFunctions$flatIndex2rawIndex(i)
        rulesAnswer <- indexRule_arbitrary_apply_single(rawIndices,
                                  setupRules)
        bruteForceAnswer <- bruteForceCalculator(rawIndices)
        ## if there is a shuffle on LHS,
        ## the order of results can differ, so we sort rows
        ## before checking equivalence
        expect_equivalent(matrix_sort(rulesAnswer),
                          matrix_sort(bruteForceAnswer),
                          info = paste("answers for flat index", i))
    }
}


test_that("arbitraryIndexRuleClass", {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))
    
    singleContext2ni <-
        modelSingleContext(forCode = quote(for(j in 1:n[i]){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij<- modelContextClass$new(list(singleContext1,
                                            singleContext2))
    
    context_ijni<- modelContextClass$new(list(singleContext1,
                                              singleContext2ni))

    
## Example case:
## for(i in 1:10)
##     for(j in 1:5)
##         y[i, j] <- foo(x[i+1, j + 2]

    setupRules <- indexRule_arbitrary_setup(
        toIndexExprList = list(
            t1 = quote(i),
            t2 = quote(j)),
        fromIndexExprList = list(
            f1 = quote(i+1),
            f2 = quote(j+2)),
        context = context_ij,
        constants = new.env())
    
    expect_equal(indexRule_arbitrary_apply_single(c(5, 3),
                                                setupRules),
                 matrix(c(4, 1), nrow = 1))

    expect_equal(indexRule_arbitrary_apply_matrix(matrix(c(5, 3, 6, 3),
                                                         byrow = TRUE,
                                                         nrow = 2),
                                                  setupRules),
                 matrix(c(4, 1, 5, 1),
                        byrow = TRUE,
                        nrow = 2))

    ## re-do previous case using indexRuleClass_arbitrary
    thisRule <- indexRuleClass_arbitrary$new(
        toIndexExprList = list(
            t1 = quote(i),
            t2 = quote(j)),
        fromIndexExprList = list(
            f1 = quote(i+1),
            f2 = quote(j+2)),
        context = context_ij)

    thisRule$applyOne(c(5, 3))
    debug(thisRule$apply)
    thisAns <- thisRule$apply(indexRange_matrix(matrix(c(5, 3, 6, 3),
                                                       byrow = TRUE,
                                                       nrow = 2)))
    expect_identical(attr(thisAns, "rangeType"), "matrix")
    expect_identical(class(thisAns), "indexRange")
   
    expect_equivalent(thisAns,
                      indexRange_matrix(
                          matrix(c(4, 1, 5, 1),
                                 byrow = TRUE,
                                 nrow = 2)))

    indexRangeList2matrix(list(indexRange(quote(c(1, 2, 3))),
                               indexRange(quote(2:4))
                               )
                          )
    
    ## Non-scalar RHS test
    ## for(i in 1:10) 
    ##     for(j in 1:5) 
    ##         y[i, j] <- foo(x[i+1, 1:(j+1)])
    
    setupRules <- indexRule_arbitrary_setup(
        toIndexExprList = list(
            t1 = quote(i),
            t2 = quote(j)),
        fromIndexExprList = list(
            f1 = quote(i+1),
            f2 = quote(1:(j+2))),
        context = context_ij,
        constants = new.env())

    expect_equal(indexRule_arbitrary_apply_single(c(4, 6),
                                                setupRules),
                 matrix(c(3, 4, 3, 5), byrow = TRUE, nrow = 2))

    ## Non-scalar on LHS
    ## for(i in 1:10)
    ##  y[i, 1:n[i] ] <- foo(x[i+1])


    setupRules <- indexRule_arbitrary_setup(
        toIndexExprList = list(
            t1 = quote(i),
            t2 = quote(1:n[i])),
        fromIndexExprList = list(
            f1 = quote(i+1)),
        context = context_i,
        constants = list2env(list(n = 1:10))
    )

    expect_equivalent(test <- indexRule_arbitrary_apply_single(c(2),
                                                             setupRules),
                      matrix(c(1, 1), nrow = 1))

    ## Ragged definition
    ## for(i in 1:10)
    ##     for(j in 1:n[i])
    ##         y[i, j] <- foo(x[i, j])
    setupRules <- indexRule_arbitrary_setup(
        toIndexExprList = list(
            t1 = quote(i),
            t2 = quote(j)),
        fromIndexExprList = list(
            f1 = quote(i),
            f2 = quote(j)),
        context = context_ijni,
        constants = list2env(list(n = 1:10))
    )

    expect_equal(indexRule_arbitrary_apply_single(c(8, 2),
                                                setupRules),
                 matrix(c(8, 2), nrow = 1))

    test_arbitraryIndexRule(quote(y[i]),
                            quote(x[i]),
                            context_i)

    test_arbitraryIndexRule(quote(y[i + 1]),
                            quote(x[i]),
                            context_i)

    test_arbitraryIndexRule(quote(y[i]),
                            quote(x[i + 1]),
                            context_i)

    test_arbitraryIndexRule(quote(y[i + 1]),
                            quote(x[i + 1]),
                            context_i)

    test_arbitraryIndexRule(quote(y[i + 3]),
                            quote(x[i + 2]),
                            context_i)

    block <- as.integer(c(1, 3, 2, 3, 1, 2, 1, 3, 2, 3))

    test_arbitraryIndexRule(quote(y[i + 3]),
                            quote(x[block[i] + 2]),
                            context_i,
                            constants = list(block = block))

    gappy_block <- as.integer(c(1, 3, 1, 3, 1, 1, 1, 3, 3, 3))

    test_arbitraryIndexRule(quote(y[i + 3]),
                            quote(x[gappy_block[i] + 2]),
                            context_i,
                            constants = list(gappy_block = gappy_block))


    shuffle <- as.integer(c(5, 4, 1, 9, 10, 8, 6, 2, 7, 3))
    test_arbitraryIndexRule(quote(y[shuffle[i] + 3]),
                            quote(x[i + 2]),
                            context_i,
                            constants = list(shuffle = shuffle))

    test_arbitraryIndexRule(quote(y[shuffle[i] + 3]),
                            quote(x[block[i] + 2]),
                            context_i,
                            constants = list(shuffle = shuffle,
                                             block = block))


    ## 2D

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[i, j]),
                            context_ij)

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[j, i]),
                            context_ij)

    test_arbitraryIndexRule(quote(y[j, i]),
                            quote(x[i, j]),
                            context_ij)

    test_arbitraryIndexRule(quote(y[j, i]),
                            quote(x[j, i]),
                            context_ij)

    test_arbitraryIndexRule(quote(y[i + 5, j + 3]),
                            quote(x[i, j]),
                            context_ij)

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[i + 5, j + 3]),
                            context_ij)

    test_arbitraryIndexRule(quote(y[j + 7, i + 11]),
                            quote(x[i + 5, j + 3]),
                            context_ij)

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[block[i], j]),
                            context_ij,
                            constants = list(block = block))

    block2 <- c(5,4,5,3,4)

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[block[i], block2[j]]),
                            context_ij,
                            constants = list(block = block,
                                             block2 = block2))

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[block2[j], block[i]]),
                            context_ij,
                            constants = list(block = block,
                                             block2 = block2))

    test_arbitraryIndexRule(quote(y[shuffle[i] + 3, j+5]),
                            quote(x[block2[j] + 11, block[i] + 7]),
                            context_ij,
                            constants = list(shuffle = shuffle,
                                             block = block,
                                             block2 = block2))

    ## cases where j extent depends on i
    raggedRowLengths <- c(5, 8, 3, 9, 4, 5, 2, 1, 9, 2)

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[i, j]),
                            context_ijni,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[j + 7, i + 11]),
                            quote(x[i + 5, j + 3]),
                            context_ijni,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[i, 1:n[i]]),
                            quote(x[i]),
                            context_i,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[i+3, 4:(n[i] + 5)]),
                            quote(x[i+11]),
                            context_i,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[4:(n[i] + 5), i + 3]),
                            quote(x[i+11]),
                            context_i,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[i]),
                            quote(x[1:n[i]]),
                            context_i,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[i + 5]),
                            quote(x[2:n[i]]),
                            context_i,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[i + 5]),
                            quote(x[2:n[block[i]]]),
                            context_i,
                            constants = list(block = block,
                                             n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[i, j]),
                            quote(x[2:n[i]]),
                            context_ij,
                            constants = list(n = raggedRowLengths))

    test_arbitraryIndexRule(quote(y[i + 5, j]),
                            quote(x[j + 3, 2:n[i]]),
                            context_ij,
                            constants = list(n = raggedRowLengths))
}
)
