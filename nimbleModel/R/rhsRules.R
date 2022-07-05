rhsRuleClass <- R6Class(
    classname = "rhsRuleClass",
    portable = FALSE,
    inherit = nodeRuleClass,
    public = list(

        initialize = function(expr, ID, context = modelContextClass$new(), constants = list()) {

            ## Treat constants in RHS as sequences because we need indexing for them when determining RHSonly.
            if(length(expr) && expr[[1]] == "[") {
                scalarConstants <- sapply(3:length(expr),
                                         function(i) length(expr[[i]]) == 1 && !length(all.vars(expr[[i]])))
                blockConstants <- sapply(3:length(expr),
                                         function(i) length(expr[[i]]) > 1 && expr[[i]][[1]] == ":" && !length(all.vars(expr[[i]])))
                if(any(scalarConstants) || any(blockConstants)) { 
                    newSingleContexts <- context$singleContexts
                    cnt  <- length(newSingleContexts)
                    if(any(scalarConstants)) {
                       for(idx in which(scalarConstants)) {
                           cnt <- cnt + 1
                           newSingleContexts[[cnt]] <- modelSingleContext(
                               indexVarExpr = parse(text=paste0(".block", cnt))[[1]],
                               indexRangeExpr = substitute(A:A, list(A = expr[[idx+2]])))
                           expr[[idx+2]] <- newSingleContexts[[cnt]]$indexVarExpr
                       }
                    }
                    if(any(blockConstants)) {
                       for(idx in which(blockConstants)) {
                           cnt <- cnt + 1
                           newSingleContexts[[cnt]] <- modelSingleContext(
                               indexVarExpr = parse(text=paste0(".block", cnt))[[1]],
                               indexRangeExpr = expr[[idx+2]])
                           expr[[idx+2]] <- newSingleContexts[[cnt]]$indexVarExpr
                       }
                    }
                    context <- modelContextClass$new(newSingleContexts)
                }
            }

            super$initialize(expr, ID, context = modelContextClass$new(), constants = list())

        }
    )
)


## Takes a RHS rule (created from original RHS of an expression) and intersects it with a LHS rule.
## Result can be:
##  - no intersection: RHS passed through
##  - RHS is fully in LHS: NULL
##  - partly intersects: fracture and return one or more fractured RHS rules

## Is the intersection with calcRules or with declRules or both? I think with calcRules, where input declRule to the calcRule can be NULL for testing

## This might not be a method of rhsRuleClass because result can be to remove the RHSrule if have complete overlap,
## or split the rule into two rules.
## But could be a method and code that manipulates the rhsRule could throw away the original rule.

exclude <- function(RHSrule, LHSrule) {
    LHSrange <- LHSrule$getFullRange()
    RHSrange <- RHSrule$getFullRange()
    intersection <- RHSrule$apply(LHSrange)$getVarRange()
    if(varRange_isEmpty(intersection))
        return(list(RHSrule))
    if(varRange_isEqual(RHSrange, intersection)) 
        return(NULL)
    ## otherwise need to fracture the RHSrule
    ## if intersect, need new IDs?
    identicalIndices <- sapply(seq_along(RHSrange$indexID_2_rangeID), function(idx)
        isTRUE(all.equal(RHSrange$indexRanges[[RHSrange$indexID_2_rangeID[idx]]],
                  intersection$indexRanges[[intersection$indexID_2_rangeID[[idx]]]])))

    nonIdenticalIndices <- which(!identicalIndices)
    
    expr <- RHSrule$expr
    singleContexts <- RHSrule$context$singleContexts
    
    if(RHSrule$numIndices == 1 || length(nonIdenticalIndices) == 1) {
        ## split, shrink, or remove from focal index, and combine with other indices
        RHS <- RHSrange$indexRanges[[RHSrange$indexID_2_rangeID[nonIdenticalIndices]]]
        int <- intersection$indexRanges[[intersection$indexID_2_rangeID[nonIdenticalIndices]]]
        typeRHS <- attr(RHS, "rangeType")
        typeInt <- attr(int, "rangeType")

        focalContext <- sapply(names(singleContexts), function(nm)
            nm %in% all.vars(expr[[2+nonIdenticalIndices]]))
        
        if(typeRHS == "matrix" || typeInt == "matrix") {
            valsRHS <- switch(typeRHS,
                              matrix = RHS[[1]],
                              scalar = RHS[[1]],
                              sequence = RHS[[1]][[1]]:RHS[[1]][[2]],
                              stop("typeRHS not found")
                              )
            valsInt <- switch(typeInt,
                              matrix = int[[1]],
                              scalar = int[[1]],
                              sequence = int[[1]][[1]]:int[[1]][[2]],
                              stop("typeInt not found")
                              )
            valsRHS <- valsRHS[!valsRHS %in% valsInt]

            ## Modify RHSrule expr and context to insert vector of relevant values.
            newSingleContexts <- singleContexts[!focalContext]
            newSingleContexts[[length(newSingleContexts)+1]] <- modelSingleContext(
                               indexVarExpr = quote(.newidx),
                indexRangeExpr = substitute(1:L, list(L = length(valsRHS))))

            expr[[nonIdenticalIndices+2]] <- quote(.idx[.newidx])
         
            resultRule <- rhsRuleClass$new(expr, 1, context = modelContextClass$new(newSingleContexts),
                                            constants = list(.idx = valsRHS)) 
            return(list(resultRule))
        } else {  # seq+seq or seq+scalar
            if(typeInt == "scalar")
                int <- indexRange(substitute(A:A, list(A = int[[1]])))
            ## now process two seqs
            if(typeRHS == "scalar") stop("Not expecting RHS to be a scalar")  ## scalar RHS either fully intersected or not intersected
          
            if(int[[1]][1] == RHS[[1]][[1]] || int[[1]][[2]] == RHS[[1]][[2]]) {
                ## Shrink existing index block
                if(int[[1]][[1]] == RHS[[1]][[1]]) 
                    RHS[[1]][[1]] <- int[[1]][[2]]+1 else RHS[[1]][[2]] <- int[[1]][[1]]-1

                newSingleContexts <- singleContexts[!focalContext]
                newSingleContexts[[length(newSingleContexts)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = RHS[[1]][[1]], B = RHS[[1]][[2]])))
                expr[[nonIdenticalIndices+2]] <- newSingleContexts[[length(newSingleContexts)]]$indexVarExpr

                resultRule <- rhsRuleClass$new(expr, 1, context = modelContextClass$new(newSingleContexts))
                return(list(resultRule))
            } else {
                ## Modify RHSrule expr and context to create two new rules.
                newSingleContexts1 <- singleContexts[!focalContext]
                newSingleContexts2 <- singleContexts[!focalContext]

                expr1 <- expr2 <- expr
                newSingleContexts1[[length(newSingleContexts1)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = RHS[[1]][[1]], B = int[[1]][[1]]-1)))
                expr1[[nonIdenticalIndices+2]] <- newSingleContexts1[[length(newSingleContexts1)]]$indexVarExpr

                newSingleContexts2[[length(newSingleContexts2)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = int[[1]][[2]]+1, B = RHS[[1]][[2]])))
                expr2[[nonIdenticalIndices+2]] <- newSingleContexts2[[length(newSingleContexts2)]]$indexVarExpr
               
                resultRule1 <- rhsRuleClass$new(expr1, 1, context = modelContextClass$new(newSingleContexts1))
                resultRule2 <- rhsRuleClass$new(expr2, 1, context = modelContextClass$new(newSingleContexts2))
                return(list(resultRule1, resultRule2))
            }
        }
    } else { ## not simple setting of a single non-identical scalar index that needs to be considered
        ## unroll, exclude, create new arbitrary RHSrule by creating a complicated context, crossed with any indices that are identical
        unrolledRHS <- RHSrange$getIndexRangeMatrix(nonIdenticalIndices)
        unrolledIntersection <- intersection$getIndexRangeMatrix(nonIdenticalIndices)

        rhsAsChar <- do.call(paste, as.data.frame(unrolledRHS[[1]]))
        intAsChar <- do.call(paste, as.data.frame(unrolledIntersection[[1]]))

        remaining <- !rhsAsChar %in% intAsChar
        mat <- unrolledRHS[[1]][remaining, ]

        focalContext <- sapply(names(singleContexts), function(nm)
            nm %in% unlist(lapply(2+nonIdenticalIndices, function(x) all.vars(expr[[x]]))))
        if(sum(!focalContext)) {
            newSingleContexts <- singleContexts[!focalContext]
        } else newSingleContexts <- list()

        newSingleContexts[[length(newSingleContexts) + 1]] <- modelSingleContext(
                               indexVarExpr = quote(.newidx),
            indexRangeExpr = substitute(1:L, list(L = nrow(mat))))

        nms <- paste0(".idx", seq_len(ncol(mat)), "[.newidx]")
        for(i in seq_along(nonIdenticalIndices)) 
            expr[[nonIdenticalIndices[i]+2]] <- parse(text = nms[i])[[1]]
        constants <- lapply(seq_len(ncol(mat)), function(i) mat[,i])
        names(constants) <- paste0(".idx", seq_along(constants))
        resultRule <- rhsRuleClass$new(expr, 1, context = modelContextClass$new(newSingleContexts),
                                        constants = constants)
        return(list(resultRule))
     }
}

## need tests for:
## setting up LHS node rules
## setting up RHS rules
## applying rules to get nodeRanges

## I think nodeRuleClass can stay as is, even though in real work, input would be a calcRule.

if(FALSE) {
    ## Hopefully comprehensive testing of exclude(); move into test-nodeRules.R or test-exclude.R
    library(nimbleModel)
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:4){}))
    
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))
    context_j <- modelContextClass$new(list(singleContext2))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

    ## scalar/seq overlap at end
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)[[1]]

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)

    ## scalar/seq overlap no overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[33])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)

    result <- exclude(RHSrule, LHSrule)
    expect_identical(result[[1]], RHSrule)

    ## scalar/seq overlap in middle
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[4])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:2){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)


    ## seq/seq partial overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 5:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)


    ## seq/seq full overlap
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:9){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)
    expect_identical(result, NULL)

    
    ## matrix in LHS
    RHS <- quote(mu[i+1])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[idx[i]])
    idx <- c(2,5,4)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp, constants = list(idx = idx))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    idx <- as.integer(c(3,6,7,8,9))
    expected <- rhsRuleClass$new(LHS, 1, context_tmp, constants = list(idx = idx))

    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## matrix in RHS
    idx <- c(14,4,2,9,1,3,7,11)
    RHS <- quote(mu[idx[i]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx = idx))
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:4){}))))
    idx <- as.integer(c(4,7,9,11))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx = idx))

    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## two-d arbitrary case, extracting block elements from RHS matrix
    RHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx1 = idx1, idx2 = idx2))
    LHS <- quote(mu[i,j])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    idx1 <- c(11,11,12,13,5)
    idx2 <- c(2,5,6,7,13)
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    

    ## two-d arbitrary case, extracting matrix elements from RHS block
    RHS <- quote(mu[i,j])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij)
    LHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i, constants = list(idx1 = idx1, idx2 = idx2))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:26){}))))
    idx1 <- c(2,3,5,6,7,8,2,4:8,rep(2:8, 2))
    idx2 <- c(rep(1,6),rep(2,6),rep(3,7), rep(4,7))
    expected <- rhsRuleClass$new(LHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case with constant
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case, all excluded
    RHS <- quote(mu[5,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_0)
    LHS <- quote(mu[i, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    
    result <- exclude(RHSrule, LHSrule)
    expect_identical(result, NULL)

    ## basic mv node case with seq-seq partial overlap
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:5){}))))
    LHS <- quote(mu[i+1, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:8){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case with shared matrix index
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1,2)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx = idx))
    LHS <- quote(mu[5, idx[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = list(idx = idx))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    
    ## Awkward intersections

    ## Partial overlap in some rows; for now this is simply unrolled.
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:17){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## LHS element inside RHS; for now this is simply unrolled
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    LHS <- quote(mu[3, 3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:20){}))))
    idx1 <- c(2:8,2:8,2,4:8)
    idx2 <- c(rep(1,7), rep(2,7), rep(3, 6))
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)


    ## LHS fully overlaps RHS block constant in additional dimension; this is handled nicely.
    RHS <- quote(mu[i,1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:4])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:6){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)

    ## basic 3-d case - two identical indices
    RHS <- quote(mu[1:3, i, 1:2])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[1:3, i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
 
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:6){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     RHSrule$externalRules$indexRules[[3]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[3]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$index2setID, c(1,3,2))

    ## 3-d case with partial overlap in some rows but with additional identical index (j)
    RHS <- quote(mu[i, j, 1:3])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    LHS <- quote(mu[i, j, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:17){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    expr <- quote(mu[idx1[i], j, idx2[i]])
    expected <- rhsRuleClass$new(expr, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)

    ## 3-d case with multi-column shared matrix indexRange
    idx1 <- c(4,7,1,2)
    idx2 <- c(1,9,3,2)
    
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    LHS <- quote(mu[idx1[j], 2, idx2[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_j, constants = list(idx1 = idx1, idx2 = idx2))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- rhsRuleClass$new(RHS, 1, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## 3-d case with multi-column unshared matrix indexRange
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    idx3 <- c(4,7,2,2)
    idx4 <- c(1,10,3,2)
    LHS <- quote(mu[idx3[j], i, idx4[j]])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_ij, constants = list(idx3 = idx3, idx4 = idx4))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:2){}))))
    idx5 <- c(1,7)
    idx6 <- c(3,9)
    expr <- quote(mu[idx5[j], i, idx6[j]])
    expected <- rhsRuleClass$new(expr, 1, context_tmp, constants = list(idx5 = idx5, idx6 = idx6))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[2]]$setupResults)
    
    ## This is not error-trapped (use of index and constant in wrong way)
    RHS <- quote(mu[i[idx]])
    idx <- c(4,7,1)
    RHSrule <- rhsRuleClass$new(RHS, 1, context_i, constants = list(idx = idx))
    
    ## incorrect length of constant (move this check to test-graphRules, probably).
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1)
    expect_error(RHSrule <- rhsRuleClass$new(RHS, 1, context_ij, constants = list(idx = idx)),
                 "Missing values found in setting up arbitrary indexRule")
}

if(FALSE) {
    ## Checking getFullRange
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$getFullRange(),
                     varRangeClass$new(list(indexRange(5), indexRange(quote(1:3)))))

    LHS <- quote(mu[4:5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_0)
    expect_equal(LHSrule$getFullRange(),
                     varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:3)))))
    
    LHS <- quote(mu[4:5, i, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(2:8)), indexRange(quote(1:3)))))
    
    expr <- quote(mu[4:5, j, i, 3])
    LHSrule <- nodeRuleClass$new(expr, 1, context_ij)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:4)),
                                        indexRange(quote(2:8)), indexRange(quote(3)))))
    RHSrule <- rhsRuleClass$new(expr, 1, context_ij)
    expect_equal(RHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:4)),
                                        indexRange(quote(2:8)), indexRange(quote(3)))))

    LHS <- quote(mu[4:5, i, i, 3])
    LHSrule <- nodeRuleClass$new(LHS, 1, context_i)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(matrix(rep(2:8, 2), ncol = 2)), indexRange(3))))
    
    expr <- quote(mu[j, 1:3, i, 2])
    RHSrule <- rhsRuleClass$new(expr, 1, context_ij)
    LHSrule <- nodeRuleClass$new(expr, 1, context_ij)

    expect_equal(RHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(1:4)), indexRange(quote(1:3)),
                                        indexRange(quote(2:8)), indexRange(2))))

}


