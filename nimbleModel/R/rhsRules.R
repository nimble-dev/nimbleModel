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

            super$initialize(expr, ID, context = context, constants = constants)

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
    intersection <- RHSrule$apply(LHSrange)
    if(intersection$isEmpty())
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
                                            constants = c(list(.idx = valsRHS), RHSrule$constants)) 
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

                resultRule <- rhsRuleClass$new(expr, 1, context = modelContextClass$new(newSingleContexts), constants = RHSrule$constants)
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
               
                resultRule1 <- rhsRuleClass$new(expr1, 1, context = modelContextClass$new(newSingleContexts1), constants = RHSrule$constants)
                resultRule2 <- rhsRuleClass$new(expr2, 1, context = modelContextClass$new(newSingleContexts2), constants = RHSrule$constants)
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
                                        constants = c(constants, RHSrule$constants))
        return(list(resultRule))
     }
}

