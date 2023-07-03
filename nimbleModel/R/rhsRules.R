## A rhsRuleClass object represents the indexing information for a variable
## in a right-hand side expression.
## This may come from the original declaration, or from excluding from that declaration
## elements that appear on the left-hand side of another expression.

## Constant vectors are treated as sequences because we need indexing for them
## when determining RHSonly.
## TODO: clarify above statement to be more specific.

rhsRuleClass <- R6Class(
    classname = "rhsRuleClass",
    portable = FALSE,
    inherit = nodeRuleClass,
    public = list(
        usedInIndex = FALSE,  # Allows tracking of use as dynamic index.
        
        initialize = function(expr, ID = NULL, context = modelContextClass$new(), constants = list(), usedInIndex = FALSE) {
            usedInIndex <<- usedInIndex
            ## Process any dynamic indexing.
            if(getNimbleModelOption('allowDynamicIndexing') && isUsedInIndex(expr)) {
                usedInIndex <<- TRUE
                expr <- stripIndexWrapping(expr)
            }

            ## Indices for dynamically-indexed RHS vars cannot be determined
            ## so set to some very large value (can't use `1:Inf`).
            if(getNimbleModelOption('allowDynamicIndexing')) 
                if(length(expr) == 3 && isDynamicIndex(expr[[3]])) {
                    expr[[3]] <- quote(1:2)
                    expr[[3]][[3]] <- .Machine$integer.max
                }

            ## Replace sequence indexing with maximal indexing in cases
            ## in which could have duplicate declarations of RHS because
            ## one or more singleContexts are not used in indexing the RHS
            ## but are used in for loop expression, e.g.,
            ## `for(i in 1:4) for(t in 1:seasons[i]) y[i,t] <- alpha[t]`.
            usedIndexVarsBool <- names(context$singleContexts) %in% all.vars(expr)
            if(exists('paciorek')) browser()
            if(any(usedIndexVarsBool) && !all(usedIndexVarsBool)) {
                usedIndexVars <- names(context$singleContexts)[usedIndexVarsBool]
                depIndexVarExprs <- sapply(usedIndexVars,
                                           function(var) 
                                               !all(all.vars(context$singleContexts[[var]]$indexRangeExpr) %in%
                                                    names(constants)))
                usedIndexVars <- usedIndexVars[depIndexVarExprs]
                if(length(usedIndexVars)) {  ## Only if index expr uses other indices.
                    allReplacements <- lapply(usedIndexVars, as.name)
                    names(allReplacements) <- paste0("t", seq_along(allReplacements))
                    unrolledIndicesEnv <-
                        expandContextAndReplacements(
                            allReplacements = allReplacements,
                            allReplacementNameExpr = lapply(names(allReplacements), as.name),
                            context = context,
                            constants = constants
                        )
                    rgs <- lapply(usedIndexVars, function(x)  ## Use full extent of indexing across all iterations.
                        range(unrolledIndicesEnv[[x]]))
                    newSingleContexts <- context$singleContexts
                    ## Assign full extent back into singleContext indexRangeExpr, removing
                    ## dependence on other indices.
                    newSingleContexts[usedIndexVars] <- lapply(seq_along(usedIndexVars),
                                                function(i) {
                                                    singleContextClass$new(
                                                                           indexVarExpr = as.name(usedIndexVars[i]),
                                                                           indexRangeExpr = substitute(L:M, list(L = rgs[[i]][1], M = rgs[[i]][2])))
                                                })
                    context <- modelContextClass$new(newSingleContexts)
                }
            }

            ## Transform constants into sequences.
            if(length(expr) > 1 && expr[[1]] == "[") {
                scalarConstants <- sapply(3:length(expr),
                                         function(i) length(expr[[i]]) == 1 && !length(all.vars(expr[[i]])))
                blockConstants <- sapply(3:length(expr),
                                         function(i) length(expr[[i]]) > 1 && expr[[i]][[1]] == ":" && !length(all.vars(expr[[i]])))

                scalarOrBlockConstants <- scalarConstants | blockConstants
                if(any(scalarOrBlockConstants)) {
                    newSingleContexts <- context$singleContexts
                    cnt <- length(newSingleContexts)
                    for(idx in which(scalarOrBlockConstants)) {
                        cnt <- cnt + 1
                        newSingleContexts[[cnt]] <- singleContextClass$new(
                               indexVarExpr = parse(text=paste0(".block", cnt))[[1]],
                               indexRangeExpr = if(scalarConstants[idx]) 
                                                       substitute(A:A, list(A = expr[[idx+2]])) 
                                                       else expr[[idx+2]])
                           expr[[idx+2]] <- newSingleContexts[[cnt]]$indexVarExpr
                    }
                    context <- modelContextClass$new(newSingleContexts)
                }
            }
            super$initialize(expr, ID, context = context, constants = constants, isRHS = TRUE)
        }
    )
)


## Takes a RHS rule (created from variable used in original RHS of an expression) and intersects it with another rule,
## which could be a `rhsRule` or a `declRule`, from use of the variable either on the LHS or the RHS.
## Result can be:
##  - no intersection: RHS passed through,
##  - RHS is fully in LHS: result is NULL, or
##  - partly intersects: fracture and return one or more fractured RHS rules.
## This could be a method of `rhsRuleClass`, but it is long, and the result can
## be to remove the `rhsRule` object or split it into two rules, so it's not naturally
## set up as a method that modifies the object.


exclude <- function(rhsRule, excludingRule, constants = list()) {
    ## Can also take varRanges for use in `traverseGraph`.
    if(is(excludingRule, 'varRangeClass')) {
        excludingRange <- excludingRule
    } else excludingRange <- excludingRule$getFullRange()
    if(is(rhsRule, 'varRangeClass')) {
        rhsRange <- rhsRule
        rhsRule <- rhsRule$toRule()
    } else rhsRange <- rhsRule$getFullRange()
    intersection <- rhsRule$apply(excludingRange)

    if(is.null(intersection)) # no overlap
        return(list(rhsRule))
    if(varRange_isEqual(rhsRange, intersection)) # full overlap
        return(NULL)

    ## Partial overlap case. We need to fracture the rhsRule.

    identicalIndices <- sapply(seq_along(rhsRange$indexSlotToRange), function(idx)
        isTRUE(all.equal(rhsRange$indexRanges[[rhsRange$indexSlotToRange[idx]]],
                  intersection$indexRanges[[intersection$indexSlotToRange[[idx]]]])))

    nonIdenticalIndices <- which(!identicalIndices)
    
    expr <- rhsRule$expr
    singleContexts <- rhsRule$context$singleContexts
    
    if(length(rhsRule$fullRule$indexSets$toIndexSlotToSet) == 1 || length(nonIdenticalIndices) == 1) {
        ## TODO: remove this check when have done full testing and presumably simply above to only check 2nd condition.
        if(length(rhsRule$fullRule$indexSets$toIndexSlotToSet) == 1 && length(nonIdenticalIndices) != 1)
            stop("DEBUG: check.")

        ## split, shrink, or remove from focal index, and combine with other indices
        RHS <- rhsRange$indexRanges[[rhsRange$indexSlotToRange[nonIdenticalIndices]]]
        int <- intersection$indexRanges[[intersection$indexSlotToRange[nonIdenticalIndices]]]

        focalContext <- sapply(names(singleContexts), function(nm)
            nm %in% all.vars(expr[[2+nonIdenticalIndices]]))
        
        if(is(RHS, "indexRangeMatrixClass") || is(int, "indexRangeMatrixClass")) {
            ## Handle any matrix cases by expanding elements.
            valsRHS <- switch(class(RHS)[1],
                              indexRangeMatrixClass = RHS$values,
                              indexRangeScalarClass = RHS$value,
                              indexRangeSequenceClass = as.numeric(RHS$start:RHS$end),
                              stop("`RHS` type not found.")
                              )
            valsInt <- switch(class(int)[1],
                              indexRangeMatrixClass = int$values,
                              indexRangeScalarClass = int$value,
                              indexRangeSequenceClass = as.numeric(int$start:int$end),
                              stop("`int` type not found.")
                              )
            valsRHS <- valsRHS[!valsRHS %in% valsInt]

            ## Modify rhsRule expr and context to insert vector of relevant values.
            newSingleContexts <- singleContexts[!focalContext]
            newSingleContexts[[length(newSingleContexts)+1]] <- singleContextClass$new(
                               indexVarExpr = quote(.newidx),
                indexRangeExpr = substitute(1:L, list(L = length(valsRHS))))

            newcode <- paste0(".idx", nonIdenticalIndices, "[.newidx]")
            expr[[nonIdenticalIndices+2]] <- parse(text = newcode[1])[[1]]

            ## Replace any constants related to an index slot processed in a previous
            ## call to `exclude`.
            constants <- list(valsRHS)
            names(constants) <- paste0(".idx", nonIdenticalIndices)
            oldConstants <- constants
            oldConstants[names(oldConstants) %in% names(constants)] <- NULL

            resultRule <- rhsRuleClass$new(expr, context = modelContextClass$new(newSingleContexts),
                                           constants = c(constants, oldConstants), usedInIndex = rhsRule$usedInIndex)
            return(list(resultRule))
        } else {  # seq+seq or seq+scalar
            if(is(int, "indexRangeScalarClass"))  # convert to sequence to avoid special case code
                int <- newIndexRange(substitute(A:A, list(A = int$value)))
            if(is(RHS, "indexRangeScalarClass"))
                stop("not expecting RHS to be a scalar.")  ## scalar RHS either fully intersected or not intersected

            ## Now process two sequences.
            if(int$start == RHS$start || int$end == RHS$end) {
                ## Shrink existing index block
                if(int$start == RHS$start) 
                    RHS$start <- int$end+1 else RHS$end <- int$start-1

                newSingleContexts <- singleContexts[!focalContext]
                newSingleContexts[[length(newSingleContexts)+1]] <- singleContextClass$new(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = RHS$start, B = RHS$end)))
                expr[[nonIdenticalIndices+2]] <- newSingleContexts[[length(newSingleContexts)]]$indexVarExpr

                resultRule <- rhsRuleClass$new(expr, context = modelContextClass$new(newSingleContexts),
                                               constants = constants, usedInIndex = rhsRule$usedInIndex)
                return(list(resultRule))
            } else {
                ## Modify rhsRule expr and context to create two new rules.
                newSingleContexts1 <- singleContexts[!focalContext]
                newSingleContexts2 <- singleContexts[!focalContext]

                expr1 <- expr2 <- expr
                newSingleContexts1[[length(newSingleContexts1)+1]] <- singleContextClass$new(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = RHS$start, B = int$start-1)))
                expr1[[nonIdenticalIndices+2]] <- newSingleContexts1[[length(newSingleContexts1)]]$indexVarExpr

                newSingleContexts2[[length(newSingleContexts2)+1]] <- singleContextClass$new(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = int$end+1, B = RHS$end)))
                expr2[[nonIdenticalIndices+2]] <- newSingleContexts2[[length(newSingleContexts2)]]$indexVarExpr
               
                resultRule1 <- rhsRuleClass$new(expr1, context = modelContextClass$new(newSingleContexts1),
                                                constants = constants, usedInIndex = rhsRule$usedInIndex)
                resultRule2 <- rhsRuleClass$new(expr2, context = modelContextClass$new(newSingleContexts2),
                                                constants = constants, usedInIndex = rhsRule$usedInIndex)
                return(list(resultRule1, resultRule2))
            }
        }
    } else { 
        ## Scenarios that are not the simple setting of a single index slot that needs to be considered.
        ## Create fully unrolled matrix of indices for non-identical indices, do exclusion,
        ## then create new arbitrary rhsRule by creating a complicated context, crossed with any indices that are identical
        unrolledRHS <- rhsRange$extractIndexRange(nonIdenticalIndices)
        unrolledIntersection <- intersection$extractIndexRange(nonIdenticalIndices)

        ## Convert matrices of index values by row to strings to allow matching.
        rhsAsChar <- do.call(paste, as.data.frame(unrolledRHS$values))
        intAsChar <- do.call(paste, as.data.frame(unrolledIntersection$values))

        remaining <- !rhsAsChar %in% intAsChar
        remainingVals <- unrolledRHS$values[remaining, , drop = FALSE]

        ## Retain singleContexts for identical indices.
        focalSingleContexts <- sapply(names(singleContexts), function(nm)
            nm %in% unlist(lapply(2+nonIdenticalIndices, function(x) all.vars(expr[[x]]))))
        if(sum(!focalSingleContexts)) {
            newSingleContexts <- singleContexts[!focalSingleContexts]
        } else newSingleContexts <- list()

        newSingleContexts[[length(newSingleContexts) + 1]] <- singleContextClass$new(
                               indexVarExpr = quote(.newidx),
            indexRangeExpr = substitute(1:L, list(L = nrow(remainingVals))))

        newcode <- paste0(".idx", nonIdenticalIndices, "[.newidx]")
        for(i in seq_along(nonIdenticalIndices)) 
            expr[[nonIdenticalIndices[i]+2]] <- parse(text = newcode[i])[[1]]

        ## Replace any constants related to an index slot processed in a previous
        ## call to `exclude`.
        oldConstants <- constants
        constants <- lapply(seq_len(ncol(remainingVals)), function(i) remainingVals[ , i])
        names(constants) <- paste0(".idx", nonIdenticalIndices)
        oldConstants[names(oldConstants) %in% names(constants)] <- NULL
        resultRule <- rhsRuleClass$new(expr, context = modelContextClass$new(newSingleContexts),
                                       constants = c(constants, oldConstants), usedInIndex = rhsRule$usedInIndex)
        return(list(resultRule))
     }
}

