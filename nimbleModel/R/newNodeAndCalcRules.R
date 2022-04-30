## new framework for node, calc, nodeType, RHS rules

## basic type of class is nodeRule
## calcRules are just nodeRules (either from original declaration or fractured based on top-down processing)
## RHSonlyRules are just nodeRules with certain fields set to NULL as not relevant
## could use some inheritance from a base nodeRuleClass

## {top,end,latent} rules are just lists of pointers/shallow copies of calcRules
## {stoch,dep} rules are just lists of pointers/shallow copies of nodeRules

## key functions:
## exclude(RHSonlyRule,nodeRule/LHS) -> RHSonlyRule (0 or more)
## fracture(dep, nodeRule/LHS, stochParent, parentID) -> calcRule (0 or more)

## have rulesListClass to serve as the nodeRules, calcRules, topRules, etc.
## rulesListClass could contain the counter of the number of included rules

nodeRuleClass <- R6Class(
    classname = "nodeRuleClass",
    portable = FALSE,
    public = list(
        varName = character(),
        ID = numeric(),
        sortID = numeric(),
        stoch = logical(),
        numIndices = numeric(),
        originalRule = NULL, # pointer to canonical nodeRule from declaration (possibly `self`)
        allRules = NULL,
        externalRules = NULL, # indexing for the nodes
        internalRules = NULL, # indexing for components, if multivariate nodes
        numExternalRules = numeric(0),
        numInternalRules = numeric(0),
        index2setID = NULL,
        originalIndexRules = NULL, # determines original indexing (based on context); set equal to originalRule$originalIndexRule if not canonical nodeRule
        ## These are used in `exclude`.
        context = NULL,
        expr = NULL,
        constants = NULL,
        
        stochParent = FALSE,
        stochDep = FALSE,
        touchedDown = FALSE,
        touchedUp = FALSE,
        edgesFrom = numeric(),

        top = FALSE,
        end = FALSE,
        ## latent is !top & !end
        RHSonly = FALSE,

        calculate = NULL,  ## generic function for calculation

        initialize = function(expr, isLHS, ID, stoch, context = modelContextClass$new(), constants = list()) {
            ## Set up rules that operate on the indexing of the nodes and on
            ## the internal indexing of the elements of a node.
            if(length(expr) > 1)
                varName <<- expr[[2]] else varName <<- expr   ## not clear everything will go through if no indexing
            stoch <<- stoch
            if(!isLHS)   ## Initialize here, then will determine what is truly RHSonly later using exclude()
                RHSonly <<- TRUE
            ID <<- ID

            ## Treat constants in RHS as sequences because we need indexing for them when determining RHSonly.
            if(RHSonly && length(expr) && expr[[1]] == "[") {
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

            context <<- context
            expr <<- expr
            constants <<- constants
            
            originalIndexRules <<- originalIndexRuleClass$new(expr, context, constants)

            ## Note: this is awkward to go into the data structures and modify them

            ## TODO: modify allRules to be a graphRule
            allRules <<- makeGraphIndexRules(expr, expr, context, constants)
            allRules$constraints <- list()
            index2setID <<- allRules$indexSets$LHSindex2setID
            isBlockConstant <- sapply(allRules$indexRules, is, "indexRuleClass_constant") &
                sapply(allRules$indexRules, function(rule) identical(attr(rule$setupResults[[1]], 'rangeType'), 'sequence'))
            numIndices <<- length(allRules$indexSets$LHSindex2setID)

            ## TODO: replace with call to allRules$getFullRange?

            if(FALSE) {
                if(RHSonly && any(isConstant)) {  # convert constant rules to block rules, as notion of a multivariate RHS is not useful
                    wh <- which(isConstant)
                    for(idx in wh) {
                        rg <- allRules$indexRules[[wh]]$setupResults$constant[[1]]
                        if(!is.list(rg))
                            rg <- list(rg,rg)
                        context_tmp <- modelContextClass$new(list(modelSingleContext(
                                                             indexVarExpr = quote(.i),
                                                             indexRangeExpr = substitute(A:B,
                                                                                         list(A = rg[[1]], B = rg[[2]])),)))
                        ## TODO: need to remove 'nimbleModel:::' when this is in the package
                        allRules$indexRules[[idx]] <- nimbleModel:::indexRuleClass_block$new(
                                                                                             toIndexExprList = list(t1 = quote(i)),
                                                                                             fromIndexExprList = list(f1 = quote(i)),
                                                                                             context = context_tmp)
                        ## update allRules to have additional indexes
                        
                    }
                    isConstant <- rep(FALSE, length(isConstant))
                }
            }

            ## Treat block constants as internal rules that don't relate to indexing over nodes.
            externalRules <<- allRules
            externalRules$indexRules[isBlockConstant] <<- NULL
            externalRules$indexSets$LHSindex2setID <<-
                externalRules$indexSets$LHSindex2setID[externalRules$indexSets$LHSindex2setID != 0]
            
            internalRules <<- allRules
            internalRules$indexRules[!isBlockConstant] <<- NULL
            internalRules$indexSets$numSets <<- 0
            internalRules$indexSets$LHSindex2setID <<-
                internalRules$indexSets$LHSindex2setID[internalRules$indexSets$LHSindex2setID == 0]
            
            numExternalRules <<- length(externalRules$indexRules)
            numInternalRules <<- length(internalRules$indexRules)

        },

        apply = function(varRange = NULL) {
            if(is.null(varRange))   ## user wants full range for the variable
                varRange <- fullRange
            if(numExternalRules) {
                externalRange <- applyGraphIndexRules(varRange, externalRules)
            } else externalRange <- NULL # varRangeClass$new(list(nimbleModel:::indexRange_empty()))
            if(numInternalRules) {
                internalRange <- applyGraphIndexRules(varRange, internalRules)
            } else internalRange <- NULL # varRangeClass$new(list(nimbleModel:::indexRange_empty()))
            result <- nodeRangeClass$new(varName, externalRange, internalRange, index2setID, self)
            return(result)
        },

        is_type = function(type) {
            switch(type,
                end = return(end),
                top = return(top),
                latent = return(!end && !top),
                RHSonly = (RHSonly),
                stop("Invalid type ", type)
            )
        },

        set = function(type) {
            switch(type,
                end = end <<- TRUE,
                top = top <<- TRUE,
                RHSonly = RHSonly <<- TRUE,
                stop("Invalid type ", type)
            )            
        },

        unset = function(type) {
            switch(type,
                end = end <<- FALSE,
                top = top <<- FALSE,
                RHSonly = RHSonly <<- FALSE,
                stop("Invalid type ", type)
            )
        },

        getFullRange = function() {
            extent <- lapply(seq_along(allRules$indexRules), function(idx)
                allRules$indexRules[[idx]]$get_max())
            maxes <- rep(0, length(index2setID))
            cnt <- 1
            cntConstant <- 0
            constants <- which(index2setID == 0)
            for(i in seq_along(extent)) {
                if(is(allRules$indexRules[[i]], "indexRuleClass_constant")) {
                    cntConstant <- cntConstant + 1
                    maxes[constants[cntConstant]] <- extent[[i]]
                } else {
                    maxes[index2setID == i] <- extent[[i]] 
                }

            }
            
            return(applyGraphIndexRules(
                    varRangeClass$new(lapply(seq_len(numIndices),
                                             function(i) indexRange(
                                                             substitute(1:MAX, list(MAX = maxes[i]))))),
               allRules))
        }
    )
)


## nodeRange

## Holds a collection of like nodes (same declaration, same graph role (e.g., latent, top) and same sort ID (need to think about state space case)

## example: y[i, 1:5] ~ dmnorm() has:
## externalRange: first index, over selected nodes
## internalRange: 1:5

nodeRangeClass <- R6Class(
    classname = "nodeRangeClass",
    portable = FALSE,
    public = list(
        varName = NULL,
        externalRange = NULL,  # a varRange
        internalRange = NULL,  # a varRange
        rule = NULL,
        originalIndexRange = NULL,
        index2setID = NULL,
        indexID_2_rangeID = NULL,    
        rangeID_2_indexID = NULL,

        initialize = function(varName,
                              externalRange,
                              internalRange,
                              index2setID,
                              rule) {
            varName <<- varName
            externalRange <<- externalRange
            internalRange <<- internalRange
            rule <<- rule  ## pointer to governing rule
            index2setID <<- index2setID
browser()
            ## These apply to the combination of the externalRange and internalRange;
            ## internalRules indexRanges are considered to be last.
            indexID_2_rangeID <<- index2setID
            indexID_2_rangeID[indexID_2_rangeID != 0] <<- externalRange$indexID_2_rangeID
            indexID_2_rangeID[indexID_2_rangeID == 0] <<- internalRange$indexID_2_rangeID +
                length(externalRange$indexRanges)
            rangeID_2_indexID <<- lapply(seq_len(max(indexID_2_rangeID)),
                                         function(x) which(indexID_2_rangeID == x))

            originalIndexRange <<- rule$originalIndexRules$apply(self$getVarRange())

        },

        calculate = function() {
            rule$calculate(originalIndexRange)
        },
        
        getVarRange = function() {
            ## Extract varRange (i.e., ignoring node structure) for use with methods that apply
            ## to varRanges.
            varRangeClass$new(indexInfo = c(externalRange$indexRanges, internalRange$indexRanges),
                              indexOrders = rangeID_2_indexID)
            
        },

        getSortID = function() {
            return(rule$sortID)
        },
        
        expandNames = function() {
            ## Expand externalRange into full matrix (crossed if necessary), keeping full internal
            ## indexing for each individual node.
            nc <- length(index2setID)  # might be, e.g., 0 1 0 2 3 or 0 1 0 2 1
            str <- paste0(varName, "[")

            nodeInfo <- lapply(externalRange$indexRanges, function(x) {
                if(identical(attr(x, 'rangeType'), 'sequence'))
                    result <- seq(x[[1]][[1]], x[[1]][[2]]) else result <- x[[1]]
                if(!is.matrix(result)) result <- matrix(result, ncol = 1)
                return(result)
            })
            expanded <- do.call(expand.grid, lapply(nodeInfo, function(x) {
                if(is.matrix(x)) 1:nrow(x) else 1:length(x)
            }))

            internalInfo <- lapply(internalRange$indexRanges, function(x) {
                if(identical(attr(x, 'rangeType'), 'sequence')) return(deparse(substitute(X:Y, list(X = x[[1]][[1]], Y = x[[1]][[2]]))))
                return(x)
            })

            ## Mark column position within indexRanges of the externalRange.
            colID <- as.list(rep(1, length(nodeInfo)))
            ## Mark position within internal- and node-related indexes.
            idxInternal <- 1
            idxNode <- 1
            
            for(i in 1:nc) {
                if(i > 1)
                    str <- paste0(str, ", ")
                if(index2setID[i] == 0) {
                    str <- paste0(str, internalInfo[[idxInternal]])
                    idxInternal <- idxInternal + 1
                } else {
                    rangeIdx <-  externalRange$indexID_2_rangeID[idxNode]  ## which indexRange is being used
                    str <- paste0(str, nodeInfo[[rangeIdx]][expanded[ , rangeIdx], colID[[rangeIdx]]])
                    colID[[rangeIdx]] <- colID[[rangeIdx]] + 1  ## index through columns of the indexRange_matrix 
                    idxNode <- idxNode + 1
                }
            }
            str <- paste0(str, "]")
            return(str)
        }
    )
)


## Takes a RHS rule (created from original RHS of an expression) and intersects it with a LHS rule.
## Result can be:
##  no intersection: RHS passed through
##  RHS is fully in LHS: NULL
##  partly intersects: fracture and return one or more fractured RHS rules

## This cannot be a method of nodeRangeClass because result can be to remove the RHSrule if have complete overlap,
## or split the rule into two rules.

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

            ## Modify RHSrule expr and context to insert vector of relevent values.
            newSingleContexts <- singleContexts[!focalContext]
            newSingleContexts[[length(newSingleContexts)+1]] <- modelSingleContext(
                               indexVarExpr = quote(.newidx),
                indexRangeExpr = substitute(1:L, list(L = length(valsRHS))))

            expr[[nonIdenticalIndices+2]] <- quote(.idx[.newidx])
         
            resultRule <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context = modelContextClass$new(newSingleContexts),
                                            constants = list(.idx = valsRHS)) 
            return(list(resultRule))
        } else {  # seq+seq or seq+scalar
            if(typeInt == "scalar")
                int <- indexRange(substitute(A:A, list(A = int[[1]])))
            ## now process two seqs
            if(typeRHS == "scalar") stop("Not expecting RHS to be a scalar")  ## scalar RHS either fully intersected or not intersected


            
            if(int[[1]][1] == RHS[[1]][[1]] || int[[1]][[2]] == RHS[[1]][[2]]) {
                if(int[[1]][[1]] == RHS[[1]][[1]]) 
                    RHS[[1]][[1]] <- int[[1]][[2]]+1 else RHS[[1]][[2]] <- int[[1]][[1]]-1
                RHSrule$externalRules$indexRules[[RHSrule$index2setID[[nonIdenticalIndices]]]]$modify_extent(RHS[[1]])
                return(list(RHSrule))
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
               
                resultRule1 <- nodeRuleClass$new(expr1, FALSE, 1, FALSE, context = modelContextClass$new(newSingleContexts1))
                resultRule2 <- nodeRuleClass$new(expr2, FALSE, 1, FALSE, context = modelContextClass$new(newSingleContexts2))
                return(list(resultRule1, resultRule2))
            }
        }
    } else { ## not simple setting of a single non-identical scalar index that needs to be considered
        ## unroll, exclude, create new arbitrary RHSrule by creating a complicated context, crossed with any indices that are identical
        unrolledRHS <- RHSrange$getIndexRangeMatrix(nonIdenticalIndices)
        unrolledIntersection <- intersection$getIndexRangeMatrix(nonIdenticalIndices)

        rhsAsChar <- do.call(paste, as.data.frame(unrolledRHS[[1]]))
        intAsChar <- do.call(paste, as.data.frame(unrolledIntersection[[1]]))

        remaining <- which(!rhsAsChar %in% intAsChar)
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
        resultRule <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context = modelContextClass$new(newSingleContexts),
                                        constants = constants)
        ## make sure correctly handle multiple indexes, such as mu[j,idx[j]] that are tied together
        ## look for all all.vars used in relevant indices and take them out. What about mu[j,i+j]?
        return(list(resultRule))
     }
}

## need tests for:
## setting up LHS node rules
## setting up RHS rules
## applying rules to get nodeRanges

if(FALSE) {
    ## Extensive, but not comprehensive, testing of exclude()
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
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    LHS <- quote(mu[3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)
    
    result <- exclude(RHSrule, LHSrule)[[1]]

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:8){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)

    ## scalar/seq overlap no overlap
    RHS <- quote(mu[i+1])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    LHS <- quote(mu[33])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)

    result <- exclude(RHSrule, LHSrule)
    expect_identical(result[[1]], RHSrule)

    ## scalar/seq overlap in middle
    RHS <- quote(mu[i+1])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    LHS <- quote(mu[4])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:2){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:8){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)


    ## seq/seq partial overlap
    RHS <- quote(mu[i+1])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 5:8){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)


    ## seq/seq full overlap
    RHS <- quote(mu[i+1])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:9){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)

    result <- exclude(RHSrule, LHSrule)
    expect_identical(result, NULL)

    
    ## matrix in LHS
    RHS <- quote(mu[i+1])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[idx[i]])
    idx <- c(2,5,4)
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp, constants = list(idx = idx))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    idx <- as.integer(c(3,6,7,8,9))
    expected <- nodeRuleClass$new(LHS, FALSE, 1, FALSE, context_tmp, constants = list(idx = idx))

    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## matrix in RHS
    idx <- c(14,4,2,9,1,3,7,11)
    RHS <- quote(mu[idx[i]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i, constants = list(idx = idx))
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[i])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:4){}))))
    idx <- as.integer(c(4,7,9,11))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp, constants = list(idx = idx))

    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## two-d arbitrary case, extracting block elements from RHS matrix
    RHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i, constants = list(idx1 = idx1, idx2 = idx2))
    LHS <- quote(mu[i,j])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij)

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:5){}))))
    idx1 <- c(11,11,12,13,5)
    idx2 <- c(2,5,6,7,13)
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    

    ## two-d arbitrary case, extracting matrix elements from RHS block
    RHS <- quote(mu[i,j])
    RHSrule <- nodeRuleClass$new(RHS, TRUE, 1, FALSE, context_ij)
    LHS <- quote(mu[idx1[i],idx2[i]])
    idx1 <- c(1,3,5,11,4,11,12,13)
    idx2 <- c(1,2,13,2,1,5,6,7)
    LHSrule <- nodeRuleClass$new(LHS, FALSE, 1, FALSE, context_i, constants = list(idx1 = idx1, idx2 = idx2))

    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:26){}))))
    idx1 <- c(2,3,5,6,7,8,2,4:8,rep(2:8, 2))
    idx2 <- c(rep(1,6),rep(2,6),rep(3,7), rep(4,7))
    expected <- nodeRuleClass$new(LHS, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case with constant
    RHS <- quote(mu[i,1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:8){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case, all excluded
    RHS <- quote(mu[5,1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_0)
    LHS <- quote(mu[i, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
    
    result <- exclude(RHSrule, LHSrule)
    expect_identical(result, NULL)

    ## basic mv node case with seq-seq partial overlap
    RHS <- quote(mu[i,1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:5){}))))
    LHS <- quote(mu[i+1, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:8){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## basic mv node case with shared matrix index
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1,2)
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij, constants = list(idx = idx))
    LHS <- quote(mu[5, idx[j]])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_j, constants = list(idx = idx))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:4){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[2]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    
    ## Awkward intersections

    ## Partial overlap in some rows; for now this is simply unrolled.
    RHS <- quote(mu[i,1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:17){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## LHS element inside RHS; for now this is simply unrolled
    RHS <- quote(mu[i,1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    LHS <- quote(mu[3, 3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:20){}))))
    idx1 <- c(2:8,2:8,2,4:8)
    idx2 <- c(rep(1,7), rep(2,7), rep(3, 6))
    RHS <- quote(mu[idx1[i], idx2[i]])
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)


    ## LHS fully overlaps RHS block constant in additional dimension; this is handled nicely.
    RHS <- quote(mu[i,1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[i, 1:4])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:6){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)

    ## basic 3-d case - two identical indices
    RHS <- quote(mu[1:3, i, 1:2])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){}))))
    LHS <- quote(mu[1:3, i, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)
 
    result <- exclude(RHSrule, LHSrule)
    
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:6){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[3]]$setupResults,
                     RHSrule$externalRules$indexRules[[3]]$setupResults)
    expect_identical(result[[1]]$index2setID, c(2,1,3))

    ## 3-d case with partial overlap in some rows but with additional identical index (j)
    RHS <- quote(mu[i, j, 1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 7:9){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
                                              
    LHS <- quote(mu[i, j, 1:2])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:17){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    idx1 <- c(2:6,2:6,2:8)
    idx2 <- c(rep(1,5), rep(2,5), rep(3, 7))
    RHS <- quote(mu[idx1[i], j, idx2[i]])
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)

    ## 3-d case with multi-column shared matrix indexRange
    idx1 <- c(4,7,1,2)
    idx2 <- c(1,9,3,2)
    
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    LHS <- quote(mu[idx1[j], 2, idx2[j]])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_j, constants = list(idx1 = idx1, idx2 = idx2))
    

    ## is warning about crossing correct?
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    
    ## HERE
    ## 3-d case with multi-column unshared matrix indexRange
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij, constants )
                                              
    idx1 <- c(4,7,2,2)
    idx2 <- c(1,10,3,2)
    LHS <- quote(mu[idx3[j], i, idx4[j]])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij)
    
    result <- exclude(RHSrule, LHSrule)

    ## HERE
    ## two blocks none shared
    ## shared multi-col indexrange matrix  mu[(i,j),k]
    ## unshared multi-col indexrange_matrx mu[i,j,k] where (i,k) are in an indexrange
    ## 4) case whre one or more shared and complicated non-shared, including mv indexRange (mostly deal with in mu[i,j], mu[idx1[i],idx2[i]] case?
    ## 5) these cases:
    

    ## is this ok?
    idx <- c(4,7,1,3)
    RHS <- quote(mu[2,i,3,idx[i]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_j, constants = list(idx = idx))
    LHS <- quote(mu[2,1,3,idx[i]])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_j, constants = list(idx = idx))
    result <- exclude(RHSrule, LHSrule)

    idx <- c(4,7,1)
    RHS <- quote(mu[2,i,3,idx[i]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i_short, constants = list(idx = idx))
    LHS <- quote(mu[2,1,3,idx[i]])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i_short, constants = list(idx = idx))
    debugonce(exclude)
    result <- exclude(RHSrule, LHSrule)

    ## TODO: presumably take out ability to pass matrix to indexRule_arbitrary

    ## TODO: presumably remove modify_extent
    ## TODO: error trap if length of constant is not correct; check if this happens with regular graph rules too

    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1)
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij, constants = list(idx = idx))

    ## TODO: error trap missing constants
    RHS <- quote(mu[idx[i]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
}

if(FALSE) {
    ## Checking getFullRange
    LHS <- quote(mu[5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)
    expect_equal(LHSrule$getFullRange(),
                     varRangeClass$new(list(indexRange(5), indexRange(quote(1:3)))))

    LHS <- quote(mu[4:5, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)
    expect_equal(LHSrule$getFullRange(),
                     varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:3)))))
    
    LHS <- quote(mu[4:5, i, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(2:8)), indexRange(quote(1:3)))))
    
    expr <- quote(mu[4:5, j, i, 3])
    LHSrule <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_ij)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:4)),
                                        indexRange(quote(2:8)), indexRange(quote(3)))))
    RHSrule <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context_ij)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(quote(1:4)),
                                        indexRange(quote(2:8)), indexRange(quote(3)))))

    LHS <- quote(mu[4:5, i, i, 3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
    expect_equal(LHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(4:5)), indexRange(matrix(rep(2:8, 2), ncol = 2)), indexRange(3))))
    
    expr <- quote(mu[j, 1:3, i, 2])
    RHSrule <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context_ij)
    LHSrule <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_ij)

    expect_equal(RHSrule$getFullRange(),
                 varRangeClass$new(list(indexRange(quote(1:4)), indexRange(quote(1:3)),
                                        indexRange(quote(2:8)), indexRange(2))))

}

## Remaining disorganized code for doing haphazard checking while developing nodeRules/ranges. 

if(F) {

    
        RHS <- quote(mu[i])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij)

        
        RHS <- quote(mu[i,idx1[j],idx2[j],3, 1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij,constants = list(idx1=1:4,idx2=1:4))

   singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:4){}))
    
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))
   rules <- makeGraphIndexRules(LHS = quote(y[i+2]),
                                 RHS = quote(x[i+5]),
                                 context = context_i)

       
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))
       k1=c(4,7,1)
    k2=c(99,1,3)
 
   rules <- makeGraphIndexRules(LHS = quote(y[k1[i]]),
                                 RHS = quote(x[k2[i]]),
                                context = context_i, constants = list(k1=k1,k2=k2))
    

    
   singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))

    
   k1=c(4,1,7)
k2=c(99,1,3)
                             constants = list(k1=k1,k2=k2)
    LHS <- quote(mu[k1[i]+2])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i,constants=list(k1=k1))

## fails
       rules <- makeGraphIndexRules(LHS = quote(y[k1[i]+2]),
                                 RHS = quote(x[i]),
                                 context = context_i, constants =list(k1=k1))

      shuffle <- as.integer(c(5, 4, 1)) # , 9, 10, 8, 6, 2, 7, 3))
    makeGraphIndexRules(quote(y[shuffle[i] + 3]),
                            quote(x[i]),
                            context_i,
                            constants = list(shuffle = shuffle))


      makeGraphIndexRules(quote(y[i*3]),
                            quote(x[i]),
                            context_i,
                            constants = list(shuffle = shuffle))


    LHS <- quote(mu[j,k,i])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ijnik,constants=list(k1=k1))

    
LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:100)))))
## try mu[k1[i]+2] - doesn't seem to work - compare to shuffle[i]+2 in test-indexRules_arbitrayr


    n <- c(1,7,2)
   LHS <- quote(mu[k,j,i])
    LHSrule = nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ijnik_short,constants=list(n=n))
    
    n <- c(1,7,2)
   LHS <- quote(mu[k,j,i])
    LHSrule = nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ijk,constants=list(n=n))

    rules <- makeGraphIndexRules(LHS = quote(y[k,j,i]),
                             RHS = quote(x[k,j,i]),
                             context = context_ijnik_short,
                             constants = list(n = n))

    }

if(F) {
singleContext1 <-
    modelSingleContext(forCode = quote(for(idx in 1:2){}))
singleContext2 <-
    modelSingleContext(indexVarExpr = quote(j1),
                       indexRangeExpr = quote(idx1[idx]),
                       )
singleContext3 <-
    modelSingleContext(indexVarExpr = quote(j2),
                       indexRangeExpr = quote(idx2[idx]),
                       )
singleContext4 <-
    modelSingleContext(indexVarExpr = quote(j3),
                       indexRangeExpr = quote(idx3[idx]),
                       )
idx1=result[,1]
idx2=result[,2]
idx3=result[,3]

context <- modelContextClass$new(list(singleContext2,singleContext3,singleContext4,singleContext1))

   rules <- makeGraphIndexRules(LHS = quote(y[j1,j2,j3,idx]),
                                 RHS = quote(x[j1,j2,j3,idx]),
                                context = context,
                                constants = list(idx1=idx1,idx2=idx2,idx3=idx3))


}

## fracture()


## code for setting up the various lists and processing down and up

if(FALSE) {
   rules <- makeGraphIndexRules(LHS = quote(x[i,j]),
                                 RHS = quote(y[i,j,3]),
                                context = context_ijni_short,
                                constants = list(n = n))
   expect_identical(length(rules$indexRules), 1L)
   expect_true(is(rules$indexRules[[1]], "indexRuleClass_arbitrary"))

   ## NOTE: this case has redundant results.
   expect_equal(
       applyGraphIndexRules(
           varRangeClass$new(list(indexRange(quote(2:3)),
                                  indexRange(matrix(c(3,2,2,3,3,4), ncol = 2)))), rules),
       varRangeClass$new(list(indexRange(matrix(c(3,2,2), ncol = 1))))
   )


   
   rules <- makeGraphIndexRules(LHS = quote(x[i]),
                                 RHS = quote(y[i]),
                                context = context_i)


   LHS <- quote(y[j, i+1, 5:9])
   nodeRule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij)
   ## This works nicely - originalIndexRange is i=2:4,j=1:2
   nodeRule$apply(varRangeClass$new(list(indexRange(quote(1:2)), indexRange(quote(3:5)), indexRange(6))))

   singleContext1 <-
       modelSingleContext(forCode = quote(for(i in 2:5){}))
   context_i <- modelContextClass$new(list(singleContext1))

   RHS <- quote(mu[i+1])
   nodeRule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
   
 }
 
