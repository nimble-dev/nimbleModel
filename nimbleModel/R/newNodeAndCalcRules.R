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
        externalRules = NULL, # indexing for the nodes
        internalRules = NULL, # indexing for components, if multivariate nodes
        numExternalRules = numeric(0),
        numInternalRules = numeric(0),
        index2setID = NULL,
        originalIndexRules = NULL, # determines original indexing (based on context); set equal to originalRule$originalIndexRule if not canonical nodeRule
        stochParent = FALSE,
        stochDep = FALSE,
        touchedDown = FALSE,
        touchedUp = FALSE,
        edgesFrom = numeric(),

        fullRange = NULL,
        
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

            originalIndexRules <<- originalIndexRuleClass$new(expr, context, constants)

            ## Note: this is awkward to go into the data structures and modify them

            ## TODO: modify allRules to be a graphRule
            allRules <- makeGraphIndexRules(expr, expr, context)
            index2setID <<- allRules$indexSets$LHSindex2setID
            isConstant <- sapply(allRules$indexRules, is, "indexRuleClass_constant")
            numIndices <<- length(allRules$indexSets$LHSindex2setID)

            fullRange <<- allRules$getFullRange()
            
            #fullRange <<- applyGraphIndexRules(
            #    varRangeClass$new(lapply(seq_len(numIndices),
            #                             function(i) indexRange(quote(1:Inf)))), allRules)

            if(RHSonly && any(isConstant)) {  # convert constant rules to block rules, as notion of a multivariate RHS is not useful
                wh <- which(isConstant)
                for(idx in wh) {
                    rg <- allRules$indexRules[[wh]]$setupResults$constant[[1]]
                    context <- modelContextClass$new(list(modelSingleContext(
                                                     indexVarExpr = quote(i),
                                                     indexRangeExpr = substitute(A:B,
                                                                                 list(A = rg[[1]], B = rg[[2]])),)))
                    ## TODO: need to remove 'nimbleModel:::' when this is in the package
                    allRules$indexRules[[idx]] <- nimbleModel:::indexRuleClass_block$new(
                                                      toIndexExprList = list(t1 = quote(i)),
                                                      fromIndexExprList = list(f1 = quote(i)),
                                                      context = context)
                }
                isConstant <- rep(FALSE, length(isConstant))
            }
            
            externalRules <<- allRules
            externalRules$indexRules[isConstant] <<- NULL
            externalRules$indexSets$LHSindex2setID <<-
                externalRules$indexSets$LHSindex2setID[externalRules$indexSets$LHSindex2setID != 0]
            
            internalRules <<- allRules
            internalRules$indexRules[!isConstant] <<- NULL
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

            originalIndexRange <<- rule$originalIndexRules$apply(self$getVarRange())

            ## These apply to the combination of the externalRange and internalRange;
            ## internalRules indexRanges are considered to be last.
            indexID_2_rangeID <<- index2setID
            indexID_2_rangeID[indexID_2_rangeID != 0] <<- externalRange$indexID_2_rangeID
            indexID_2_rangeID[indexID_2_rangeID == 0] <<- internalRange$indexID_2_rangeID +
                length(externalRange$indexRanges)
            rangeID_2_indexID <<- lapply(seq_len(max(indexID_2_rangeID)),
                                         function(x) which(indexID_2_rangeID == x))

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

exclude <- function(RHSrule, LHSrule) {
    LHSrange <- LHSrule$fullRange
    RHSrange <- RHSrule$fullRange
    intersection <- RHSrule$apply(LHSrange)$getVarRange()
    if(varRange_isEmpty(intersection))
        return(list(RHSrule))
    if(varRange_isEqual(RHSrange, intersection)) 
        return(NULL)
    ## otherwise need to fracture the RHSrule
    ## if intersect, need new IDs?
    identicalRanges <- sapply(seq_along(RHSrange$indexRanges), function(idx)
        identical(RHSrange$indexRanges[[idx]], intersection$indexRanges[[idx]]))
    
    clean <- RHSrule$numIndices == 1 || sum(identicalRanges) == RHSrule$numIndices-1

    if(clean) {  ## split, shrink, or remove from focal index, and combine with other indices
        idx <- which(!identicalRanges)

        RHS <- RHSrange$indexRanges[[idx]]
        int <- intersection$indexRanges[[idx]]
        typeRHS <- attr(RHS, "rangeType")
        typeInt <- attr(int, "rangeType")

        if(typeRHS == "arbitrary" || typeInt == "arbitrary") {
            valsRHS <- switch(typeRHS,
                              arbitrary = RHS[[1]],
                              scalar = RHS[[1]],
                              seq = RHS[[1]][1]:RHS[[1]][2]
                              )
            valsInt <- switch(typeInt,
                              arbitrary = int[[1]],
                              scalar = int[[1]],
                              seq = int[[1]][1]:int[[1]][2]
                              )
            valsRHS <- valsRHS[!valsRHS %in% valsInt]
            ## RHS <- indexRange(matrix(valsRHS))
            RHSrule$externalRules$indexRules[[idx]] <- nimbleModel:::indexRule_arbitrary_setup(NULL, NULL, NULL,
                                   matrix = matrix(valsRHS))
        } else {  # seq+seq or seq+scalar
            if(typeInt == "scalar")
                int <- indexRange(substitute(A:A, list(A = int[[1]])))
            ## now process two seqs
            if(typeRHS == "scalar") stop("Not expecting RHS to be a scalar")  ## scalar RHS either fully intersected or not intersected

            if(int[[1]][1] == RHS[[1]][[1]] || int[[1]][[2]] == RHS[[1]][[2]]) {
                if(int[[1]][[1]] == RHS[[1]][[1]]) 
                    RHS[[1]][[1]] <- int[[1]][[2]]+1 else RHS[[1]][[2]] <- int[[1]][[1]]-1
                RHSrule$externalRules$indexRules[[idx]]$modify_extent(RHS[[1]])
                return(list(RHSrule))
            } else {
                RHSrule2 <- RHSrule$clone(deep = TRUE)
                ## Awkward - somehow the externalRules$indexRules are still shallow copies, perhaps because we have R6 within list within R6?
                RHSrule2$externalRules$indexRules <- lapply(RHSrule2$externalRules$indexRules, function(x) x$clone(deep = TRUE))

                RHS2 <- RHS
                RHS[[1]][[2]] <- int[[1]][[1]]-1
                RHS2[[1]][[1]] <- int[[1]][[2]]+1
                RHSrule$externalRules$indexRules[[idx]]$modify_extent(RHS[[1]])
                RHSrule2$externalRules$indexRules[[idx]]$modify_extent(RHS2[[1]])
                return(list(RHSrule, RHSrule2))
            }
        }
    } else {  ## unroll, exclude, create new arbitrary RHSrule by creating a complicated context
        unrolledRHS <- RHSrange$getIndexRangeMatrix(seq_len(numIndices))
        unrolledIntersection <- intersection$getIndexRangeMatrix(seq_len(numIndices))

        unrolledRHS <- indexRange(matrix(c(1,2,3,1,2,4,3,4,7,1,4,6), ncol = 3, byrow =TRUE))
        unrolledIntersection <- indexRange(matrix(c(1,2,3,3,4,7), ncol = 3, byrow =TRUE))
        
        rhsAsChar <- do.call(paste, as.data.frame(unrolledRHS[[1]]))
        intAsChar <- do.call(paste, as.data.frame(unrolledIntersection[[1]]))

        remaining <- which(!rhsAsChar %in% intAsChar)
        ## Clunky to call indexRule_arbitrary_setup directly...
        ## from_flatMax and length(from_flat2iRow) will be size of (hyper)cube
        ## encompassing all the remaining elements
        ## TODO: remove 'nimbleModel:::'
        RHSrule$externalRules <- nimbleModel:::indexRule_arbitrary_setup(NULL, NULL, NULL,
                                   matrix = unrolledRHS[[1]][remaining, ])
        return(list(RHSrule))
     }
}

## need tests for:
## setting up LHS node rules
## setting up RHS rules
## applying rules to get nodeRanges
## using exclude()

if(F) {

    library(nimbleModel)
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 2:8){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:4){}))
    
    context_0 <- modelContextClass$new()
    context_i <- modelContextClass$new(list(singleContext1))
    
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
    ## what to compare?

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

## how deal with this to address next issue?
k1=c(4,7,1)
k2=c(99,1,3)
                             constants = list(k1=k1,k2=k2)
    RHS <- quote(mu[k1[i],j,k2[i]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij_short)
## need getMax for each indexRule type
    
    ## matrix in LHS - failing when create LHS rule because can't get fullRange with 1:inf
    RHS <- quote(mu[i+1])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
    LHS <- quote(mu[idx[i]])
    idx <- c(400,7,8)
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp, constants = list(idx = idx))

    result <- exclude(RHSrule, LHSrule)

    ## matrix in RHS
    
    ## check 2-d case
    ## y[i, 1:3] <- mu[i+1, 1:3]
    RHS <- quote(mu[i+1,1:3])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i)
    ## mu[3, 1:3]  # also do mu[4,1:3] to fracture, mu[i,1:3] for i in 3:4 and do mu[c(3,5),1:3] via k[i]?
    LHS <- quote(mu[1, 1:3])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_0)
    
    debug(exclude)
    exclude(RHSrule, LHSrule)

    ## test mu[i,j] going from 2 seq rules to a single matrix rule if intersect in awkward way

    ## various other cases such as mu[i, 1:3] with awkward intersection
    
    
    
    
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
 
