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
        originalRule = NULL, # pointer to canonical nodeRule from declaration (possibly `self`
        externalRules = NULL, # indexing for the nodes
        internalRules = NULL, # indexing for components, if multivariate nodes
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

        ## do we want canonicalRange (either at var or node level)?

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
            allRules <- makeGraphIndexRules(expr, expr, context)
            index2setID <<- allRules$indexSets$LHSindex2setID
            isConstant <- sapply(allRules$indexRules, is, "indexRuleClass_constant")
            num_indices <<- length(allRules$indexSets$LHSindex2setID)

            canonicalRange <<- applyGraphIndexRules(
                varRangeClass$new(lapply(seq_len(num_indices),
                                         function(i) indexRange(quote(1:Inf)))), allRules)

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
                varRange <- canonicalRange
            if(numExternalRules) {
                externalRange <- applyGraphIndexRules(varRange, externalRules)
            } else externalRange <- varRangeClass$new(list(nimbleModel:::indexRange_empty()))
            if(numInternalRules) {
                internalRange <- applyGraphIndexRules(varRange, internalRules)
            } else internalRange <- varRangeClass$new(list(nimbleModel:::indexRange_empty()))
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


## exclude()

## Takes a RHS rule (created from original RHS of an expression) and
## intersects it with a LHS rule.
## Result can be:
##  no intersection: RHS passed through
##  RHS is fully in LHS: NULL
##  partly intersects: fracture and return one or more fractured RHS rules

exclude <- function(RHSrule, LHSrule) {
    LHSrange <- LHSrule$canonicalRange
    RHSrange <- RHSrule$canonicalRange
    intersection <- RHSrule$apply(LHSrange)
    if(varRange_isEmpty(intersection))
        return(list(RHSrule))
    if(varRange_isEqual(RHSrange, intersection)) 
        return(NULL)
    ## otherwise need to fracture the RHSrule
    ## if intersect, need new IDs?
    identicalRanges <- sapply(seq_along(RHSrange$indexRanges), function(idx)
        identical(RHSrange$indexRanges[[idx]], intersection$indexRanges[[idx]]))
    
    clean <- RHSrule$num_indices == 1 || sum(identicalRanges) == RHSrule$num_indices-1

    if(clean) {  ## split, shrink, or remove from focal index, and combine with other indices
        idx <- which(!identicalRanges)

        RHS <- RHSrange$indexRanges[[idx]]
        int <- intersection$indexRanges[[idx]]
        typeRHS <- attr(RHS, "rangeType")
        typeInt <- attr(int, "rangeType")

        ## make temp copy:
        ## RHSir <- RHSrange[[idx]]
        if(typeRHS == "arbitrary" || typeInt == "arbitrary") {
            valsRHS <- switch(typeRHS,
                              arbitrary = RHS[[1]]
                              scalar = RHS[[1]]
                              seq = RHS[[1]][1]:RHS[[1]][2]
                              )
            valsInt <- switch(typeInt,
                              arbitrary = int[[1]]
                              scalar = int[[1]]
                              seq = int[[1]][1]:int[[1]][2]
                              )
            valsRHS <- valsRHS[!valsRHS %in% valsInt]
            RHS <- indexRange(matrix(valsRHS))
            RHSrule$indexRules[[idx]] <- nimbleModel:::indexRule_arbitrary_setup(NULL, NULL, NULL,
                                   matrix = matrix(valsRHS))
        } else {  # seq+seq or seq+scalar
            if(typeInt == "scalar")
                int <- indexRange(substitute(A:A, list(A = int[[1]])))
            ## now process two seqs
            if(typeRHS == "scalar") stop("Not expecting RHS to be a scalar")

            ## check if using ref semantics
            if(int[[1]][1] == RHS[[1]][1] || int[[1]][2] == RHS[[1]][2]) {
                if(int[[1]][1] == RHS[[1]][1]) 
                    RHS[[1]][1] <- int[[1]][2]+1 else RHS[[1]][2] <- int[[1]][1]-1
                RHSrange$indexRanges[[idx]] <- RHS
                RHSrule$indexRules[[idx]] <- 7  ## HERE - need code to create block rule from seq range
            } else {
                RHSrule2 <- RHSrule$clone()
                RHS2 <- RHS
                RHS[[1]][2] <- int[[1]][1]-1
                RHS2[[1]][1] <- int[[1]][2]+1
                RHSrule$indexRules[[idx]] <- nimbleModel:::indexRule_block_setup(RHS[[1]])  ## HERE - need code to create block rule from seq range
                RHSrule2$indexRules[[idx]] <- nimbleModel:::indexRule_block_setup(RHS2[[1]])  ## HERE - need code to create block rule from seq range
               
            }
        }
        
        ## how create indexRule from range?
        ## insert indexRule into RHSrule
        return(list(RHSrule, RHSrule2))
        ## create rule(s) based on modified index and remaining indices
    } else {  ## unroll, exclude, create new arbitrary RHSrule by creating a complicated context
        unrolledRHS <- RHSrange$getIndexRangeMatrix(seq_len(num_indices))
        unrolledIntersection <- intersection$getIndexRangeMatrix(seq_len(num_indices))

        unrolledRHS <- indexRange(matrix(c(1,2,3,1,2,4,3,4,7,1,4,6), ncol = 3, byrow =TRUE))
        unrolledIntersection <- indexRange(matrix(c(1,2,3,3,4,7), ncol = 3, byrow =TRUE))
        
        rhsAsChar <- do.call(paste, as.data.frame(unrolledRHS[[1]]))
        intAsChar <- do.call(paste, as.data.frame(unrolledIntersection[[1]]))

        remaining <- which(!rhsAsChar %in% intAsChar)
        ## Clunky to call indexRule_arbitrary_setup directly...
        ## from_flatMax and length(from_flat2iRow) will be size of (hyper)cube
        ## encompassing all the remaining elements
        ## TODO: remove 'nimbleModel:::'
        rules <- nimbleModel:::indexRule_arbitrary_setup(NULL, NULL, NULL,
                                   matrix = unrolledRHS[[1]][remaining, ])
        return(list(rules))
     }
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



a <- matrix(T,3,3)
a[2,2] <- F
apply(a, 1, function(x) which(x))

a <- matrix(T,3,3)
a[2,2] <- F
a[1,] <- F
apply(a, 1, function(x) which(x))
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
   
}
