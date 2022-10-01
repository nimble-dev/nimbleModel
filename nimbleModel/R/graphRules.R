## To make this more object-oriented, we probably want the elements of
## the indexRules list to be elements of the graphRuleClass,
## with makeGraphRule a method that populates those.

graphRuleClass <- R6Class(
    classname = "graphRuleClass",
    portable = FALSE,
    public = list(
        indexRules = NULL,
        indexSets = NULL,
        constraints = NULL,
        numRHSindices = NULL,
        parentVar = NULL,
        childVar = NULL,
        stoch = logical(),
        initialize = function(LHS,
                              RHS,
                              context,
                              constants = list(),
                              stoch = NULL) {
            ## This is a hack for the moment, as makeGraphRule
            ## needs to be a method, not a stand-alone function.
            stoch <<- stoch
            output <- 
                makeGraphRule(LHS,
                              RHS,
                              context,
                              constants)
            indexRules <<- output$indexRules
            indexSets <<- output$indexSets
            constraints <<- output$constraints
            numRHSindices <<- output$numRHSindices
            parentVar <<- output$parentVar
            childVar <<- output$childVar
        },
        apply = function(fromVarRange) {
            varName <- getVarName(fromVarRange)
            if(!is.null(parentVar) && varName != parentVar)
                return(NULL)
            if(is.character(fromVarRange)) {
                if(varName == parentVar && numRHSindices) {
                    fromVarRange <- getFromRange()   # only varName given
                } else fromVarRange <- varRangeClass$new(fromVarRange)   # string providing the varRange         
            }
            applyGraphRule(
                fromVarRange,
                self
            )
        },

        getFromRange = function() {
            if(!length(indexSets$RHSindex2setID)) { ## no indexing
                vr <- varRangeClass$new(parentVar)
            } else {
                maxes <- indexSets$RHSindex2setID
                indexed <- maxes != 0
                
                ## deal with RHS constants
                if(sum(!indexed))
                    maxes[!indexed] <- sapply(constraints, function(x) max(x$constraint))

                if(sum(indexed)) 
                    for(idx in unique(maxes[indexed])) 
                        maxes[indexSets$RHSindex2setID == idx] <- indexRules[[idx]]$get_max()

                vr <- varRangeClass$new(lapply(seq_along(maxes),
                                             function(i) indexRange(
                                                             substitute(1:MAX, list(MAX = maxes[i])))),
                                     varName = parentVar) 
            }
            return(vr)
            ## extent <- lapply(indexRules, function(rule) rule$get_max())

            ## ## Case of no indexing, e.g. x ~ dnorm(0,1)
            ## if(!length(extent[[1]]))
            ##     return(varRangeClass$new(list(indexRange_none()), varName = childVar, stoch = stoch))
            
            ## maxes <- rep(0, length(indexSets$RHSindex2setID))
            ## cnt <- 1
            ## cntConstant <- 0
            ## constants <- which(indexSets$RHSindex2setID == 0)
            ## for(i in seq_along(extent)) {
            ##     if(is(indexRules[[i]], "indexRuleClass_constant")) {
            ##         cntConstant <- cntConstant + 1
            ##         maxes[constants[cntConstant]] <- extent[[i]]
            ##     } else {
            ##         maxes[indexSets$RHSindex2setID == i] <- extent[[i]] 
            ##     }
            ## }
        })
)


## This file has code for managing the set of indexRules (i.e., a graphRule) for a variable.
##
## This first function here separates sets of index relationships
## into separable sets.
##
## See test-graphRules.R

## LHS is an expr like quote(y[i, j])
## RHS is an expr like quote(x[i, j])
## context is a modelContextClass object
##
makeSeparableIndexSets <- function(LHS,
                                   RHS,
                                   context) {
    if(length(context$singleContexts)) {
        indexVarNames <- structure(context$indexVarNames,
                                   names = context$indexVarNames)
    } else indexVarNames <- character(0)
    numIndexVars <- length(indexVarNames)

    if(length(LHS) < 3) {  # no LHS indexing
        LHSindexExprs <- NULL
        LHSnDim <- 0
    } else {
        LHSindexExprs <- as.list(LHS[-c(1,2)])
        LHSnDim <- length(LHSindexExprs)
    }
    
    if(length(RHS) < 3) {  # no RHS indexing
        RHSindexExprs <- NULL
        RHSnDim <- 0
    } else {
        RHSindexExprs <- as.list(RHS[-c(1,2)])
        RHSnDim <- length(RHSindexExprs)
    }

    ## Keep track of which index var definitions use another index (e.g. j in 1:n[i])
    contextExprsVars <- lapply(seq_along(context$singleContexts),
                               function(idx) all.vars(context$singleContexts[[idx]]$indexRangeExpr))
        
    varUsedinContextExprs <- lapply(indexVarNames, function(nm) {
        indexVarNames[sapply(contextExprsVars, function(usedVars) nm %in% usedVars)] })
        
    make_BoolIndexVarList <-
        function(indexExpr)
            structure(
                indexVarNames %in% all.vars(indexExpr),
                names = indexVarNames
            )
    LHSboolIndexVarList <- lapply(LHSindexExprs,
                                   make_BoolIndexVarList)
    RHSboolIndexVarList <- lapply(RHSindexExprs,
                                   make_BoolIndexVarList)
    indexVar2setID <- structure(vector('list', length = numIndexVars),
                                names = indexVarNames)
    LHSindex2setID <- integer(length = LHSnDim)
    RHSindex2setID <- integer(length = RHSnDim)
    indexVarNameSets <- list()
    currentSetID <- 0
    remainingIndexVarNames <- indexVarNames
    RHSonly <- NULL
    
    while(length(remainingIndexVarNames)) {
        currentSetID <- currentSetID + 1
        done <- FALSE
        currentIndexVarNames <- remainingIndexVarNames[1]
        while(!done) {
            LHSboolUsesCurrentIndexVars <- unlist(
                lapply(LHSboolIndexVarList,
                       function(x) any(x[currentIndexVarNames])))
            RHSboolUsesCurrentIndexVars <- unlist(
                lapply(RHSboolIndexVarList,
                       function(x) any(x[currentIndexVarNames])))
            ## ditto
            LHSadditionalIndexVars <- unique(unlist(lapply(
                LHSboolIndexVarList[LHSboolUsesCurrentIndexVars],
                function(x) indexVarNames[x])
                ))
            
            RHSadditionalIndexVars <- unique(unlist(lapply(
                RHSboolIndexVarList[RHSboolUsesCurrentIndexVars],
                function(x) indexVarNames[x])
                ))

            ## Add in vars where the current index is used in their context expression.
            ## This should cause any indices using i or j to be in the same set,
            ## which deals with ragged indexing such as for(i in 1:m) for(j in 1:n[i])

            contextAdditionalIndexVars <- unlist(varUsedinContextExprs[currentIndexVarNames])
            
            allAdditionalIndexVarNames <- setdiff(unique(c(LHSadditionalIndexVars,
                                                           RHSadditionalIndexVars,
                                                           contextAdditionalIndexVars)),
                                                  currentIndexVarNames)
            if(!length(allAdditionalIndexVarNames)) {
                done <- TRUE
            } else {
                currentIndexVarNames <- c(currentIndexVarNames,
                                          allAdditionalIndexVarNames)
            }

            ## Case of index on RHS and not LHS (from getParents cases):
            ## record that this rule is a RHSonly constraint rule.
            if(done && !sum(unlist(
                lapply(LHSboolIndexVarList,
                       function(x) any(x[currentIndexVarNames]))))) 
                RHSonly[currentSetID] <- TRUE
        }
        
        ## recording them this way sorts them:
        indexVarNameSets[[currentSetID]] <- indexVarNames[currentIndexVarNames]
        indexVar2setID[currentIndexVarNames] <- currentSetID
        LHSindex2setID[LHSboolUsesCurrentIndexVars] <- currentSetID
        RHSindex2setID[RHSboolUsesCurrentIndexVars] <- currentSetID
        remainingIndexVarNames <- setdiff(remainingIndexVarNames,
                                          currentIndexVarNames)
    }

    if(is.null(RHSonly)) {
        RHSonly <- rep(FALSE, currentSetID)
    } else {
        tmp <- rep(FALSE, currentSetID)
        tmp[which(RHSonly)] <- TRUE
        RHSonly <- tmp
    }

    return(list(LHSindex2setID = LHSindex2setID,
         RHSindex2setID = RHSindex2setID,
         indexVar2setID = indexVar2setID,
         numSets = currentSetID,
         indexVarNameSets = indexVarNameSets,
         RHSonly = RHSonly))
}


makeConstraints <- function(RHSindexExprs, constrainedBool) {
    constraints <- list()
    cnt <- 0
    for(idx in seq_along(RHSindexExprs)) {
        if(constrainedBool[idx]) {
            cnt <- cnt + 1
            constraints[[cnt]] <- list(RHSindex = idx)
            if(is.name(RHSindexExprs[[idx]]) && RHSindexExprs[[idx]] == "") {
                constraints[[cnt]]$constraint <- c(1, Inf)  ## x[] case, hopefully will fill in bounds based on var dimensions later.
            } else {
                expr <- RHSindexExprs[[idx]]
                if(is.numeric(expr)) {   ## scalar case, e.g., x[2]
                    constraints[[cnt]]$constraint <- rep(expr, 2)
                } else if(is.call(expr) && expr[[1]] == ":") {   ## sequence case, e.g., x[2:4]
                    constraints[[cnt]]$constraint <- c(expr[[2]], expr[[3]])
                } else stop("makeConstraints: Unexpected RHS expression provided.")            
            }
        }
    }
    return(constraints)
}

checkForVars <- function(LHS, RHS, context, constants) {
    varsInExpr <- NULL
    if(length(RHS) > 1)
        varsInExpr <- c(varsInExpr, all.vars(RHS[2:length(RHS)]))
    if(length(LHS) > 1)
        varsInExpr <- c(varsInExpr, all.vars(LHS[2:length(LHS)]))
    wh <- which(!varsInExpr %in% c(names(constants), context$indexVarNames))
    if(length(wh))
        stop("Index or constant ", paste(unique(varsInExpr[wh]), collapse = ','), " not found as loop index or in constants.")
}


modifyContextForRHSonlyRules <- function(LHS, RHS, context, constants) {
    if(identical(LHS, RHS)) {
        varsInExpr <- NULL
        if(length(RHS) > 1)
            varsInExpr <- all.vars(RHS[2:length(RHS)]) 
        indexVarsInExpr <- varsInExpr[!varsInExpr %in% constants]
        context <- modelContextClass$new(context$singleContexts[names(context$singleContexts) %in% indexVarsInExpr])
    }
    return(context)
}
    
## The following functions may be used from class methods in the future.
## For now they are standalone for development and debugging.

## Probably want stoch vs. det information, so need to have declRule as an additional argument
makeGraphRule <- function(LHS,
                          RHS,
                          context,
                          constants = list()) {
    constantsEnv <- if(is.environment(constants))
                        constants
                    else
                        list2env(constants)

    checkForVars(LHS, RHS, context, constants)

    ## rhsOnlyRules can involve fewer single contexts than the full declaration (e.g., mu[i] <- tau)
    ## Need to remove unneeded contexts or the indexSets won't be correct.
    context <- modifyContextForRHSonlyRules(LHS, RHS, context, constants)
    
    parentVar <- deparse(ifelse(length(RHS) > 1, RHS[[2]], RHS))
    childVar <- deparse(ifelse(length(LHS) > 1, LHS[[2]], LHS))
    
    indexSets <-
        makeSeparableIndexSets(LHS, RHS, context)

    if(length(LHS) >= 3 && LHS[[1]] == '[') {
        LHSindexExprs <- as.list(LHS[-c(1,2)])
    } else if(length(LHS) == 1) LHSindexExprs <- list() else
        stop("makeGraphRule: 'LHS' should be an index expression or variable name")

    if(length(RHS) >= 3 && RHS[[1]] == '[') {
        RHSindexExprs <- as.list(RHS[-c(1,2)])
    } else if(length(RHS) == 1) RHSindexExprs <- list() else
        stop("makeGraphRule: 'RHS' should be an index expression or variable name")

    ## Constraints from RHS fixed index values
    constraints <- list()
    RHSindicesBool <- indexSets$RHSindex2setID == 0
    if(any(RHSindicesBool)) 
        constraints <- makeConstraints(RHSindexExprs, RHSindicesBool)

    numRHSindices <- length(RHS) - 2
    if(numRHSindices == -1)
        numRHSindices <- 0
    if(numRHSindices < 0) stop("Unable to determine number of indices in ", RHS)
    
    numSets <- indexSets$numSets
    indexRules <- list()

    for(iSet in seq_len(numSets)) {
        LHSindicesBool <- indexSets$LHSindex2setID == iSet
        RHSindicesBool <- indexSets$RHSindex2setID == iSet
        numLHSbool <- sum(LHSindicesBool)
        thisLHSindexExprs <- structure(
            LHSindexExprs[LHSindicesBool],
            names = if(numLHSbool)
                        paste0("t", seq_len(sum(LHSindicesBool)))
                    else character(0)
        )
        numRHSbool <- sum(RHSindicesBool)
        thisRHSindexExprs <- structure(
            RHSindexExprs[RHSindicesBool],
            names = if(numRHSbool)
                        paste0("f", seq_len(numRHSbool))
                    else character(0)
        )
        indexVarNamesInThisSet <- indexSets$indexVarNameSets[[iSet]]
        thisContext <-
            modelContextClass$new(context$singleContexts[indexVarNamesInThisSet])

        ## We try making each rule in order.
        ## It one fails, we will throw it away and try to make the next.

        ## y[i] <- x[2] case
        thisIndexRule <- indexRuleClass_all$new(
                                                toIndexExprList = thisLHSindexExprs,
                                                fromIndexExprList = thisRHSindexExprs,
                                                context = thisContext,
                                                constants = constantsEnv)
        ## y[i] -> x[2] getParents case
        if(is.null(thisIndexRule$setupResults)) {  
            thisIndexRule <- indexRuleClass_any$new(
                                                    toIndexExprList = thisLHSindexExprs,
                                                    fromIndexExprList = thisRHSindexExprs,
                                                    context = thisContext,
                                                    constants = constantsEnv)
        }
        ## y[i] <- x[i] case
        if(is.null(thisIndexRule$setupResults)) {
            thisIndexRule <- indexRuleClass_block$new(
                                                      toIndexExprList = thisLHSindexExprs,
                                                      fromIndexExprList = thisRHSindexExprs,
                                                      context = thisContext,
                                                      constants = constantsEnv)
        }
        ## catch-all, e.g., y[i] <- x[block[i]]
        if(is.null(thisIndexRule$setupResults)) {
            thisIndexRule <- indexRuleClass_arbitrary$new(
                                                          toIndexExprList = thisLHSindexExprs,
                                                          fromIndexExprList = thisRHSindexExprs,
                                                          context = thisContext,
                                                          constants = constantsEnv)
        }
        indexRules[[iSet]] <- thisIndexRule
    }
    ## Make constant rules for all LHS constants, e.g. first index in y[3, i] <- x[i]
    iSet <- length(indexRules) + 1
    for(constantSet in which(indexSets$LHSindex2setID == 0)) {
        thisLHSindexExprs <- structure(
            LHSindexExprs[constantSet], names = 't1')        
        thisIndexRule <- indexRuleClass_constant$new(
                                                     toIndexExprList = thisLHSindexExprs,
                                                     fromIndexExprList = character(0),
                                                     context = modelContextClass$new(),
                                                     constants = constantsEnv)
        indexRules[[iSet]] <- thisIndexRule
        iSet <- iSet + 1
    }
    ## Make constant rule for case of no LHS indexing, e.g., y <- x[3]
    if(!length(indexSets$LHSindex2setID)) {
        thisIndexRule <- indexRuleClass_constant$new(
                                                     toIndexExprList = character(0),
                                                     fromIndexExprList = character(0),
                                                     context = modelContextClass$new(),
                                                     constants = constantsEnv)
        indexRules[[iSet]] <- thisIndexRule
    }
    ## Also add: declRule = declRule from input argument
    return(list(
        parentVar = parentVar,
        childVar = childVar,
        indexSets = indexSets,
        indexRules = indexRules,
        constraints = constraints,
        numRHSindices = numRHSindices))
}

## Evaluate validity on a per indexRange basis, looping through constraints
## and combining results from multiple constraints for a single indexRange as needed.
checkConstraints <- function(fromVarRange, constraints) {
    valid <- list(); length(valid) <- length(fromVarRange$indexRanges)    
    if(length(constraints)) 
        for(i in seq_along(constraints)) {
            irIndex <- which(sapply(fromVarRange$rangeID_2_indexID, function(x)
                constraints[[i]]$RHSindex %in% x))
            ## These two error traps should never be triggered. 
            if(!length(irIndex))  
                stop("checkConstraints: No relevant indexRanges for the constraint.")
            if(length(irIndex) > 1)
                stop("checkConstraints: Multiple indexRanges for the constraint should not be possible.")
            ## 'col' only actually used if a matrix indexRange with multiple columns.
            col <- which(fromVarRange$rangeID_2_indexID[[irIndex]] == constraints[[i]]$RHSindex)
            if(length(col) != 1)
                stop("checkConstraints: Multiple columns associated with the constraint should not be possible.")
            ## Result is logical: either a scalar or, for matrix indexRanges, a vector.
            result <- checkOneConstraint(fromVarRange$indexRanges[[irIndex]], constraints[[i]]$constraint, col)
            ## Combine results (element-wise for matrices) from multiple constraints applied to a single indexRange, if needed.
            if(is.null(valid[[irIndex]])) {
                valid[[irIndex]] <- result
            } else valid[[irIndex]] <- valid[[irIndex]] & result
        }
    return(valid)
}

## If indexRanges were R6 classes with inheritance, we could move the
## checking into methods of the indexRange classes.

checkOneConstraint <- function(indexRange, constraint, col = 1) {
    if(!attr(indexRange, 'rangeType') %in% c('scalar', 'sequence', 'matrix', 'none')) {
        warning("Not yet checking input in case of non-{scalar, sequence, matrix, none} indexRanges.")
        return(TRUE)
    }
    rg <- indexRange[[1]]
    if(is.list(rg))   # from a sequence range
        rg <- unlist(rg)

    ## Vectorized check for a matrix.
    if(is.matrix(rg)) {
        return(rg[ , col] >= constraint[1] & rg[ , col] <= constraint[2])
    }
    ## Single logical check of inclusion for scalar or sequence range.
    if(constraint[2] < min(rg) || constraint[1] > max(rg))
        return(FALSE) else return(TRUE)
}

## fromVarRange will have indexRanges that may be sequences, matrices, blanks, or scalars.
##
## We need to extract the relevant components of fromVarRange for each rule, apply the rule,
## and compose the result as a new varRange.
applyGraphRule <- function(fromVarRange,
                                 rule,
                                 varName = NULL) {
    
    if(!is(fromVarRange, 'varRangeClass'))
        stop("applyGraphRule: 'fromVarRange' needs to be a varRange object.")

    if(fromVarRange$isEmpty())  ## TODO: probably no longer needed
        return(NULL)
        ## return(fromVarRange)

    ## Some of the steps below will reveal things that
    ## could be cached and re-used.
    indexSets <- rule$indexSets
    indexRules <- rule$indexRules
    numSets <- indexSets$numSets

    ## Determine number of sets applied to get result (i.e., excluding RHSonly constraint rules from getParents cases)
    constantSets <- which(indexSets$LHSindex2setID == 0)
    numSetsResult <- sum(!indexSets$RHSonly) + length(constantSets)
    numSetsResult <- max(1, numSetsResult)  ## e.g., y <- x[2] case
    
    ## Check valid number of input indices
    numRHSindices <- length(unlist(fromVarRange$rangeID_2_indexID))
    if(numRHSindices != rule$numRHSindices)
        stop("applyGraphRule: incorrect number of input indices.")
        
    ansIndexRanges <- list()
    ansIndexOrders <- list()


    ## Following will be used in some complicated cases not yet implemented.
    ## Check with PdV to get sense of what this was to handle. // CJP
    if(FALSE) {
        ## Set up the implied (times, rep) for each indexRange:
        timesRepList <- list()
        numIndexRanges <- length(fromVarRange$indexRanges)
        if(numIndexRanges == 0)
            stop('fromVarRange has no indexRanges')
        if(numIndexRanges == 1)
            message('implement simpler handling for 1 indexRange')
        rangeLengths <- unlist(lapply(fromVarRange$indexRanges,
                                      indexRange_numRows))
        
        for(i in 1:numIndexRanges) {
            thisTimes <- if(i < numIndexRanges)
                             prod(rangeLengths[(i+1):numIndexRanges])
                         else
                             1
            
            thisRep <- if(i > 1)
                           prod(rangeLengths[1:(i-1)])
                       else
                           1
            timesRepList[[i]] <- c(thisTimes, thisRep)
        }
    }

    ## Determine which RHSindices will be used for each rule
    ## and set up index crossing and aligning needs.
    setID_2_RHSindices <- vector('list', length = numSets)
    complicatedCrossing <- FALSE
    ## each set corresponds to one rule
    for(iSet in seq_len(numSets)) {
        ## Which "from" indices are in this indexSet?
        RHSindicesBool <- indexSets$RHSindex2setID == iSet
        ## extract the relevant indices from fromVarRange
        thisRHSindices <- which(RHSindicesBool)
        setID_2_RHSindices[[iSet]] <- thisRHSindices
            
        ## Discover cases where a rule uses multiple input indexRanges and 
        ## at least one of those indexRanges also covers other indices unused in the rule.
        ## E.g., y[f(i,j),k] <- x[i,j,k] where j,k are together in an input indexRange.
        ## We need those tied to together to avoid incorrect crossing of results.
        ## For simplicity if this happens at all, we do full crossing of all input indices to
        ## implicitly produce a single indexRange.
        ## NOTE: extract this out as a function so it can be tested via unit testing?
        usedRanges <- sapply(fromVarRange$rangeID_2_indexID, function(x) any(thisRHSindices %in% x))
        if(length(unique(fromVarRange$indexID_2_rangeID[thisRHSindices])) > 1) 
            if(!identical(thisRHSindices, sort(unique(unlist(fromVarRange$rangeID_2_indexID[usedRanges])))))
                complicatedCrossing <- TRUE
    }

    inputIndexRanges <- fromVarRange$indexRanges
    numIndexRanges <- length(inputIndexRanges) 
    
    ## Which setIDs are part of an input rangeID
    rangeID_2_setIDs <- lapply(seq_len(numIndexRanges),
                               function(x) integer())

    ## For complicated crossing we first set up the fully-crossed inputs.
    if(complicatedCrossing) {
        warning("Detected that not all indices in an indexRange are used in an indexRule that uses that indexRange, so fully crossing all inputs.")        
        fromIndicesInfoFullyCrossed <-
            fromVarRange$getIndexRangeMatrix(seq_len(numRHSindices),
                                             details = TRUE)
        if(!identical(attr(fromIndicesInfoFullyCrossed$result, 'rangeType'), 'matrix'))
            stop("applyGraphRule: expecting a matrix indexRange.")
    }

    ## Check valid RHS.
    
    ## Returns a list indicating the valid rows for each input indexRange (for matrices) or scalars for non-matrix indexRanges.
    if(complicatedCrossing) {
        RHSconstraints <- checkConstraints(varRangeClass$new(list(fromIndicesInfoFullyCrossed$result)), rule$constraints)
    } else {
        RHSconstraints <- checkConstraints(fromVarRange, rule$constraints)
    }

    ## Apply indexRules, getting inputs from multiple indexRanges if necessary.
    
    setIdx <- 1
    for(iSet in seq_len(numSets)) {
        thisRHSindices <- setID_2_RHSindices[[iSet]]
        
        if(length(thisRHSindices)) {
            if(!complicatedCrossing) {
                fromIndicesInfo <-
                    if(length(thisRHSindices) == 1)
                        fromVarRange$getSingleIndexRange(thisRHSindices,
                                                         details = TRUE)
                    else
                        fromVarRange$getIndexRangeMatrix(thisRHSindices,
                                                         details = TRUE)
                fromIndices <- fromIndicesInfo$result
            } else {
                ## Extract relevant RHS columns from fully-crossed inputs.
                fromIndicesInfo <- fromIndicesInfoFullyCrossed
                fromIndicesInfo$result[[1]] <- fromIndicesInfo$result[[1]][ , thisRHSindices, drop = FALSE]
                fromIndices <- fromIndicesInfo$result
            }
            ## usedRanges is a vector of rangeIDs from RHS from which
            ## fromIndicesInfo were extracted
            usedRanges <- fromIndicesInfo$usedRanges
            for(ur in usedRanges)
                rangeID_2_setIDs[[ur]] <- append(rangeID_2_setIDs[[ur]], iSet)
        } else {  ## no indexing or constant indexing
            fromIndices <- NULL
        }

        ## Apply the rule.
        thisLHSresult <-
            indexRules[[iSet]]$apply(fromIndices,
                                     collapse = FALSE
                                     )

        ## Result could be a regular indexRange or a RHSonly constraint rule result (getParents situation, e.g., y[i] -> x[2].
        if(indexSets$RHSonly[iSet]) {
            ## Add result to RHSconstraints.
            iRange <- fromVarRange$indexID_2_rangeID[which(indexSets$RHSindex2setID == iSet)]  # indexRange(s) involved in constraint
            ## Can have multiple input indexRanges in various cases for single RHSonly constraint rule, e.g.,
            ## (1) with nested indexing, e.g., y[i,j] -> x  where j in 1:n[i]
            ## (2) y[foo(i,j),j] -> x
            ## If so, just duplicate the result for simplicity.
            for(idx in iRange)  
            ## Handle that two RHSonly constraints can use the same indexRange, e.g. y[i,j] -> x[2] for indexRange on i and j
                if(!is.null(RHSconstraints[[idx]])) 
                    RHSconstraints[[idx]] <- RHSconstraints[[idx]] & thisLHSresult # all constraints must be satisfied
                else RHSconstraints[[idx]] <- thisLHSresult
        } else {
            ## Populate answer indexRange information.
            ansIndexRanges[[setIdx]] <- thisLHSresult
            ansIndexOrders[[setIdx]] <-
                which(indexSets$LHSindex2setID == iSet)
            setIdx <- setIdx + 1
        }
    }

    ## Empty result if any indexRange has no valid rows based on constraints.
    invalid <- sapply(seq_along(RHSconstraints), function(i)
        !is.null(RHSconstraints[[i]]) && !any(RHSconstraints[[i]]))
    if(any(invalid)) # as many empty indexRanges as number of rules (apart from RHSonly constraint rules)
        return(NULL)
        ## return(varRangeClass$new(lapply(seq_len(numSetsResult), function(i) indexRange_empty()), varName = rules$childVar))

    ## Compose results from the various rules, including those unrelated to input indexRanges.

    finalIndexRanges <- list()
    finalIndexOrders <- list()

    ## Treat as a single input indexRange, so that collapse across results of all rules.
    if(complicatedCrossing) {  
        numIndexRanges <- 1
        rangeID_2_setIDs <- list(sort(unique(unlist(rangeID_2_setIDs))))
    }

    ## Aggregate results from multiple rules applied to one input.

    ## Loop through rules based on input indexRanges; this will not handle 'any' or 'constant' rules.
    ## Also, this will produce duplicate results (handled at the end) when multiple indexRanges used in a single rule.
    iAns <- 1
    for(iRange in seq_len(numIndexRanges)) {
        sets <- rangeID_2_setIDs[[iRange]]
        sets <- sets[!indexSets$RHSonly[sets]]  ## RHSonly constraint rule handled above.
        if(length(sets)) {
            if(length(sets) > 1) {  # Multiple rules operate on the indexRange.
                if(any(sapply(ansIndexRanges[ sets ],
                              function(x) identical(attr(x, 'rangeType'), 'empty')))) {
                    finalIndexRanges[[iAns]] <- indexRange_empty()
                } else {
                    ## Combine results from multiple rules (which are in matrixList form, because result for an input row can have arbitrary output rows)
                    ## into multi-column matrix indexRange.
                    indexRangeExpandedMatrices <-
                        ansIndexRanges[ sets ]
                    finalIndexRanges[[iAns]] <-
                        collapse_indexRangeMatrices(indexRangeExpandedMatrices)
                }
                ## Sort the columns of the result based on ordering of indices of LHS..
                finalIndexOrders[[iAns]] <-
                    do.call('c',
                            ansIndexOrders[ sets ] )
                sortedIndexOrders <- order(finalIndexOrders[[iAns]])
                if(!identical(sortedIndexOrders, seq_along(sortedIndexOrders))) {
                    finalIndexOrders[[iAns]] <-
                        finalIndexOrders[[iAns]][sortedIndexOrders]
                    if(!identical(attr(finalIndexRanges[[iAns]], 'rangeType'), 'empty'))
                        finalIndexRanges[[iAns]] <-
                            indexRange_matrix(
                                finalIndexRanges[[iAns]][[1]][, sortedIndexOrders, drop = FALSE])
                }
            } else {  # Only one rule operates on the indexRange.
                finalIndexRanges[[iAns]] <-
                    ansIndexRanges[[ sets ]]
                if(identical(
                    attr(finalIndexRanges[[iAns]], 'rangeType'),
                    'matrixList'
                ))  ## Simplify to a matrix indexRange.
                    finalIndexRanges[[iAns]] <-
                        indexRange2matrix(finalIndexRanges[[iAns]])
                finalIndexOrders[[iAns]] <-
                    ansIndexOrders[[ sets ]]
            }
            
            ## Remove invalid rows from matrix indexRanges flagged by constraint checking, only
            ## when indexRange is involved in the rule.
            ## Only need to check cases where have a logical vector as RHSconstraint as
            ## logical scalar (resulting from the constraint index having its own input indexRange)
            ## already checked above where 'invalid' is created and used.
            if(length(RHSconstraints[[iRange]]) > 1) {
                if(!identical(attr(finalIndexRanges[[iAns]], 'rangeType'), 'matrix'))
                    stop("Expecting RHSconstraints to only be relevant for a matrix indexRange.")
                if(length(RHSconstraints[[iRange]]) != nrow(finalIndexRanges[[iAns]][[1]]))
                    stop("Expecting RHSconstraints to have as many logicals as rows of the indexRange.")
                finalIndexRanges[[iAns]][[1]] <-
                    finalIndexRanges[[iAns]][[1]][RHSconstraints[[iRange]], , drop = FALSE]
            }

            iAns <- iAns + 1
        }
    }    

    ## Remove invalid rows from matrix indexRanges based on presence of NAs and set to empty if no rows left
    ## This can only be done after collapsing or else the constituent matrixLists will have different lengths
    ## and couldn't be properly collapsed above (i.e., if we removed the NAs earlier).
    for(iRange in seq_along(finalIndexRanges)) 
        if(identical(attr(finalIndexRanges[[iRange]], 'rangeType'), 'matrix')) {
            NArows <- apply(finalIndexRanges[[iRange]][[1]], 1,
                            function(x) any(is.na(x)))
            if(all(NArows)) {
                finalIndexRanges[[iRange]] <- indexRange_empty()
            } else finalIndexRanges[[iRange]][[1]] <- finalIndexRanges[[iRange]][[1]][!NArows, , drop = FALSE]
        }

    ## Convert single-column matrix indexRanges to sequence if possible
    ## (e.g., to more efficiently handle y[i] <- x[k[i]] cases where all y's included
    for(iRange in seq_along(finalIndexRanges)) 
        if(identical(attr(finalIndexRanges[[iRange]], 'rangeType'), 'matrix'))
           finalIndexRanges[[iRange]] <- indexRange_matrix2sequence(finalIndexRanges[[iRange]])
    

    ## Add in results from 'any' rules, as these have no RHS index used in the rule (i.e., blank or constant RHS, e.g., x[] or x[2])
    missedSets <- which(!seq_along(ansIndexRanges) %in% unlist(rangeID_2_setIDs))
    if(length(missedSets)) {
        finalIndexRanges <- c(finalIndexRanges, ansIndexRanges[missedSets])
        finalIndexOrders <- c(finalIndexOrders, ansIndexOrders[missedSets])
    }

    ## Add in result of constant rules (constant LHS index and (of course) no corresponding RHS index).
    iSet <- 1
    if(length(constantSets)) {
        constantIndexRanges <- list()
        for(constantSet in constantSets) {
            thisLHSresult <- indexRules[[iSet + numSets]]$apply(NULL)
            constantIndexRanges[[iSet]] <- thisLHSresult
            iSet <- iSet + 1
        }
        constantIndexOrders <- as.list(constantSets)
        finalIndexRanges <- c(finalIndexRanges, constantIndexRanges)
        finalIndexOrders <- c(finalIndexOrders, constantIndexOrders)
    }
    ## Add in result of constant rule for case of no LHS indexing at all.
    if(!length(indexSets$LHSindex2setID)) {
        iSet <- which(sapply(indexRules, function(ir) 'indexRuleClass_constant' %in% class(ir)))
        thisLHSresult <- indexRules[[iSet]]$apply(NULL)
        constantIndexRanges <- list(thisLHSresult)
        constantIndexOrders <- as.list(1)
        finalIndexRanges <- c(finalIndexRanges, constantIndexRanges)
        finalIndexOrders <- c(finalIndexOrders, constantIndexOrders)
    }

    ## Put final results in natural order in case they are not already.
    finalIndexOrderStarts <- unlist(lapply(finalIndexOrders, `[`, 1))
    orderFinalIndexOrderStarts <- order(finalIndexOrderStarts)
    if(!identical(orderFinalIndexOrderStarts,
                  seq_along(orderFinalIndexOrderStarts))) {
        finalIndexRanges <- finalIndexRanges[orderFinalIndexOrderStarts]
        finalIndexOrders <- finalIndexOrders[orderFinalIndexOrderStarts]
    }

    ## Remove duplicate columns (from cases where two indexRanges are used in a single rule).
    repeats <- duplicated(finalIndexOrders)

    result <- varRangeClass$new(
        indexInfo = finalIndexRanges[!repeats],
        indexOrders = finalIndexOrders[!repeats],
        varName = ifelse(is.null(varName), rule$childVar, varName),
        stoch = rule$stoch
        )
    if(result$isEmpty()) result <- NULL
    return(result)
}
