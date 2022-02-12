## This doesn't work yet.
graphRuleClass <- R6Class(
    classname = "graphRuleClass",
    portable = FALSE,
    public = list(
        indexRules = NULL,
        initialize = function(LHS,
                              RHS,
                              context) {
            indexRules <<-
                makeGraphIndexRules(LHS,
                                    RHS,
                                    context)
        },
        apply = function(fromVarRange) {
            applyGraphIndexRules(
                fromVarRange,
                indexRules
            )
        }
    )
)

## This file has code for managing the set of rules for a variable.
##
## This first function here separates sets of index relationships
## into separable sets.
##
## See test-graphRules.R

## LHS is an expr like quote(y[i, j])
## RHS is an expr like quote(x[i, j])
## context is a modelContextClass object
##
## TO-DO: recognize non-independence of index-ranges.
## That is *not* handled at the moment.
## 2022-01-14: not clear what non-independence is not handled. // CP
makeSeparableIndexSets <- function(LHS,
                                   RHS,
                                   context) {
    if(length(context$singleContexts)) {
        indexVarNames <- structure(context$indexVarNames,
                                   names = context$indexVarNames)
    } else indexVarNames <- character(0)
    numIndexVars <- length(indexVarNames)

    ## need to check this is working ok for no LHS index case
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

    list(LHSindex2setID = LHSindex2setID,
         RHSindex2setID = RHSindex2setID,
         indexVar2setID = indexVar2setID,
         numSets = currentSetID,
         indexVarNameSets = indexVarNameSets,
         RHSonly = RHSonly
         )
}


makeConstraints <- function(RHSindexExprs, constrainedBool) {
    constraints <- list()
    cnt <- 0
    for(idx in seq_along(RHSindexExprs)) {
        if(constrainedBool[idx]) {
            cnt <- cnt + 1
            constraints[[cnt]] <- list(RHSindex = idx)
            expr <- RHSindexExprs[[idx]]
            if(is.numeric(expr)) {
                constraints[[cnt]]$constraint <- rep(expr, 2)
            } else if(is.call(expr) && expr[[1]] == ":") {
                constraints[[cnt]]$constraint <- c(expr[[2]], expr[[3]])
            } else {  # various other cases: x[] or x[i], x[i+j] RHS index only (getParents case)
                constraints[[cnt]]$constraint <- character(0)
            }
        }
    }
    return(constraints)
}

## The following functions may be used from class methods in the future.
## For now they are standalone for development and debugging.
makeGraphIndexRules <- function(LHS,
                                RHS,
                                context,
                                constants = list()) {
    constantsEnv <- if(is.environment(constants))
                        constants
                    else
                        list2env(constants)
        
    indexSets <-
        makeSeparableIndexSets(LHS, RHS, context)

    if(length(LHS) >= 3 && LHS[[1]] == '[') {
        LHSindexExprs <- as.list(LHS[-c(1,2)])
    } else if(length(LHS) == 1) LHSindexExprs <- list() else
        stop("makeGraphIndexRules: 'LHS' should be an index expression or variable name")

    if(length(RHS) >= 3 && RHS[[1]] == '[') {
        RHSindexExprs <- as.list(RHS[-c(1,2)])
    } else if(length(RHS) == 1) RHSindexExprs <- list() else
        stop("makeGraphIndexRules: 'RHS' should be an index expression or variable name")

    ## Constraints from RHS fixed index values
    constraints <- list()
    RHSindicesBool <- indexSets$RHSindex2setID == 0
    if(any(RHSindicesBool)) 
        constraints <- makeConstraints(RHSindexExprs, RHSindicesBool)
    if(!length(RHSindexExprs))  # placeholder for now for 'x' case (no indexing)
        constraints <- list(list(RHSindex = numeric(0), constraint = rep(0, 2)))

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
        thisIndexRule <- indexRuleClass_all$new(
                                                toIndexExprList = thisLHSindexExprs,
                                                fromIndexExprList = thisRHSindexExprs,
                                                context = thisContext,
                                                constants = constantsEnv)
        if(is.null(thisIndexRule$setupResults)) {  ## y[i] -> x[2] getParents case
            thisIndexRule <- indexRuleClass_any$new(
                                                    toIndexExprList = thisLHSindexExprs,
                                                    fromIndexExprList = thisRHSindexExprs,
                                                    context = thisContext,
                                                    constants = constantsEnv)
        }
        if(is.null(thisIndexRule$setupResults)) {
            thisIndexRule <- indexRuleClass_block$new(
                                                      toIndexExprList = thisLHSindexExprs,
                                                      fromIndexExprList = thisRHSindexExprs,
                                                      context = thisContext,
                                                      constants = constantsEnv)
        }
        if(is.null(thisIndexRule$setupResults)) {
            thisIndexRule <- indexRuleClass_arbitrary$new(
                                                          toIndexExprList = thisLHSindexExprs,
                                                          fromIndexExprList = thisRHSindexExprs,
                                                          context = thisContext,
                                                          constants = constantsEnv)
        }
        indexRules[[iSet]] <- thisIndexRule
    }
    ## Make constant rules for all LHS constants.
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
    ## Make constant rule for case of no LHS indexing. for all LHS constants.
    if(!length(indexSets$LHSindex2setID)) {
        ## thisLHSindexExprs <- structure(list(0), names = 't1')
        thisIndexRule <- indexRuleClass_constant$new(
            toIndexExprList = character(0),
            fromIndexExprList = character(0),
            context = modelContextClass$new(),
            constants = constantsEnv)
        indexRules[[iSet]] <- thisIndexRule
    }
    list(indexSets = indexSets,
         indexRules = indexRules,
         constraints = constraints,
         numRHSindices = numRHSindices
         )
}


## Handle constraints by running over indexRanges rather than over constraints
## and collapse matrix cases across columns when multiple columns in an indexRange
## use recycling to handle T & (T,F) type situations.

## if indexRanges were R6 classes with inheritance, we could move the
## checking into methods of the indexRange classes.
checkOneConstraint <- function(indexRange, constraint) {
    if(!length(constraint)) {
        warning("Not yet checking input to blank index case.")
        return(TRUE)
    }
    if(!attr(indexRange, 'rangeType') %in% c('scalar', 'block', 'matrix', 'none')) {
        warning("Not yet checking input in case of non-{scalar, block, matrix, none} indexRanges.")
        return(TRUE)
    }
    rg <- indexRange[[1]]
    if(is.list(rg))   # from a block range
        rg <- unlist(rg)
    if(is.matrix(rg) && ncol(rg) > 1)
        stop("checkOneConstraint: cannot handle multi-column matrix indexRanges.")
    if(constraint[1] == 0)  # no RHS indices
        if(!length(rg)) return(TRUE) else return(FALSE)
    if(constraint[2] < min(rg) || constraint[1] > max(rg))
        return(FALSE) else return(TRUE)
}

checkNonSeparableConstraint <- function(indexRange, rangeID_2_indexID, constraints) {
    ## Get the (column) elements of the matrix indexRange that need to be checked
    constraintsIndex <- sapply(constraints, `[[`, 'RHSindex')
    matched <- match(rangeID_2_indexID, constraintsIndex)
    names(matched) <- seq_along(matched)
    matchedConstraints <- matched[!is.na(matched)]
    matchedRangeID <- as.numeric(names(matchedConstraints))  # which column of the matrix matches the given constraint
    if(length(matchedConstraints)) {
        constraint <- constraints[[matchedConstraints[1]]]$constraint
        if(!length(constraint)) {
            warning("Not yet checking input to blank index case.")
            constraint <- c(-Inf, Inf)  # need something so invalid can be created
        } 
        irCol <- indexRange[[1]][ , matchedRangeID[1]]
        invalid <- constraint[2] < irCol | constraint[1] > irCol
        if(length(matchedConstraints) > 1)
            for(i in 2:length(matchedConstraints)) {
                constraint <- constraints[[matchedConstraints[i]]]$constraint
                if(!length(constraint)) {
                    warning("Not yet checking input to blank index case.")
                } else {
                    irCol <- indexRange[[1]][ , matchedRangeID[i]]
                    invalid <- invalid |
                        constraint[2] < irCol | constraint[1] > irCol
                }
            }
    }
    return(!invalid)
}

checkConstraints <- function(fromVarRange, constraints) {
    valid <- list(); length(valid) <- length(fromVarRange$indexRanges)    
    if(length(constraints)) {
        for(i in seq_along(constraints)) {
            irIndex <- sapply(fromVarRange$rangeID_2_indexID, function(x)
                constraints[[i]]$RHSindex %in% x)
            if(!length(irIndex)) {
                browser()
                return(checkOneConstraint(fromVarRange$indexRanges[[1]], constraints[[i]]$constraint))
            } else {
                irIndex <- which(irIndex)
                if(!(attr(fromVarRange$indexRanges[[irIndex]], 'rangeType') == 'matrix' &&
                     ncol(fromVarRange$indexRanges[[irIndex]][[1]]) > 1))  ## multi-column matrices handled non-separably below
                    valid[[irIndex]] <- checkOneConstraint(fromVarRange$indexRanges[[irIndex]], constraints[[i]]$constraint)
            }
        }
        ## add non-separability check for matrix cases
        ## should this be handled more similarly to 
        matIRs <- which(sapply(fromVarRange$indexRanges, function(x)
            identical(attr(x, 'rangeType'), 'matrix') &&
            ncol(x[[1]]) > 1))

        for(idx in matIRs) 
            valid[[idx]] <- checkNonSeparableConstraint(fromVarRange$indexRanges[[idx]],
                                            fromVarRange$rangeID_2_indexID[[idx]], constraints)
    }
    return(valid)
}

## fromVarRange will have indexRanges that may be
## blocks, matrices, blanks, or scalars.
##
## We need to extract the relevant components of fromVarRange
## for each rule, apply the rules, and compose the result
## as a new varRange.
applyGraphIndexRules <- function(fromVarRange,
                                 rules) {

    if(!is(fromVarRange, 'varRangeClass'))
        stop("applyGraphIndexRules: 'fromVarRange' needs to be a varRange object.")
    ## Some of the steps below will reveal things that
    ## could be cached and re-used.
    indexSets <- rules$indexSets
    indexRules <- rules$indexRules
    numSets <- indexSets$numSets

    constantSets <- which(indexSets$LHSindex2setID == 0)
    numSetsResult <- sum(!indexSets$RHSonly) + length(constantSets)
    numSetsResult <- max(1, numSetsResult)  ## e.g., y <- x[2] case
    
    ## Check valid number of input indices
    numRHSindices <- length(unlist(fromVarRange$rangeID_2_indexID))
    if(numRHSindices != rules$numRHSindices)
        stop("applyGraphIndexRules: incorrect number of input indices.")
        
    ## First handle cases like indexRules_any
    ## I think this is defunct - not sure what it was intended to handle, perhaps constant rules? // CJP 2022-02-05
    if(is.null(indexSets)) {
        answer <- indexRules$apply(fromVarRange)
        return(
            varRangeClass$new(
                indexInfo = list(ans),
                indexOrders = list(1)
            )
        )
    }
    
    ## Currently this will handle one set of fromIndices,
    ## not multiples.
    ## ncol needs to come from dim of LHS
    ## nrow needs to come from number of fromIndices provided
    LHSnDim <- length(indexSets$LHSindex2setID)
    ##    lhsIndices <- matrix(ncol = LHSnDim, nrow = 1)
    ansIndexRanges <- list()
    ansIndexOrders <- list()
    RHShandlingRules <- numeric()

    ## Set up the implied (times, rep) for each indexRange:
    timesRepList <- list()
    numIndexRanges <- length(fromVarRange$indexRanges)
    if(numIndexRanges == 0)
        stop('fromVarRange has no indexRanges')
    if(numIndexRanges == 1)
        message('implement simpler handling for 1 indexRange')
    rangeLengths <- unlist(lapply(fromVarRange$indexRanges,
                                  indexRange_numRows))
    ## Following will be used in some complicated cases not
    ## yet implemented.  It is deactivated behind if(FALSE) {}.
    if(FALSE) {
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
        # which "from" indices are in this indexSet?
        RHSindicesBool <- indexSets$RHSindex2setID == iSet
        ## extract the relevant indices from fromVarRange
        thisRHSindices <- which(RHSindicesBool)

        setID_2_RHSindices[[iSet]] <- thisRHSindices
            
        ## Not clear how this would be used; crossing works (using
        ## fromVarRange$getIndexRangeMatrix) for cases where
        ## the RHS indexRanges don't cause additional indices unused in a rule
        ## to be tied together with the indices that are used.
        ## I.e., // CP
        thisIndicesNeedCrossing <-
            if(length(thisRHSindices) == 1)
                FALSE
            else {
                thisRangeIDs <-
                    unlist(fromVarRange$indexID_2_rangeID[thisRHSindices])
                length(unique(thisRangeIDs)) > 1
            }
        if(thisIndicesNeedCrossing)
            warning('Some indices need crossing: not fully implemented yet; proceeding anyway.')

        ## Discover cases where a rule uses multiple input indexRanges and 
        ## at least one indexRange also covers other indices unused in the rule.
        ## We need those tied to together to avoid incorrect crossing.
        ## For simplicity if this happens at all, we do full crossing of all input indices to
        ## implicitly produce a single indexRange.
        ## NOTE: extract this out as a function so it can be tested via unit testing?
        usedRanges <- sapply(fromVarRange$rangeID_2_indexID, function(x) any(thisRHSindices %in% x))
        if(length(unique(fromVarRange$indexID_2_rangeID[thisRHSindices])) > 1) 
            if(!identical(thisRHSindices, sort(unique(unlist(fromVarRange$rangeID_2_indexID[usedRanges])))))
                complicatedCrossing <- TRUE
    }

    if(complicatedCrossing)
        warning("Detected unused indices in an indexRange used in an indexRule, so fully crossing all inputs.")

    inputIndexRanges <- fromVarRange$indexRanges
    numIndexRanges <- length(inputIndexRanges) ## reset in case something changes
    ## indexID_2_rangeID <- fromVarRange$indexID_2_rangeID
    ## rangeID_2_indexID <- fromVarRange$rangeID_2_indexID
    ## modify by expansion if needed,
    ## until application of rules is nested within inputIndexRanges
    ## can test equivalence of new indexRules by full expansion
    
    ## At this point we assume the columns needed come from only a single
    ## indexRange.
    ## Need to track which rules apply to which input indexRange
    ## And expand their results together.
    
    ## which setIDs are part of an input rangeID
    rangeID_2_setIDs <- lapply(seq_len(numIndexRanges),
                               function(x) integer())

    ## For complicated crossing we first set up the fully-crossed inputs.
    if(complicatedCrossing) {
        
        fromIndicesInfoFullyCrossed <-
            fromVarRange$getIndexRangeMatrix(seq_len(numRHSindices),
                                             details = TRUE)
        if(!identical(attr(fromIndicesInfoFullyCrossed$result, 'rangeType'), 'matrix'))
            stop("applyGraphIndexRules: expecting a matrix indexRange.")
        RHSconstraints <- checkConstraints(varRangeClass$new(list(fromIndicesInfoFullyCrossed$result)), rules$constraints)
    } else {
        ## Check valid RHS
        ## Returns a list indicating the valid rows for each input indexRange (for matrices)
        ## or boolean for non-matrix indexRange.
        RHSconstraints <- checkConstraints(fromVarRange, rules$constraints)
    }

    setIdx <- 1
    for(iSet in seq_len(numSets)) {
        thisRHSindices <- setID_2_RHSindices[[iSet]]
        ##fromVarRange$rangeID_2_indexID
        
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
            ## used ranges is a vector of rangeIDs from RHS from which
            ## fromIndicesInfo were extracted
            usedRanges <- fromIndicesInfo$usedRanges
            for(ur in usedRanges)
                rangeID_2_setIDs[[ur]] <- append(rangeID_2_setIDs[[ur]], iSet)
        } else {  ## no indexing or constant indexing
            fromIndices <- NULL
        }

        thisLHSresult <-
            indexRules[[iSet]]$apply(fromIndices,
                                     collapse = FALSE
                                     )

        ## Result could be a regular indexRange or info about a RHS only constraint check
        if(indexSets$RHSonly[iSet]) {
            iRange <- fromVarRange$indexID_2_rangeID[which(indexSets$RHSindex2setID == iSet)]
            ## Can have multiple input indexRanges when have nested indexing, in which case, just duplicate them.
            for(idx in iRange)  
            ## & is needed in cases where have two RHSindex constraints that use the same indexRange.
                if(!is.null(RHSconstraints[[idx]]))
                    RHSconstraints[[idx]] <- RHSconstraints[[idx]] & thisLHSresult
                else RHSconstraints[[idx]] <- thisLHSresult
        } else {
            ## There may be a need to pull apart thisLHSvarRange
            ## into subsets of its indexRanges
            ansIndexRanges[[setIdx]] <- thisLHSresult
            ansIndexOrders[[setIdx]] <-
                which(indexSets$LHSindex2setID == iSet)
            setIdx <- setIdx + 1
        }
    }

    ## Invalid if indexRange has no valid rows based on constraints.
    invalid <- sapply(seq_along(RHSconstraints), function(i)
        !is.null(RHSconstraints[[i]]) && !any(RHSconstraints[[i]]))
    if(any(invalid)) 
        return(varRangeClass$new(lapply(seq_len(numSetsResult), function(i) indexRange_empty())))

    ## Compose results: aggregate results from multiple rules applied to one input.
    
    finalIndexRanges <- list()
    finalIndexOrders <- list()

    ## Treat as a single input indexRange, so that collapse across results of all rules.
    if(complicatedCrossing) {  
        numIndexRanges <- 1
        rangeID_2_setIDs <- list(sort(unique(unlist(rangeID_2_setIDs))))
    }

    iAns <- 1
    for(iRange in seq_len(numIndexRanges)) {
        sets <- rangeID_2_setIDs[[iRange]]
        sets <- sets[!indexSets$RHSonly[sets]]
        if(length(sets)) {
            if(length(sets) > 1) {
                if(any(sapply(ansIndexRanges[ sets ],
                              function(x) identical(attr(x, 'rangeType'), 'empty')))) {
                    finalIndexRanges[[iAns]] <- indexRange_empty()
                } else {
                    ## NOTE: Insert a check that number of input rows are all the same?
                    ## Deal with NULL entries - need to turn into NA or 0?
                    indexRangeExpandedMatrices <-
                        ansIndexRanges[ sets ]
                    finalIndexRanges[[iAns]] <-
                        collapse_indexRangeMatrices(indexRangeExpandedMatrices)
                }
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
            } else {
                finalIndexRanges[[iAns]] <-
                    ansIndexRanges[[ sets ]]
                if(identical(
                    attr(finalIndexRanges[[iAns]], 'rangeType'),
                    'matrixList'
                ))
                    finalIndexRanges[[iAns]] <-
                        indexRange2matrix(finalIndexRanges[[iAns]])
                
                finalIndexOrders[[iAns]] <-
                    ansIndexOrders[[ sets ]]
            }
            
            ## Remove invalid rows from matrix indexRanges flagged by constraint checking, only
            ## when indexRange is involved in the rule.
            ## Only need to check cases where have a logical vector as RHSconstraint as scalar
            ## logical already checked.
            if(length(RHSconstraints[[iRange]]) > 1) {
                if(!identical(attr(finalIndexRanges[[iAns]], 'rangeType'), 'matrix'))
                    stop("Expecting RHSconstraints to only be relevant for a matrix indexRange.")
                if(length(RHSconstraints[[iRange]]) != nrow(finalIndexRanges[[iAns]][[1]]))
                    stop("Expecting RHSconstraints to have as many logicals as row of the indexRange.")
                finalIndexRanges[[iAns]][[1]] <-
                    finalIndexRanges[[iAns]][[1]][RHSconstraints[[iRange]], , drop = FALSE]
            }

            iAns <- iAns + 1
        }
    }    
        
    ## Remove invalid rows from matrix indexRanges based on presence of NAs and set to empty if no rows left
    ## This can only be done after collapsing or else the constituent matrixLists will have different lengths
    ## (e.g., if we removed the NAs earlier).
    for(iRange in seq_along(finalIndexRanges)) 
        if(identical(attr(finalIndexRanges[[iRange]], 'rangeType'), 'matrix')) {
            NArows <- apply(finalIndexRanges[[iRange]][[1]], 1,
                            function(x) any(is.na(x)))
            if(all(NArows)) {
                finalIndexRanges[[iRange]] <- indexRange_empty()
            } else finalIndexRanges[[iRange]][[1]] <- finalIndexRanges[[iRange]][[1]][!NArows, , drop = FALSE]
        }

    ## Add in rules that have no RHS index (not present, blank, or constant)
    missedSets <- which(!seq_along(ansIndexRanges) %in% unlist(rangeID_2_setIDs))
    if(length(missedSets)) {
        finalIndexRanges <- c(finalIndexRanges, ansIndexRanges[missedSets])
        finalIndexOrders <- c(finalIndexOrders, ansIndexOrders[missedSets])
    }

    ## Add in constant rules (constant LHS index and (of course) no RHS index)
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
    ## Constant rule for case of no LHS indexing.
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

    ## Remove duplicate columns (from cases where two indexRanges go to same output index)
    repeats <- duplicated(finalIndexOrders)
    
    varRangeClass$new(
        indexInfo = finalIndexRanges[!repeats],
        indexOrders = finalIndexOrders[!repeats]
    )
##    lhsIndices
}
