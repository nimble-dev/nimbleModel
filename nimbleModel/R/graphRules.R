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
makeSeparableIndexSets <- function(LHS,
                                   RHS,
                                   context) {
    indexVarNames <- structure(context$indexVarNames,
                               names = context$indexVarNames)
    numIndexVars <- length(indexVarNames)
    LHSindexExprs <- as.list(LHS[-c(1,2)])
    LHSnDim <- length(LHSindexExprs)
    RHSindexExprs <- as.list(RHS[-c(1,2)])
    RHSnDim <- length(RHSindexExprs)
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
    currentSetID <- 1
    remainingIndexVarNames <- indexVarNames

    allDone <- FALSE
    while(!allDone) {
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
            
            allAdditionalIndexVarNames <- setdiff(unique(c(LHSadditionalIndexVars,
                                                           RHSadditionalIndexVars)),
                                                  currentIndexVarNames)
            if(length(allAdditionalIndexVarNames)==0) {
                done <- TRUE
            } else {
                currentIndexVarNames <- c(currentIndexVarNames,
                                          allAdditionalIndexVarNames)
            }                                         
        }
        ## recording them this way sorts them:
        indexVarNameSets[[currentSetID]] <- indexVarNames[currentIndexVarNames]
        indexVar2setID[currentIndexVarNames] <- currentSetID
        LHSindex2setID[LHSboolUsesCurrentIndexVars] <- currentSetID
        RHSindex2setID[RHSboolUsesCurrentIndexVars] <- currentSetID
        remainingIndexVarNames <- setdiff(remainingIndexVarNames,
                                          currentIndexVarNames)
        if(length(remainingIndexVarNames)==0)
            allDone <- TRUE
        else
            currentSetID <- currentSetID + 1
    }

    list(LHSindex2setID = LHSindex2setID,
         RHSindex2setID = RHSindex2setID,
         indexVar2setID = indexVar2setID,
         numSets = currentSetID,
         indexVarNameSets = indexVarNameSets
         )
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

    ## Assume there is LHS indexing
    LHSindexExprs <- as.list(LHS[-c(1,2)])

    ## Handle a case like y[i] <- foo(x)
    if(is.name(RHS)) {
        indexSets <- NULL
        indexRules <- indexRuleClass_any$new(
            LHSindexExprs,
            NULL,
            context,
            constants
        )
        return(
            list(indexSets = indexSets,
                 indexRules = indexRules)
        )
    }
        
    indexSets <-
        makeSeparableIndexSets(LHS, RHS, context)

    ## Now assume there is RHS indexing.
    RHSindexExprs <- as.list(RHS[-c(1,2)])
     
    numSets <- indexSets$numSets
    indexRules <- list()
    for(iSet in seq_len(numSets)) {
        LHSindicesBool <- indexSets$LHSindex2setID == iSet
        RHSindicesBool <- indexSets$RHSindex2setID == iSet
        thisLHSindexExprs <- structure(
            LHSindexExprs[LHSindicesBool],
            names = paste0("t", seq_len(sum(LHSindicesBool)))
        )
        thisRHSindexExprs <- structure(
            RHSindexExprs[RHSindicesBool],
            names = paste0("f", seq_len(sum(RHSindicesBool)))
        )
        indexVarNamesInThisSet <- indexSets$indexVarNameSets[[iSet]]
        thisContext <-
            modelContextClass$new(
                context$singleContexts[indexVarNamesInThisSet]
            )
        ## We try making a block rule.
        ## It it fails, we will throw it away.
        thisIndexRule <- indexRuleClass_block$new(
            toIndexExprList = thisLHSindexExprs,
            fromIndexExprList = thisRHSindexExprs,
            context = thisContext,
            constants = constantsEnv)
        if(is.null(thisIndexRule$setupResults)) {
            thisIndexRule <- indexRuleClass_arbitrary$new(
                toIndexExprList = thisLHSindexExprs,
                fromIndexExprList = thisRHSindexExprs,
                context = thisContext,
                constants = constantsEnv)
        }
        indexRules[[iSet]] <- thisIndexRule
    }
    list(indexSets = indexSets,
         indexRules = indexRules)
}

## fromVarRange will have indexRanges that may be
## blocks, matrices, blanks, or scalars.
##
## We need to extract the relevant components of fromVarRange
## for each rule, apply the rules, and compose the result
## as a new varRange.
applyGraphIndexRules <- function(fromVarRange,
                                 rules) {
    ## Some of the steps below will reveal things that
    ## could be cached and re-used.
    indexSets <- rules$indexSets
    indexRules <- rules$indexRules

    ## First handle cases like indexRules_any
    if(is.null(indexSets)) {
        answer <- indexRules$apply(fromVarRange)
        return(
            varRangeClass$new(
                indexInfo = list(ans),
                indexOrders = list(1)
            )
        )
    }
    
    numSets <- indexSets$numSets
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
    ## each set corresponds to one rule
    for(iSet in seq_len(numSets)) {
        # which "from" indices are in this indexSet?
        RHSindicesBool <- indexSets$RHSindex2setID == iSet
        ## extract the relevant indices from fromVarRange
        thisRHSindices <- which(RHSindicesBool)
        setID_2_RHSindices[[iSet]] <- thisRHSindices
        thisIndicesNeedCrossing <-
            if(length(thisRHSindices) == 1)
                FALSE
            else {
                thisRangeIDs <-
                    unlist(fromVarRange$indexID_2_rangeID[thisRHSindices])
                length(unique(thisRangeIDs)) > 1
            }
        if(thisIndicesNeedCrossing)
            stop('Some indices need crossing: not implemented yet.')
    }

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
    for(iSet in seq_len(numSets)) {
        thisRHSindices <- setID_2_RHSindices[[iSet]]
        ##fromVarRange$rangeID_2_indexID
        fromIndicesInfo <-
            if(length(thisRHSindices)==1)
                fromVarRange$getSingleIndexRange(thisRHSindices,
                                                 details = TRUE)
            else
                fromVarRange$getIndexRangeMatrix(thisRHSindices,
                                                 details = TRUE)
        fromIndices <- fromIndicesInfo$result
        ## used ranges is a vector of rangeIDs from RHS from which
        ## fromIndicesInfo were extracted
        usedRanges <- fromIndicesInfo$usedRanges
        for(ur in usedRanges)
            rangeID_2_setIDs[[ur]] <- append(rangeID_2_setIDs[[ur]], iSet)
        thisLHSresult <-
            indexRules[[iSet]]$apply(fromIndices,
                                     collapse = FALSE
                                     )
        ## There may be a need to pull apart thisLHSvarRange
        ## into subsets of its indexRanges
        ansIndexRanges[[iSet]] <- thisLHSresult
        ansIndexOrders[[iSet]] <-
            which(indexSets$LHSindex2setID == iSet)
    }
    ## Compose results: aggregate results from multiple rules
    ## applied to one input.
    finalIndexRanges <- list()
    finalIndexOrders <- list()
    iAns <- 1
    for(iRange in seq_len(numIndexRanges)) {
        if(length(rangeID_2_setIDs[[iRange]]) > 1) {
            indexRangeExpandedMatrices <-
                ansIndexRanges[ rangeID_2_setIDs[[iRange]] ]
            finalIndexRanges[[iAns]] <-
                collapse_indexRangeMatrices(indexRangeExpandedMatrices)
            finalIndexOrders[[iAns]] <-
                do.call('c',
                        ansIndexOrders[ rangeID_2_setIDs[[iRange]] ] )
            sortedIndexOrders <- order(finalIndexOrders[[iAns]])
            if(!identical(sortedIndexOrders, seq_along(sortedIndexOrders))) {
                finalIndexOrders[[iAns]] <-
                    finalIndexOrders[[iAns]][sortedIndexOrders]
                finalIndexRanges[[iAns]] <-
                    indexRange_matrix(
                        finalIndexRanges[[iAns]][[1]][, sortedIndexOrders, drop = FALSE])
            }
        } else {
            finalIndexRanges[iAns] <-
                ansIndexRanges[ rangeID_2_setIDs[[iRange]] ]
            if(identical(
                attr(finalIndexRanges[[iAns]], 'rangeType'),
                'matrixList'
               ))
                finalIndexRanges[[iAns]] <-
                    indexRange2matrix(finalIndexRanges[[iAns]])
                    
            finalIndexOrders[iAns] <-
                ansIndexOrders[ rangeID_2_setIDs[[iRange]] ]
        }
        ## Sometimes this shouldn't increment...?
        iAns <- iAns + 1
    }

    ## Put final results in natural order in case they are not already.
    finalIndexOrderStarts <- unlist(lapply(finalIndexOrders, `[`, 1))
    orderFinalIndexOrderStarts <- order(finalIndexOrderStarts)
    if(!identical(orderFinalIndexOrderStarts,
                  seq_along(orderFinalIndexOrderStarts))) {
        finalIndexRanges <- finalIndexRanges[orderFinalIndexOrderStarts]
        finalIndexOrders <- finalIndexOrders[orderFinalIndexOrderStarts]
    }

    varRangeClass$new(
        indexInfo = finalIndexRanges,
        indexOrders = finalIndexOrders
    )
##    lhsIndices
}
