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
        apply = function(fromIndices) {
            applyGraphIndexRules(
                fromIndices,
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
    
    indexSets <-
        makeSeparableIndexSets(LHS, RHS, context)

    ## Assume there is indexing.
    LHSindexExprs <- as.list(LHS[-c(1,2)])
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
    numSets <- indexSets$numSets
    ## Currently this will handle one set of fromIndices,
    ## not multiples.
    ## ncol needs to come from dim of LHS
    ## nrow needs to come from number of fromIndices provided
    LHSnDim <- length(indexSets$LHSindex2setID)
    ##    lhsIndices <- matrix(ncol = LHSnDim, nrow = 1)
    ansIndexRanges <- list()
    ansIndexOrders <- list()
    RHShandlingRules <- numeric(
    for(iSet in seq_len(numSets)) {
        # which "from" indices are in this indexSet?
        RHSindicesBool <- indexSets$RHSindex2setID == iSet
        ## extract the relevant indices from fromVarRange
        thisRHSindices <- which(RHSindicesBool)
        thisLHSresult <-
            indexRules[[iSet]]$apply(
                                  fromVarRange,
                                  thisRHSindices,
                                  collapse = FALSE
                              )
        ## There may be a need to pull apart thisLHSvarRange
        ## into subsets of its indexRanges
        ansIndexRanges[[iSet]] <- thisLHSvarRange$indexRanges[[1]]
        ansIndexOrders[[iSet]] <-
            which(indexSets$LHSindex2setID == iSet)
    }
    ## Compose results:
    ## 1. find results from input matrix indexRanges that were handled by
    ## different rules.
    numInputRanges <- length(fromVarRules$indexRanges)
    for(iRangeID in seq_len(numInputRanges)) {
        ## which indices does this indexRange handle
        RHSindexIDs <- fromVarRules$rangeID_2_indexID[[iRangeID]]
        ## which sets were these indices handled by?
        setIDs <- indexSets$RHSindex2setID[RHSindexIDs]
        ## If they were handled by multiple sets:
        ## (At this point, the indexRange must definitely be a matrix.)
        if(length(unique(setIDs)) > 1) {
            ## (Make blockRule(matrix) --> matrixList)
            ## Any part expanded by an arbitrary rule will yield a matrixList
            ##
            ## Results will definitely be (matrixList, matrixList, ...)
            ## We need to expand grid across matrixList entries
            
        }
    }
    
    message('Composition of results needs to know when to expand grid')
    message('Need to handle correctly invalid results from rules')
    varRangeClass$new(
        indexInfo = ansIndexRanges,
        indexOrders = ansIndexOrders
    )
##    lhsIndices
}
