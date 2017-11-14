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
