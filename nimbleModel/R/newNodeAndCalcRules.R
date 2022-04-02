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
        varName = NULL,
        ID = NULL,
        sortID = NULL,
        stoch = NULL,  # boolean
        originalRule = NULL, # pointer to canonical nodeRule from declaration (possibly `self`
        externalRules = NULL, # indexing for the nodes
        internalRules = NULL, # indexing for components, if multivariate nodes
        originalIndexRules = NULL, # determines original indexing (based on context); set equal to originalRule$originalIndexRule if not canonical nodeRule
        stochParent = FALSE,
        stochDep = FALSE,
        touched_down = FALSE,
        touched_up = FALSE,

        ## do we want canonicalRange (either at var or node level)?

        initialize = function(LHS, context = modelContextClass$new(), constants = list()) {
            ## Set up rules that operate on the indexing of the nodes and on
            ## the internal indexing of the elements of a node.
            if(length(LHS) > 1)
                varName <<- LHS[[2]] else varName <<- LHS   ## not clear everything will go through if no indexing
            originalIndexRules <<- originalIndexRuleClass$new(LHS, context, constants)

            ## Note: this is awkward to go into the data structures and modify them
            fullRules <- makeGraphIndexRules(LHS, LHS, context)
            index2setID <<- fullRules$indexSets$LHSindex2setID
            isConstant <- sapply(fullRules$indexRules, is, "indexRuleClass_constant")
            externalRules <<- fullRules
            externalRules$indexRules[isConstant] <<- NULL
            externalRules$indexSets$LHSindex2setID <<-
                externalRules$indexSets$LHSindex2setID[externalRules$indexSets$LHSindex2setID != 0]
            internalRules <<- fullRules
            internalRules$indexRules[!isConstant] <<- NULL
            internalRules$indexSets$numSets <<- 0
            internalRules$indexSets$LHSindex2setID <<-
                internalRules$indexSets$LHSindex2setID[internalRules$indexSets$LHSindex2setID == 0]

            numExternalRules <<- length(externalRules$indexRules)
            numInternalRules <<- length(internalRangeRules$indexRules)
        },

        apply = function(varRange) {
            if(numExternalRules) {
                externalRange <- applyGraphIndexRules(varRange, externalRules)
            } else externalRange <- varRangeClass$new(list(nimbleModel:::indexRange_empty()))
            if(numInternalRules) {
                internalRange <- applyGraphIndexRules(varRange, internalRangeRules)
            } else internalRange <- varRangeClass$new(list(nimbleModel:::indexRange_empty()))
            result <- nodeRangeClass$new(varName, externalRange, internalRange, index2setID)
            return(result)
        }
        
    )
)
