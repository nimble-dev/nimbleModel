## converts varRange to a nodeRange

## example: y[i, 1:5] ~ dmnorm() has:
## nodeRule: first index, over full context
## internalRule: 1:5

## or y[1:5, i, 2:6, j] ~ dwish()
## nodeRule for i,j
## internalRule for 1:5, 2:6

nodeRuleClass <- R6Class(
    classname = "nodeRuleClass",
    portable = FALSE,
    public = list(
        varName = NULL,
        nodeRangeRules = NULL,       # a varRange
        internalRangeRules = NULL,  # a varRange
        index2setID = NULL, # positions for node and element indexing
        originalIndexRule = NULL,
        numNodeRules = NULL,
        numInternalRules = NULL,
        stoch = NULL,  ## need more input info to determine this
        nodeFun = NULL, ## density calculation information - perhaps just original code

        initialize = function(LHS, context, constants) {  # y[i, 2:3]
            ## Note: this is awkward to go into the data structures and modify them
            ## Set up rules that operate on the indexing of the nodes and on
            ## the internal indexing of the elements of a node.
            if(length(LHS) > 1)
                varName <<- LHS[[2]] else varName <<- LHS   ## not clear everything will go through if no indexing
            
            originalIndexRule <<- originalIndexRuleClass$new(LHS, context, constants)
            
            fullRules <- makeGraphIndexRules(LHS, LHS, context)
            index2setID <<- fullRules$indexSets$LHSindex2setID
            isConstant <- sapply(fullRules$indexRules, is, "indexRuleClass_constant")
            nodeRangeRules <<- fullRules
            nodeRangeRules$indexRules[isConstant] <<- NULL
            nodeRangeRules$indexSets$LHSindex2setID <<-
                nodeRangeRules$indexSets$LHSindex2setID[nodeRangeRules$indexSets$LHSindex2setID != 0]
            internalRangeRules <<- fullRules
            internalRangeRules$indexRules[!isConstant] <<- NULL
            internalRangeRules$indexSets$numSets <<- 0
            internalRangeRules$indexSets$LHSindex2setID <<-
                internalRangeRules$indexSets$LHSindex2setID[internalRangeRules$indexSets$LHSindex2setID == 0]

            numNodeRules <<- length(nodeRangeRules$indexRules)
            numInternalRules <<- length(internalRangeRules$indexRules)
            
        },

        apply = function(varRange) {
            ## what return if either of the ranges is empty?
            if(numNodeRules) {
                nodeRange <- applyGraphIndexRules(varRange, nodeRangeRules)
            } else nodeRange <- varRangeClass$new(list(nimbleModel:::indexRange_empty()))
            if(numInternalRules) {
                internalRange <- applyGraphIndexRules(varRange, internalRangeRules)
            } else internalRange <- varRangeClass$new(list(nimbleModel:::indexRange_empty()))
            result <- nodeRangeClass$new(varName, nodeRange, internalRange, index2setID)
            return(result)
        },

        fracture = function(varRange) {
            ## e.g. fracture y[3:8] with y[7] -> y[3:6], y[7], y[8]
            ## y[3:8] is canonicalRange for the rule

        }
        ## perhaps have a method that just returns back the full nodeRange associated with the nodeRule, i.e.,
        ## for(i in 1:3) for(j in 1:2) y[j,i,2:4] would return y[(1:2),(1:3),2:4]

        ## How return only top, only latent, etc. without full expansion
        ## could we somehow have a "topRule", "latentRule", etc. and pass full result through the rule of interest?
        ## or multiple such rules if y[1:3], y[5:9] are top, e.g.
    )
)

## check case of LHS <- quote(y)

if(FALSE) {
## simple cases
LHS <- quote(y[i])
fullRules <- makeGraphIndexRules(LHS, LHS, context_i)
nr <- applyGraphIndexRules(varRangeClass$new(list(indexRange(quote(1:10)))), nodeRangeRule)

LHS <- quote(y[3:6])
fullRules <- makeGraphIndexRules(LHS, LHS, context_0)
ir <- applyGraphIndexRules(varRangeClass$new(list(indexRange(quote(1:10)))), internalRangeRules)

LHS <- quote(y[3:5,2])
fullRules <- makeGraphIndexRules(LHS, LHS, context_0)
ir <- applyGraphIndexRules(varRangeClass$new(list(indexRange(quote(1:10)), indexRange(2))), internalRangeRules)


LHS <- quote(y[3:6,i,2:4,j])
fullRules <- makeGraphIndexRules(LHS, LHS, context_ij)
## this works for totally separate inputs
nr <- applyGraphIndexRules(varRangeClass$new(list(
                                       indexRange(quote(2:4)),
                                       indexRange(quote(1:11)),
                                       indexRange(quote(2:4)),
                                       indexRange(quote(1:30)))), nodeRangeRule)
ir <- applyGraphIndexRules(varRangeClass$new(list(
                                       indexRange(quote(2:4)),
                                       indexRange(quote(1:11)),
                                       indexRange(quote(2:4)),
                                       indexRange(quote(1:30)))), internalRangeRule)


## this gives a matrix ir output
nr <- applyGraphIndexRules(varRangeClass$new(list(
                                             indexRange(matrix(c(3,4,3,1,2,2,2,2), ncol = 4)))),
                           nodeRangeRule)
## this gives crossed seq ranges
ir <- applyGraphIndexRules(varRangeClass$new(list(
                                             indexRange(matrix(c(3,4,3,1,2,2,2,2), ncol = 4)))),
                           internalRangeRule)
## one row invalid
ir <- applyGraphIndexRules(varRangeClass$new(list(
                                             indexRange(matrix(c(3,4,30,1,2,2,2,2), ncol = 4)))),
                           nodeRangeRule)


## have variation on makeGraphIndexRules that gives only constantSets or only non-constantSets back?


LHS <- quote(y[3:6,j,2,k,i])
inputVR <- varRangeClass$new(list(
                             indexRange(matrix(c(3,3,3,1,2,2,2,2,2,2,1,2), ncol = 4)),
                             indexRange(quote(1:5))))

nodeRule <- nodeRuleClass$new(LHS, context_ijk)
nodeRange <- nodeRule$apply(inputVR)

nodeRange$expandNames()

## works with invalid internal index value
LHS <- quote(y[3:6,j,2,k,i])
inputVR <- varRangeClass$new(list(
                             indexRange(matrix(c(3,3,3,1,2,2,4,2,2,2,1,2), ncol = 4)),
                             indexRange(quote(1:5))))

nodeRule <- nodeRuleClass$new(LHS, context_ijk)
nodeRange <- nodeRule$apply(inputVR)

nodeRange$expandNames()

nodeRange$getVarRange()

 
     code <- nimbleCode({
        for(i in 1:3)
            y[i,1:3] <- mu[i,1:3]
        mu[2,2] ~ dnorm(0,1)
    })
    code <- nimbleCode({
        for(i in 1:3)
            y[i,1:3] <- mu[i,1:3]
        mu[2,1:3] ~ dmnorm(z[1:3],pr[1:3,1:3])
    })
     code <- nimbleCode({
         for(i in 1:3)
             for(j in 1:3)
                 y[i,j] <- mu[i,j]
         mu[2,2]~dnorm(0,1)
    })
    
    
   singleContext <-
    modelSingleContext(indexVarExpr = quote(i),
                       indexRangeExpr = quote(c(3,5,7)),
                       )
 
    
}
