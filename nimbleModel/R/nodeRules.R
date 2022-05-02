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
            isScalarConstant <- sapply(allRules$indexRules, is, "indexRuleClass_constant") &
                sapply(allRules$indexRules, function(rule) identical(attr(rule$setupResults[[1]], 'rangeType'), 'scalar'))
            
            numIndices <<- length(allRules$indexSets$LHSindex2setID)

## TODO: fix so that externalRules has internal 0 LHSindex2setID for scalar constant index
            
            ## Treat block constants as internal rules that don't relate to indexing over nodes.
            externalRules <<- allRules
            externalRules$indexRules[isBlockConstant] <<- NULL

            ## CHECK THIS
            zeroes <- which(externalRules$indexSets$LHSindex2setID == 0)
            constants <- which(isBlockConstant | isScalarConstant)
            scalarConstants <- which(isScalarConstant)
            scalarConstants <- which(isBlockConstant)
            scalarIndices <- zeroes[which(constants %in% scalarConstants)]
            blockIndices <- zeroes[which(constants %in% blockConstants)]
            
            externalRules$indexSets$LHSindex2setID <<-
                externalRules$indexSets$LHSindex2setID[sort(which(internalRules$indexSets$LHSindex2setID != 0), scalarIndices)]
            
            internalRules <<- allRules
            internalRules$indexRules[!isBlockConstant] <<- NULL
            internalRules$indexSets$numSets <<- 0
            internalRules$indexSets$LHSindex2setID <<-
                internalRules$indexSets$LHSindex2setID[blockIndices]
            
            numExternalRules <<- length(externalRules$indexRules)
            numInternalRules <<- length(internalRules$indexRules)

        },

        apply = function(varRange = NULL) {
            if(is.null(varRange))   ## user wants full range for the variable
                varRange <- getFullRange()
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
                stochParent = stochParent <<- TRUE,
                stop("Invalid type ", type)
            )            
        },

        unset = function(type) {
            switch(type,
                end = end <<- FALSE,
                top = top <<- FALSE,
                RHSonly = RHSonly <<- FALSE,
                stochParent = stochParent <<- FALSE,
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
            index2setID <<- index2setID  # should this be a field or just use rule$index2setID?

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

nodeRange_isEqual <- function(nr1, nr2) {
    if(is.null(nr1$externalRange) && !is.null(nr2$externalRange) ||
       !is.null(nr1$externalRange) && is.null(nr2$externalRange) ||
       is.null(nr1$internalRange) && !is.null(nr2$internalRange) ||
       !is.null(nr1$internalRange) && is.null(nr2$internalRange))
        return(FALSE)
    ok <- identical(nr1$indexID_2_rangeID, nr2$indexID_2_rangeID) &&
        identical(nr1$rangeID_2_indexID, nr2$rangeID_2_indexID)
    if(!is.null(nr1$externalRange))
        ok <- ok && identical(nr1$externalRange$indexRanges, nr2$externalRange$indexRanges)
    if(!is.null(nr1$internalRange))
        ok <- ok && identical(nr1$internalRange$indexRanges, nr2$internalRange$indexRanges)
    return(ok)
}


## Fracture a LHS rule based on dependencies of another LHS rule, produced from applyGraphIndexRules,
## passed through a nodeRule to produce a nodeRange
## Result can be:
##  - original rule
##  - subset of original rule and a rule created from the fracturingRange
##  - two subsets of original rule and a rule created from the fracturingRange (when dealing with a sequence that is split)

fracture <- function(LHSrule, fracturingRange) {
    ## presume fracturingRange is a nodeRange, so has external/internal split consistent with LHS rule
    ## but it could be that we pass in a varRange and then use nodeRule$apply to get the nodeRange
    ## this also means that fracturingRange is the 'intersection' as it shouldn't contain anything not in the LHSrule

    ## A bit convoluted to extract the varRange and then turn that into a nodeRange.
    LHSrange <- LHSrule$apply(LHSrule$getFullRange())
    ## equivalent to LHSrule$apply()

    if(nodeRange_isEqual(LHSrange, fracturingRange)) {
        LHSrule$set('stochParent')
        return(LHSrule)
    }

    ## Indices for internalRange should be identical, so just check/fracture those for external
    identicalIndices <- sapply(seq_along(LHSrange$externalRange$indexID_2_rangeID), function(idx)
        isTRUE(all.equal(LHSrange$externalRange$indexRanges[[LHSrange$externalRange$indexID_2_rangeID[idx]]],
                  fracturingRange$externalRange$indexRanges[[fracturingRange$externalRange$indexID_2_rangeID[[idx]]]])))
    
    nonIdenticalExternalIndices <- which(!identicalIndices)
    nonIdenticalFullIndices <- which(LHSrule$index2setID != 0)[nonIdenticalExternalIndices]
    
    expr <- LHSrule$expr
    singleContexts <- LHSrule$context$singleContexts

    if(LHSrule$numIndices == 1 || length(nonIdenticalFullIndices) == 1) {
        LHS <- LHSrange$externalRange$indexRanges[[LHSrange$externalRange$indexID_2_rangeID[nonIdenticalExternalIndices]]]
        frac <- fracturingRange$externalRange$indexRanges[[fracturingRange$externalRange$indexID_2_rangeID[nonIdenticalExternalIndices]]]
        typeLHS <- attr(LHS, "rangeType")
        typeFrac <- attr(frac, "rangeType")

        focalContext <- sapply(names(singleContexts), function(nm)
            nm %in% all.vars(expr[[2+nonIdenticalFullIndices]]))
        
        if(typeLHS == "matrix" || typeFrac == "matrix") {
            valsLHS <- switch(typeLHS,
                              matrix = LHS[[1]],
                              scalar = LHS[[1]],
                              sequence = LHS[[1]][[1]]:LHS[[1]][[2]],
                              stop("typeLHS not found")
                              )
            valsFrac <- switch(typeFrac,
                              matrix = frac[[1]],
                              scalar = frac[[1]],
                              sequence = frac[[1]][[1]]:frac[[1]][[2]],
                              stop("typeFrac not found")
                              )
            valsLHS <- valsLHS[!valsLHS %in% valsFrac]

            ## Modify LHSrule expr and context to insert vector of relevent values.
            newSingleContexts1 <- singleContexts[!focalContext]
            newSingleContexts2 <- singleContexts[!focalContext]

            newSingleContexts1[[length(newSingleContexts1)+1]] <- modelSingleContext(
                               indexVarExpr = quote(.newidx),
                indexRangeExpr = substitute(1:L, list(L = length(valsLHS))))

            newSingleContexts2[[length(newSingleContexts2)+1]] <- modelSingleContext(
                               indexVarExpr = quote(.newidx),
                indexRangeExpr = substitute(1:L, list(L = length(valsFrac))))

            expr[[nonIdenticalFullIndices+2]] <- quote(.idx[.newidx])
         
            resultRule <- nodeRuleClass$new(expr, TRUE, LHSrule$ID, LHSrule$stoch, context = modelContextClass$new(newSingleContexts1),
                                            constants = list(.idx = valsLHS))

            fracturingRule <- nodeRuleClass$new(expr, TRUE, LHSrule$ID, LHSrule$stoch, context = modelContextClass$new(newSingleContexts2),
                                            constants = list(.idx = valsFrac))
            fracturingRule$set('stochParent')
            
            return(list(resultRule, fracturingRule))
        } else {  # seq+seq or seq+scalar
            if(typeFrac == "scalar")
                frac <- indexRange(substitute(A:A, list(A = frac[[1]])))
            ## now process two seqs
            if(typeLHS == "scalar") stop("Not expecting LHS to be a scalar")  ## scalar LHS either fully intersected or not intersected
          
            if(frac[[1]][1] == LHS[[1]][[1]] || frac[[1]][[2]] == LHS[[1]][[2]]) {
                ## Shrink existing index block
                if(frac[[1]][[1]] == LHS[[1]][[1]]) 
                    LHS[[1]][[1]] <- frac[[1]][[2]]+1 else LHS[[1]][[2]] <- frac[[1]][[1]]-1

                newSingleContexts1 <- singleContexts[!focalContext]
                newSingleContexts2 <- singleContexts[!focalContext]

                newSingleContexts1[[length(newSingleContexts1)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = LHS[[1]][[1]], B = LHS[[1]][[2]])))

                newSingleContexts2[[length(newSingleContexts2)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = frac[[1]][[1]], B = frac[[1]][[2]])))

                expr[[nonIdenticalFullIndices+2]] <- newSingleContexts1[[length(newSingleContexts1)]]$indexVarExpr

                resultRule <- nodeRuleClass$new(expr, TRUE, LHSrule$ID, LHSrule$stoch, context = modelContextClass$new(newSingleContexts1))

                fracturingRule <- nodeRuleClass$new(expr, TRUE, LHSrule$ID, LHSrule$stoch, context = modelContextClass$new(newSingleContexts2))
                fracturingRule$set('stochParent')
                
                return(list(resultRule, fracturingRule))
            } else {
                ## Modify LHSrule expr and context to create two new rules.
                newSingleContexts1 <- singleContexts[!focalContext]
                newSingleContexts2 <- singleContexts[!focalContext]
                newSingleContexts3 <- singleContexts[!focalContext]

                expr1 <- expr2 <- expr3 <- expr
                newSingleContexts1[[length(newSingleContexts1)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = LHS[[1]][[1]], B = frac[[1]][[1]]-1)))
                expr1[[nonIdenticalFullIndices+2]] <- newSingleContexts1[[length(newSingleContexts1)]]$indexVarExpr

                newSingleContexts2[[length(newSingleContexts2)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = frac[[1]][[2]]+1, B = LHS[[1]][[2]])))
                expr2[[nonIdenticalFullIndices+2]] <- newSingleContexts2[[length(newSingleContexts2)]]$indexVarExpr

                newSingleContexts3[[length(newSingleContexts3)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = frac[[1]][[1]], B = frac[[1]][[2]])))
                expr3[[nonIdenticalFullIndices+2]] <- newSingleContexts3[[length(newSingleContexts3)]]$indexVarExpr
               
                resultRule1 <- nodeRuleClass$new(expr1, FALSE, LHSrule$ID, LHSrule$stoch, context = modelContextClass$new(newSingleContexts1))
                resultRule2 <- nodeRuleClass$new(expr2, FALSE, LHSrule$ID, LHSrule$stoch, context = modelContextClass$new(newSingleContexts2))
                fracturingRule <- nodeRuleClass$new(expr2, FALSE, LHSrule$ID, LHSrule$stoch, context = modelContextClass$new(newSingleContexts3))
                fracturingRule$set('stochParent')
                
                return(list(resultRule1, resultRule2, fracturingRule))
            }
        }
    } else {     ## unroll, exclude, create new arbitrary LHSrule by creating a complicated context, crossed with any indices that are identical
        unrolledLHS <- LHSrange$externalRange$getIndexRangeMatrix(nonIdenticalExternalIndices)
        unrolledFrac <- fracturingRange$externalRange$getIndexRangeMatrix(nonIdenticalExternalIndices)

        lhsAsChar <- do.call(paste, as.data.frame(unrolledLHS[[1]]))
        fracAsChar <- do.call(paste, as.data.frame(unrolledFrac[[1]]))

        remaining <- which(!lhsAsChar %in% fracAsChar)
        mat1 <- unrolledLHS[[1]][remaining, ]
        mat2 <- unrolledLHS[[1]][!remaining, ]

        focalContext <- sapply(names(singleContexts), function(nm)
            nm %in% unlist(lapply(2+nonIdenticalFullIndices, function(x) all.vars(expr[[x]]))))

        if(sum(!focalContext)) {
            newSingleContexts1 <- singleContexts[!focalContext]
            newSingleContexts2 <- singleContexts[!focalContext]
        } else newSingleContexts1 <- newSingleContexts2 <- list()

        newSingleContexts1[[length(newSingleContexts1) + 1]] <- modelSingleContext(
                               indexVarExpr = quote(.newidx),
            indexRangeExpr = substitute(1:L, list(L = nrow(mat1))))

        newSingleContexts2[[length(newSingleContexts2) + 1]] <- modelSingleContext(
                               indexVarExpr = quote(.newidx),
            indexRangeExpr = substitute(1:L, list(L = nrow(mat2))))

        nms <- paste0(".idx", seq_len(ncol(mat1)), "[.newidx]")
        for(i in seq_along(nonIdenticalFullIndices)) 
            expr[[nonIdenticalFullIndices[i]+2]] <- parse(text = nms[i])[[1]]
        
        constants1 <- lapply(seq_len(ncol(mat1)), function(i) mat[,i])
        names(constants1) <- paste0(".idx", seq_along(constants1))
        constants2 <- lapply(seq_len(ncol(mat2)), function(i) mat2[,i])
        names(constants2) <- paste0(".idx", seq_along(constants2))

      
        resultRule <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context = modelContextClass$new(newSingleContexts1),
                                        constants = constants1)
        fracturingRule <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context = modelContextClass$new(newSingleContexts2),
                                        constants = constants2)
        fracturingRule$set('stochParent')

        return(list(resultRule, fracturingRule))
    }
}


if(FALSE) {  # initial testing of fracture()
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
   
   ## scalar overlap at end
   LHS <- quote(mu[i+1])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
   ## fracture with mu[3]
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(3))))
   
   result <- fracture(LHSrule, fracRange)

   expect_identical(length(result), 2L)
   expr <- quote(mu[i])
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:9){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:3){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)

   ## seq overlap at end
   LHS <- quote(mu[i+1])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
   ## fracture with mu[3:4]
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(3:4)))))
   
   result <- fracture(LHSrule, fracRange)

   expect_identical(length(result), 2L)
   expr <- quote(mu[i])
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 5:9){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:4){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
      
   ## seq overlap in middle
   LHS <- quote(mu[i+1])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
   ## fracture with mu[4:5]
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(4:5)))))
   
   result <- fracture(LHSrule, fracRange)

   expect_identical(length(result), 3L)
   expr <- quote(mu[i])
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:3){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 6:9){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:5){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[3]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
      

   ## seq and matrix
   LHS <- quote(mu[i+1])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
   ## fracture with matrix
   idx <- c(3,6,9)
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(matrix(idx)))))
   
   result <- fracture(LHSrule, fracRange)

   expect_identical(length(result), 2L)
   expr <- quote(mu[idx[i]])
   idx2 <- c(4,5,7,8)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:4){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp, constants = list(idx = idx2))
   expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 1:3){}))))
   expected <- nodeRuleClass$new(expr, TRUE, 1, FALSE, context_tmp, constants = list(idx = idx))
   expect_equal(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                expected$externalRules$indexRules[[1]]$setupResults)

   ## basic case with one external, one internal: mu[1:3, i]
   LHS <- quote(mu[1:3,i])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_i)
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                     indexRange(quote(2:3)))))
   
   result <- fracture(LHSrule, fracRange)

   expect_identical(length(result), 2L)
   expect_identical(result[[1]]$internalRules$indexRules[[1]]$setupResults,
                LHSrule$internalRules$indexRules[[1]]$setupResults)
   expect_identical(result[[2]]$internalRules$indexRules[[1]]$setupResults,
                    LHSrule$internalRules$indexRules[[1]]$setupResults)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 4:8){}))))
   expected <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)
   context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:3){}))))
   expected <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_tmp)
   expect_identical(result[[2]]$externalRules$indexRules[[1]]$setupResults,
                    expected$externalRules$indexRules[[1]]$setupResults)

   ## HERE - ok?
   ## two external indices, one fractured, two constant internal rules
   LHS <- quote(mu[1:3,j,i,2:3])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij)
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                     indexRange(quote(1:4)),
                                                     indexRange(matrix(c(2,4))),
                                                     indexRange(2))))
   
   result <- fracture(LHSrule, fracRange)

   ## FAILING because (2) is external but constant and has index2setID=0
   ## two external indices, one fractured, one constant internal rule and additional external from scalar
   LHS <- quote(mu[1:3,j,i,2])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij)
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                     indexRange(quote(1:4)),
                                                     indexRange(matrix(c(2,4))),
                                                     indexRange(2))))
   
   result <- fracture(LHSrule, fracRange)

   ## this is the root of the issue:
   LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                     indexRange(quote(1:4)),
                                                     indexRange(quote(2:8)),
                                        indexRange(2)))
   ## Error in indexRules[[iSet + numSets]] : subscript out of bounds

   ## two external indices, both fractured: mu[1:3, j ,2, i] 
   LHS <- quote(mu[1:3,j,i,2])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij)
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                     indexRange(quote(2:3)))))
   
   result <- fracture(LHSrule, fracRange)

   ## two external indices fractured: mu[1:3, j ,2, i] , based on 2-d matrix
   LHS <- quote(mu[1:3,j,i,2])
   LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij)
   fracRange <- LHSrule$apply(varRangeClass$new(list(indexRange(quote(1:3)),
                                                     indexRange(matrix(c(2,3,3,7), ncol = 2)),
                                                     indexRange(2))))

   ## mu[1:3, j, 2, i] # only j fractured, i identical
   
}

## Takes a LHS rule (created from original LHS of an expression) and intersects it with a LHS rule.
## Result can be:
##  - no intersection: RHS passed through
##  - RHS is fully in LHS: NULL
##  - partly intersects: fracture and return one or more fractured RHS rules

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
                ## Shrink existing index block
                if(int[[1]][[1]] == RHS[[1]][[1]]) 
                    RHS[[1]][[1]] <- int[[1]][[2]]+1 else RHS[[1]][[2]] <- int[[1]][[1]]-1

                newSingleContexts <- singleContexts[!focalContext]
                newSingleContexts[[length(newSingleContexts)+1]] <- modelSingleContext(
                    indexVarExpr = quote(.newidx),
                    indexRangeExpr = substitute(A:B, list(A = RHS[[1]][[1]], B = RHS[[1]][[2]])))
                expr[[nonIdenticalIndices+2]] <- newSingleContexts[[length(newSingleContexts)]]$indexVarExpr

                resultRule <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context = modelContextClass$new(newSingleContexts))
                return(list(resultRule))
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
        return(list(resultRule))
     }
}

## need tests for:
## setting up LHS node rules
## setting up RHS rules
## applying rules to get nodeRanges

if(FALSE) {
    ## Hopefully comprehensive testing of exclude(); move into test-nodeRules.R or test-exclude.R
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
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[1]]$setupResults,
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
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     RHSrule$externalRules$indexRules[[3]]$setupResults)
    expect_identical(result[[1]]$externalRules$indexRules[[3]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)
    expect_identical(result[[1]]$index2setID, c(1,3,2))

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
    expr <- quote(mu[idx1[i], j, idx2[i]])
    expected <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
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
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 3:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:4){}))))
    expected <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_tmp, constants = list(idx1 = idx1,idx2=idx2))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[2]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[1]]$setupResults)

    ## 3-d case with multi-column unshared matrix indexRange
    RHS <- quote(mu[idx1[j], i, idx2[j]])
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij, constants = list(idx1 = idx1, idx2 = idx2))
                                              
    idx3 <- c(4,7,2,2)
    idx4 <- c(1,10,3,2)
    LHS <- quote(mu[idx3[j], i, idx4[j]])
    LHSrule <- nodeRuleClass$new(LHS, TRUE, 1, FALSE, context_ij, constants = list(idx3 = idx3, idx4 = idx4))
    
    result <- exclude(RHSrule, LHSrule)

    context_tmp <- modelContextClass$new(list(modelSingleContext(forCode = quote(for(i in 2:8){})),
                                              modelSingleContext(forCode = quote(for(j in 1:2){}))))
    idx5 <- c(1,7)
    idx6 <- c(3,9)
    expr <- quote(mu[idx5[j], i, idx6[j]])
    expected <- nodeRuleClass$new(expr, FALSE, 1, FALSE, context_tmp, constants = list(idx5 = idx5, idx6 = idx6))
    expect_equal(result[[1]]$externalRules$indexRules[[1]]$setupResults,
                     RHSrule$externalRules$indexRules[[1]]$setupResults)
    expect_equal(result[[1]]$externalRules$indexRules[[2]]$setupResults,
                     expected$externalRules$indexRules[[2]]$setupResults)
    
    ## This is not error-trapped (use of index and constant in wrong way)
    RHS <- quote(mu[i[idx]])
    idx <- c(4,7,1)
    RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_i, constants = list(idx = idx))
    
    ## incorrect length of constant (move this check to test-graphRules, probably).
    RHS <- quote(mu[i,idx[j]])
    idx <- c(4,7,1)
    expect_error(RHSrule <- nodeRuleClass$new(RHS, FALSE, 1, FALSE, context_ij, constants = list(idx = idx)),
                 "Missing values found in setting up arbitrary indexRule")
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


