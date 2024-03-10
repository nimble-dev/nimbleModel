## Classes for representing constraints on the `from` indexing.

indexConstraintClass <- R6Class(
    classname = 'indexRuleClass',
    portable = FALSE,
    public = list(
        slots = numeric()
    )
)
## e.g. y[i] <- x[2],  <- x[2:4], <- x[(c(2,3,5)]

## also, y[i] -> x[2],  y[k[i]] -> x[2], y[k1[i],k2[i]] -> x[2]

## TODO: do we need to check input None in any of these next ones?

## `check` method will return single boolean when given scalar or sequence,
## and vector of booleans when given a matrix. The vector is needed
## in some cases when checking multiple constraints against an
## indexRange or when an indexRange has columns additional to the column(s)
## checked against the constraint.

## A class representing a constraint such as `x[2]` in  `y[i] <- x[2]`.
indexConstraintScalarClass <- R6Class(
    classname = 'indexConstraintScalarClass',
    inherit = indexConstraintClass,
    portable = FALSE,
    public = list(
        value = numeric(),
        initialize = function(value, slot) {
            value <<- value
            slots <<- slot
        },

        check = function(indexRange) {
            switch(class(indexRange)[1],
                   indexRangeScalarClass = indexRange$value == value,
                   indexRangeSequenceClass =
                       indexRange$start <= value & indexRange$end >= value,
                   indexRangeMatrixClass =
                       c(indexRange$values == value)
                   )                   
        },

        getMax = function() {
            return(value)
        }
    )
)


## A class representing a constraint such as `x[2:4]` in  `y[i] <- sum(x[2:4])`.
indexConstraintSequenceClass <- R6Class(
    classname = 'indexConstraintSequenceClass',
    inherit = indexConstraintClass,
    portable = FALSE,
    public = list(
        start = numeric(),
        end = numeric(),
        initialize = function(start, end, slot) {
            start <<- start
            end <<- end
            slots <<- slot
        },

        check = function(indexRange) {
            switch(class(indexRange)[1],
                   indexRangeScalarClass = indexRange$value >= start && indexRange$value <= end,
                   indexRangeSequenceClass =
                       indexRange$start <= end && indexRange$end >= start,
                   indexRangeMatrixClass =
                       (end == .Machine$integer.max && any(indexRange$values >= start)) ||
                       (end != .Machine$integer.max && any(indexRange$values >= start & indexRange$values <= end))
                   )                   
        },

        getMax = function() {
            return(end)
        }
    )
)
    
## A class representing a constraint such as `x[c(2,3,5)]` in  `y[i] <- sum(x[c(2,3,5)])`, or
## result of evaluating `k[i]` for `y[k[i]] -> x[2]`.
indexConstraintMatrix1dClass <- R6Class(
    classname = 'indexConstraintMatrix1dClass',
    inherit = indexConstraintClass,
    portable = FALSE,
    public = list(
        values = numeric(),
        initialize = function(values, slot) {
            values <<- values
            slots <<- slot
        },

        ## CHECK: this assumes a one-column matrix; can't think of cases where
        ## a constraint could be specified.
        check = function(indexRange) {
            switch(class(indexRange)[1],
                   indexRangeScalarClass = indexRange$value %in% values,
                   indexRangeSequenceClass = any(indexRange$start <= values & indexRange$end >= values),
                   indexRangeMatrixClass = indexRange$values %in% values
                   )                   
        },

        getMax = function() {
            return(max(values))
        }
    )
)

indexConstraintMatrixClass <- R6Class(
    classname = 'indexConstraintMatrixClass',
    inherit = indexConstraintClass,
    portable = FALSE,
    public = list(
        values = numeric(),
        numColumns = numeric(),
        initialize = function(values, slots) {
            values <<- values
            slots <<- slots
            numColumns <<- ncol(values)
        },

        check = function(indexRange) {
            switch(class(indexRange)[1],
                   indexRangeMatrixClass = checkFunction(indexRange),
                   stop("`indexRange` must be a matrix.")
                   )                   
        },

        ## Determine rows of input that satisfy constraint. (`%in%` doesn't work for matrix rows)
        checkFunction = function(indexRange) {
            mat1 <- cbind(indexRange$values, seq_len(nrow(indexRange$values)))
            mat2 <- cbind(values, rep(1, nrow(values)))
            result <- merge(mat1, mat2, by = seq_len(numColumns), all.x = TRUE)
            ord <- order(result[ , numColumns + 1])
            return(!is.na(result[ord , numColumns + 2]))
        },

        getMax = function() {
            return(apply(values, 2, max))
        }
     
    )
)

## Create an `indexConstraint` for simple expressions, such as `x[2]`, `x[2:4]`.    
newIndexConstraint_fromSimple <- function(expr, slot, constants) {
    if(is.call(expr) && expr[[1]] == ":") {   ## sequence case, e.g., `x[2:4]`
        return(indexConstraintSequenceClass$new(eval(expr[[2]], envir = constants),
                                          eval(expr[[3]], envir = constants),
                                          slot))
    } else {  ## various other cases such as `x[2]`, `x[k+1]`, `x[c(2,3,5)]`, `x[c(2,3,k)]`.
        if(expr[[1]] == "nimC")
            expr[[1]] <- quote(c)
        result <- eval(expr, envir = constants)
        if(length(result) == 1) {
            return(indexConstraintScalarClass$new(result, slot))
        } else
            return(indexConstraintMatrix1dClass$new(result, slot))
    }
}

## Create an `indexConstraint` from eval'ing loop(s).
newIndexConstraint_fromUnrolling <- function(fromIndexExprs, slots, context, constants) {
    values <- makeIndicesMatrix(fromIndexExprs, context, constants)
    if(ncol(values) == 1) {
        return(indexConstraintMatrix1dClass$new(values[ , 1], slots))
    } else
        return(indexConstraintMatrixClass$new(values, slots))
}


checkIndexConstraints <- function(varRange, indexConstraints) {
    result <- list(); length(result) <- length(varRange$indexRanges)    
    if(length(indexConstraints))
        for(i in seq_along(varRange$indexRanges)) {
            ## Don't check ranges already used in combination with another range for a single constraint.
            if(!length(result[[i]])) {
                rangeSlots <- varRange$rangeToIndexSlot[[i]]
                usedConstraints <- sapply(indexConstraints, function(x)
                    any(rangeSlots %in% x$slots))
                if(length(usedConstraints)) {
                    for(constraint in indexConstraints[usedConstraints]) {
                        if(!all(constraint$slots %in% rangeSlots)) {
                            ## Cross ranges involved in a single constraint and
                            ## assign result to all the input ranges.
                            
                            ## This will not have to deal with complicated cases where
                            ## additional slots in a range are not constrained, as
                            ## this is handled in `applyGraphRule` as `complicatedCrossing`.
                            neededRanges <- varRange$indexSlotToRange[constraint$slots]
                            valid <- constraint$check(varRange$extractIndexRange(constraint$slots))
                            for(j in neededRanges) {
                                if(!is.null(result[[j]]))
                                    stop("encountered a case that should have been fully crossed.")
                                result[[j]] <- valid
                            }
                        } else {
                            if(!all(rangeSlots %in% constraint$slots)) {  # pull out only slots needed
                                valid <- constraint$check(varRange$extractIndexRange(constraint$slots))
                            } else valid <- constraint$check(varRange$indexRanges[[i]])
                            ## Combine results from multiple constraints applied to columns of a single range.
                            if(is.null(result[[i]])) {
                                result[[i]] <- valid
                            } else result[[i]] <- result[[i]] & valid 
                        }
                    }
                    constraintSlots <- unlist(lapply(indexConstraints[usedConstraints], function(x) x$slots))
                    ## If range fully covers the slots of the constraints (and only those slots)
                    ## just need to record scalar boolean of validity as not used to
                    ## constrain result of an indexRule to the range.
                    if(identical(sort(rangeSlots), sort(constraintSlots)))
                        result[[i]] <- any(result[[i]])
                } 
            }
        }
    return(result)
}

## Unroll loop(s) to generate indexing information in arbitrary cases.

## FUTURE: Code copied from `indexRuleArbitrary`. Might see what could be factored out
## as a single function to avoid code duplication.
makeIndicesMatrix <- function(fromIndexExprs, context, constants) {
    fromIndexNames <- lapply(names(fromIndexExprs),
                             as.name)
    unrolledIndicesEnv <-
        expandContextAndReplacements(
            allReplacements = fromIndexExprs,
            allReplacementNameExpr = fromIndexNames,
            context = context,
            constants = constants
        )
    fromUnrolledResults <-
        lapply(names(fromIndexExprs),
               function(x) unrolledIndicesEnv[[x]])
    
    if(any(is.na(unlist(fromUnrolledResults))))
        stop("missing values found in setting up arbitrary index rule: are constants the correct size?")
    unrolledSize <- unrolledIndicesEnv$outputSize

    from_allScalar <- all(
        sapply(fromUnrolledResults,
               function(x) !is.list(x)))

    if(from_allScalar) {
        allIndices <- do.call("cbind",
                              fromUnrolledResults)
        iRow2toIndices <- split(allIndices,
                                1:unrolledSize) ## makes it a list
    } else {
        ## This will return a list in the right form.
        allIndicesList <- do.call("mapply",
                                  c(list(as.name("expand.grid")),
                                    fromUnrolledResults,
                                    list(SIMPLIFY = FALSE))
                                  )
        iRows <- rep(1:unrolledSize,
                     times = unlist(lapply(allIndicesList, nrow)))
        allIndices <- do.call("rbind", allIndicesList)
        iRow2toIndices <- split(allIndices,
                                iRows)
    }
    result <- as.matrix(do.call(rbind, iRow2toIndices))
    dimnames(result) <- NULL
    return(result)
    ## combineFun <- ifelse(length(iRow2toIndices[[1]]) == 1, c, rbind)
    ## return(do.call(combineFun, iRow2toIndices))
}
