## Class for indexRules, which are individual pieces of graphRules.

## An `indexRule` determines the mapping between index values (for one more
## more index positions) for a single `indexSet`.

indexRuleClass <- R6Class(
    classname = 'indexRuleClass',
    portable = FALSE,
    public = list(
    )
)

## Look for index value +/- offset.

## FUTURE: we might be able to use some old code to partially evaluate
## more complicated expressions, or possibly use Ryacas.
## However, it is not clear what more complicated expressions we
## want to handle. Perhaps something like `i+3+7`.
## Something like i*3 could possibly be handled to avoid
## full unrolling, but result can't be handled except as indexRangeMatrix anyway.

getOffset <- function(indexExpr,
                      indexVarName,
                      constantsEnv = new.env()) {
    offset <- 0

    if(is.name(indexExpr)) {   # `i`
        indexNameInExpr <- as.character(indexExpr)
    } else {
        if(!as.character(indexExpr[[1]]) %in% c('+','-'))
            return(NULL)
        indexSlot <- NULL
        ## e.g., `3+i`, `foo(k) + i`
        if(is.name(indexExpr[[3]]) && as.character(indexExpr[[1]]) == '+')
            indexSlot <- 3
        ## e.g., `i+3`, `i-3`, `i + foo(k)`
        if(is.name(indexExpr[[2]])) 
            indexSlot <- 2

        if(is.null(indexSlot))
            return(NULL)
        indexNameInExpr <- as.character(indexExpr[[indexSlot]])
        offsetExpr <- indexExpr
        offsetExpr[[indexSlot]] <- 0
        offset <- eval(offsetExpr, envir = constantsEnv)
    }
    if(indexNameInExpr != indexVarName)
        return(NULL)
    list(offset = offset)
}

## Classes for representing constraints on the `from` indexing.

indexConstraintClass <- R6Class(
    classname = 'indexRuleClass',
    portable = FALSE,
    public = list(
    )
)
## e.g. y[i] <- x[2],  <- x[2:4], <- x[(c(2,3,5)]

## also, y[i] -> x[2],  y[k[i]] -> x[2], y[k1[i],k2[i]] -> x[2]


indexConstraintNoneClass <- R6Class(
    classname = 'indexConstraintNoneClass',
    inherit = indexConstraintClass,
    portable = FALSE,
    public = list(
        check = function(indexRange)
            return(is(indexRange, "indexRangeNoneClass"))
    )
)

## TODO: do we need to check input None in any of these next ones?

indexConstraintScalarClass <- R6Class(
    classname = 'indexConstraintScalarClass',
    inherit = indexConstraintClass,
    portable = FALSE,
    public = list(
        value = numeric(),
        slot = numeric(),
        initialize = function(value, slot) {
            value <<- value
            slot <<- slot
        },

        check = function(indexRange, col = 1) {
            switch(class(indexRange)[1],
                   indexRangeScalarClass = indexRange$value == value,
                   indexRangeSequenceClass =
                       value >= indexRange$start && value <= indexRange$end,
                   indexRangeMatrixClass =
                       value %in% indexRange$values[ , col]
                   )                   
        }
    )
)


indexConstraintSequenceClass <- R6Class(
    classname = 'indexConstraintSequenceClass',
    inherit = indexConstraintClass,
    portable = FALSE,
    public = list(
        start = numeric(),
        end = numeric(),
        slot = numeric(),
        initialize = function(start, end, slot) {
            start <<- start
            end <<- end
            slot <<- slot
        },

        check = function(indexRange, col = 1) {
            switch(class(indexRange)[1],
                   indexRangeScalarClass = value >= start && value <= end,
                   indexRangeSequenceClass =
                       indexRange$start <= end && indexRange$end >= start,
                   indexRangeMatrixClass =
                       any(indexRange$values[ , col] %in% start:end)
                   )                   
        }
    )
)
    

indexConstraintMatrixClass

newIndexConstraint <- function(expr, slot) {


}

createIndexConstraints <- function(fromIndexExprs) {

}

checkIndexConstraints <- function(varRange, indexConstraints) {
}
