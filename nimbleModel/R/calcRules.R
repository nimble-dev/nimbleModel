## Note: still unclear on when nodeFuns will be generated and where stored.

calcRuleClass <- R6Class(
    classname = "calcRuleClass",
    portable = FALSE,
    public = list(
        varName = NULL,
        originalIndexRule = NULL,
        context = NULL,
        canonicalRange = NULL,
        sortID = NULL,

        ## not entirely clear what primary input is -- declaration?

        initialize = function(LHS, decl, context, constants = list()) {
            if(length(LHS) > 1)
                varName <<- LHS[[2]] else varName <<- LHS
            originalIndexRule <<- originalIndexRuleClass$new(LHS, context, constants)
            ## full range, for use with calculate applied to full var
            canonicalRule <- makeGraphIndexRules(LHS, LHS, context, constants)
            canonicalRange <<- applyGraphIndexRules(
                varRangeClass$new(lapply(seq_along(canonicalRule$indexSets$LHSindex2setID),
                    function(i) indexRange(quote(1:Inf)))), canonicalRule)
            context <<- context
            decl <<- decl 
        },

        apply = function(varRange) {
            browser()
            ## make sure we check validity of internal range values e.g., y[i, 3:6] that 3:6 is valid
            ## do we need internalRange as with nodeRules?
            if(length(varRange$indexRanges) == 1 && identical(attr(varRange$indexRanges[[1]], 'rangeType'), "none"))
                varRange <- canonicalRange
            indexingRange <- originalIndexRule$apply(varRange)
            if(isEmpty(indexingRange))
                return(NULL)
            result <- calcRangeClass$new(varName, indexingRange, context, decl, sortID)
            ## if empty, return NULL
            return(result)
        }
    )
)

## build nodefun on the fly when provided a varRange; check if it already exists; pass the fun into the range?


if(FALSE) {
isEmpty <- function(varRange) 
    any(sapply(varRange$indexRanges, function(x) identical(attr(x, 'rangeType'), 'empty')))


calcRule <- calcRuleClass$new(quote(y[i+1]), NULL, context_i)

calcRange <- calcRule$apply(varRangeClass$new(list(indexRange(quote(3:5)))))
expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(2:4)))))

calcRule <- calcRuleClass$new(quote(y[2:4, i+1]), NULL, context_i)

calcRange <- calcRule$apply(varRangeClass$new(list(indexRange(quote(2:4)), indexRange(quote(3:5)))))
expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(2:4)))))

calcRange <- calcRule$apply(varRangeClass$new(list(indexRange(quote(2)), indexRange(quote(3:5)))))
expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(2:4)))))

calcRange <- calcRule$apply(varRangeClass$new(list(indexRange(quote(6)), indexRange(quote(3:5)))))
expect_equal(calcRange, NULL)

calcRange <- calcRule$apply(varRangeClass$new(list(nimbleModel:::indexRange_none())))
expect_equal(calcRange$indexingRange,
                 varRangeClass$new(list(indexRange(quote(1:10)))))
}
