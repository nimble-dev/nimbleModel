## Note: still unclear if calcRules operate on varRanges or nodeRanges (or both)
## to produce calcRanges

calcRuleClass <- R6Class(
    classname = "calcRuleClass",
    portable = FALSE,
    inherit = nodeRuleClass,
    public = list(
        canonicalRange = NULL,
        declRule = NULL,  # multiple calcRules can share a declRule and its density function

        stochParent = FALSE,
        stochDep = FALSE,
        touchedDown = FALSE,
        touchedUp = FALSE,
        parents = numeric(),
        children = numeric(),

        top = FALSE,
        end = FALSE,
        ## latent is !top & !end

        ## should canonicalRange should be in terms of nodes not var?

        ## could calcRule just use declRule internal indexing?
        
        ## This code below needs to operate at the node level since calculation is done by indexing
        ## over nodes.
        ## Actually, indexing is done over original indexes for calculation, so need to think more about this.
        initialize = function(declRule = NULL, expr = NULL, ID, context, constants = list()) {
            ## If LHS is NULL, just use declRule internalRange

            ## This wastes some calculation by redoing the internal/external rule stuff
            ## even though at least the internal stuff already done in the declRule
            if(is.null(expr)) expr <<- declRule$expr
            super$initialize(expr, ID, context = context, constants = constants)

            ## full range, for use with calculate applied to full var

            ## canonicalRule <- makeGraphIndexRules(expr, expr, context, constants)
            canonicalRange <<- applyGraphIndexRules(
                varRangeClass$new(lapply(seq_along(allRules$indexSets$LHSindex2setID),
                    function(i) indexRange(quote(1:Inf)))), allRules)
            context <<- context
            declRule <<- declRule

        },

        ## nodeRuleClass$apply generates a nodeRange
        
        ## Generate calcRange
        generate_calcRange = function(varRange) {
            ## make sure we check validity of internal range values e.g., y[i, 3:6] that 3:6 is valid
            ## do we need internalRange as with nodeRules?
            ## Can we generate a calcRange from a nodeRange or only a varRange?
            if(length(varRange$indexRanges) == 1 && identical(attr(varRange$indexRanges[[1]], 'rangeType'), "none"))
                varRange <- canonicalRange
            indexingRange <- declRule$originalIndexRules$apply(varRange)
            if(isEmpty(indexingRange))
                return(NULL)
            result <- calcRangeClass$new(varName, indexingRange, declRule$calcFun, sortID)
            ## if empty, return NULL
            return(result)
        },

        get = function(varRange = NULL, type) {
            ## type is 'end', 'latent', etc.
            ## returns the embedded nodeRange (or subset of it if provided a varRange) that corresponds to 'type'
        },
        
        is_type = function(type) {
            switch(type,
                end = return(end),
                top = return(top),
                latent = return(!end && !top),
                stop("Invalid type ", type)
            )
        },

        set = function(type) {
            switch(type,
                end = end <<- TRUE,
                top = top <<- TRUE,
                stochParent = stochParent <<- TRUE,
                stop("Invalid type ", type)
            )            
        },

        unset = function(type) {
            switch(type,
                end = end <<- FALSE,
                top = top <<- FALSE,
                stochParent = stochParent <<- FALSE,
                stop("Invalid type ", type)
            )
        },

        get_sortID = function() {
            return(declRule$sortID)
        }
    )
)

## build nodefun on the fly when provided a varRange; check if it already exists; pass the fun into the range?
## actually I think it can all be pre-generated

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
