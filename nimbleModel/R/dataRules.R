## The dataRuleClass represents the indexing of data nodes (or non-data nodes).

## Perhaps should be called `isDataRuleClass`.

## TODO: this sets `rule` field as NULL if the rule
## is "empty" (representing no indices).
## May need to revisit this behavior.

dataRuleClass <- R6Class(
    classname = 'dataRuleClass',
    portable = FALSE,
    public = list(
        rule = NULL,
        varName = NULL,
        nondataRule = NULL,

        initialize = function(x, varName, nondataRule = FALSE) {
            varName <<- varName
            nondataRule <<- nondataRule  ## inverse case, used for `includeData = FALSE`
            
            NAs <- is.na(x)
            if(any(NAs)) {
                if(all(NAs)) {  # Only nondataRule relevant
                    if(!nondataRule)
                        return(NULL)
                    rulePieces <- makeRulePieces(NAs, varName)
                } else {       # Both relevant
                    if(nondataRule) {
                        rulePieces <- makeRulePieces(NAs, varName, all = FALSE)
                    } else rulePieces <- makeRulePieces(!NAs, varName, all = FALSE)
                }
            } else {           # Only dataRule relevant
                if(nondataRule)
                    return(NULL)
                rulePieces <- makeRulePieces(NAs, varName)
            }
            ## We'll use graphRules, though only have a single "side".
            rule <<- graphRuleClass$new(rulePieces$expr, rulePieces$expr,
                                        context = modelContextClass$new(rulePieces$singleContexts),
                                            constants = rulePieces$constants)
        },

        apply = function(varRange = NULL) {
            if(is.null(varRange)) 
                varRange <- varName                
            name <- getVarName(varRange)
            if(name != varName)  # variable names don't match
                return(NULL)  
            if(is.character(varRange)) 
                if(name == varRange) {
                    varRange <- rule$getFromRange()  # e.g., 'y' -- produce full range of the rule
                } else varRange <- varRangeClass$new(varRange)  # e.g., 'y[1:3]'

            rule$apply(varRange)
        }

    )
)


makeRulePieces <- function(NAs, varName, all = TRUE) {
    d <- dimOrLength(NAs)
    if(all) {   ## Full 'rectangular' extent.
        idxNames <- paste0("idx", seq_along(d))
        singleContexts <- lapply(seq_along(d), function(i)
            singleContextClass$new(
                                   indexVarExpr = as.name(idxNames[i]),
                                   indexRangeExpr = substitute(1:L, list(L = d[i]))))

        expr <- quote(y[i])
        expr[[2]] <- as.name(varName)
        expr[3:(2+length(d))] <- lapply(idxNames, as.name)

        constants = list()
    } else {
        singleContexts <- list(
            singleContextClass$new(
                                   indexVarExpr = as.name("idx"),
                                   indexRangeExpr = substitute(1:L, list(L = sum(NAs)))))

        expr <- quote(y[i])
        expr[[2]] <- as.name(varName)
        newcode <- paste0("k", seq_along(d), "[idx]")
        expr[3:(2+length(d))] <- parse(text = newcode)

        inds <- which(NAs, arr.ind = TRUE)
        if(!is.array(inds))
            if(length(d) == 1) {
                inds <- matrix(inds, ncol = 1)
            } else inds <- matrix(inds, nrow = 1)  # Shouldn't ever be needed.
        constants = lapply(seq_len(ncol(inds)), function(i)
            inds[ , i])
        names(constants) <- paste0("k", seq_along(d))
    }
    return(list(expr = expr, singleContexts = singleContexts, constants = constants))
}
