## TODO: various functions in the version of this file in nimble need to be ported to nimbleModel. still use old model graph API.



# for now export this as R<3.1.2 give warnings if don't

#' Class \code{codeBlockClass}
#' @aliases codeBlockClass
#' @export
#' @description
#' Classes used internally in NIMBLE and not expected to be called directly by users.
codeBlockClass <- setRefClass(
    Class   = 'codeBlockClass',
    fields  = list(codeBlock = 'ANY'),
    methods = list(
        
        initialize = function()     codeBlock <<- quote({}),
        
        addCode = function(expr, substituteList = NULL, quote = TRUE) {
            if(quote) expr <- substitute(expr)
            if(!is.null(substituteList)) {
                expr <- eval(substitute(substitute(EXPR, substituteList), list(EXPR=expr)))
            }
            if(is.call(expr) && expr[[1]] == '{') {  ## expr is a block of code
                for(i in seq_along(expr)[-1]) {
                    codeBlock[[length(codeBlock) + 1]] <<- expr[[i]]
                }
            } else {  ## expr is a single expression
                codeBlock[[length(codeBlock) + 1]] <<- expr
            }
        },
        
        getCode = function()     return(codeBlock)
    )
    
)

mcmc_generateControlListArgument <- function(control, controlDefaults) {
    if(missing(control))           control         <- list()
    if(missing(controlDefaults))   controlDefaults <- list()
    thisControlList <- controlDefaults           ## start with all the defaults
    thisControlList[names(control)] <- control   ## add in any controls provided as an argument
    return(thisControlList)
}



mcmc_listContentsToStr <- function(ls, displayControlDefaults=FALSE, displayNonScalars=FALSE, displayConjugateDependencies=FALSE) {
    ##if(any(unlist(lapply(ls, is.function)))) warning('probably provided wrong type of function argument')
    if(!displayConjugateDependencies) {
        if(grepl('^conjugate_d', names(ls)[1])) ls <- ls[1]    ## for conjugate samplers, remove all 'dep_dnorm', etc, control elements (don't print them!)
        if(grepl('^CRP_cluster_wrapper', names(ls)[1]) && 'wrapped_type' %in% names(ls) &&
           grepl('conjugate_d', ls$wrapped_type)[1]) ls <- ls[!names(ls) %in% c('wrapped_conf')]
    }
    if((names(ls)[1] == 'CRP sampler') && ('clusterVarInfo' %in% names(ls)))   ls[which(names(ls) == 'clusterVarInfo')] <- NULL   ## remove 'clusterVarInfo' from CRP sampler printing
    ls <- lapply(ls, function(el) if(is.nf(el) || is.function(el)) 'function' else el)   ## functions -> 'function'
    ls2 <- list()
    ## to make displayControlDefaults argument work again, would need to code process
    ## setup function of each sampler, find & extract the default control values, and pass
    ## them into this function.  Then set:
    ## defaultOptions <- [the default list for this sampler]
    ## (the commented function below, mcmc_findControlListNamesInCode, is a helpful start).
    ## old roxygen documentation for displayControlDefaults (from conf$print() method):
    ## displayControlDefaults: A logical argument, specifying whether to display default values of control list elements (default FALSE).
    ## -DT July 2017
    for(i in seq_along(ls)) {
        controlName <- names(ls)[i]
        controlValue <- ls[[i]]
        if(length(controlValue) == 0) next   ## remove length 0
        ##if(!displayControlDefaults)
        ##    if(controlName %in% names(defaultOptions))   ## skip default control values
        ##        if(identical(controlValue, defaultOptions[[controlName]])) next
        if(!displayNonScalars)
            if(is.numeric(controlValue) || is.logical(controlValue))
                if(length(controlValue) > 1)
                    controlValue <- ifelse(is.null(dim(controlValue)), 'custom vector', 'custom array')
        if(inherits(controlValue, 'samplerConf'))   controlValue <- controlValue$name
        deparsedItem <- safeDeparse(controlValue, warn = TRUE)
        if(length(deparsedItem) > 1) deparsedItem <- paste0(deparsedItem, collapse='')
        ls2[[i]] <- paste0(controlName, ': ', deparsedItem)
    }
    ls2 <- ls2[unlist(lapply(ls2, function(i) !is.null(i)))]
    str <- paste0(ls2, collapse = ',  ')
    ##if(length(ls2) == 1)
    ##    str <- paste0(str, ', default')
    str <- gsub('\"', '', str)
    str <- gsub('c\\((.*?)\\)', '\\1', str)
    return(str)
}


#' Extract named elements from MCMC sampler control list
#'
#' @param controlList control list object, which is passed as an argument to all MCMC sampler setup functions.
#' @param elementName character string, giving the name of the element to be extracted from the control list.
#' @param defaultValue default value of the control list element, giving the value to be used when the \code{elementName} does not exactly match the name of an element in the \code{controlList}.
#' @param error character string, giving the error message to be printed if no \code{defaultValue} is provided and \code{elementName} does not match the name of an element in the \code{controlList}.
#' @return The element of \code{controlList} whose name matches \code{elementName}. If no \code{controlList} name matches \code{elementName}, then \code{defaultValue} is returned.
#' @author Daniel Turek
#' @export
extractControlElement <- function(controlList, elementName, defaultValue, error) {
    if(missing(controlList) | !is.list(controlList))      stop('extractControlElement: controlList argument must be a list')
    if(missing(elementName) | !is.character(elementName)) stop('extractControlElement: elementName argument must be a character string variable name')
    if(missing(defaultValue) & missing(error))            stop('extractControlElement: must provide either defaultValue or error argument')
    if(elementName %in% names(controlList)) {
        return(controlList[[elementName]])
    } else {
        if(!missing(defaultValue)) return(defaultValue) else stop(error)
    }
}

 
