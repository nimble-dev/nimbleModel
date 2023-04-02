dimOrLength <- function(obj, scalarize = FALSE) {
    if(scalarize) if(length(obj) == 1) return(integer(0))
    if(is.null(dim(obj))) return(length(obj))
    return(dim(obj))
}
