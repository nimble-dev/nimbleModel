type2itype <- list(
    '0' = 0,
    "1_seq" = 1,
    "1_mat" = 2,
    "1_matp" = 3,
    "2_seq_seq" = 4,
    "2_seq_mat" = 5,
    "2_mat_seq" = 6,
    "2_mat_mat" = 7,
    "2_seq_matp" = 8,
    "2_matp_seq" = 9,
    "2_matp_matp" = 10,
    "2_mat_matp" = 11,
    "2_matp_mat" = 12,
    "3_generic" = 13 # Need to deal with itype for _slot cases.
)

## Stand-alone function for setting up inputs to instrClass constructor.
## This could be embedded in the constructor.
range2instr <- function(range) {
    instr <- list()  
    if(!length(range$indexingRange$indexRanges)) {  # No indexing
        instr$lens <- 1
        instr$index_types <- 0
        instr$dim <- 0
    } else {
        instr$lens <-  sapply(range$indexingRange$indexRanges, function(x) x$numElements)
        instr$dims <- sapply(range$indexingRange$rangeToIndexSlot, length)
        instr$dim <- sum(instr$dims)
        instr$slots <- unlist(range$indexingRange$rangeToIndexSlot)
        instr$index_types <- sapply(range$indexingRange$indexRanges, function(x)
            switch(class(x)[1],
                   "indexRangeScalarClass" = 2,
                   "indexRangeSequenceClass" = 1,
                   "indexRangeMatrixClass" = 2))
        instr$values <- lapply(range$indexingRange$indexRanges, function(x)
            switch(class(x)[1],
                   "indexRangeScalarClass" = x$value,
                   "indexRangeSequenceClass" = x$start-1,  # -1 to avoid constantly adding 1 in calculate()
                   "indexRangeMatrixClass" = c(t(matrix(x$values, nc = x$numColumns)))))  # in calcRange, column major; need row major here for simpler/more efficient determination of indices
    }
    instr$type <- determineInstrType(instr)
    instr$sortID <- range$sortID
    instr$declID <- range$declID
    return(instr)
}

## Eventually think about reordering order of looping for efficiency (and take parallelization into account).
## For the moment, we determine mat vs. seq here and then in declFunClass calculate we will determine whether to
## vectorize based on whether possible based on the declaration.
## Open question of when to determine if to use parallel calculate.
determineInstrType <- function(instr, use_vec = FALSE) {
    type <- NULL
    if(!length(instr$dims)) 
        type <- "0"
    if(length(instr$dims) == 1) 
        if(instr$index_types[1] == 1) {
            type <- "1_seq"
        } else {
            if(instr$dims[1] == 1) type <- "1_mat" else  type <- "1_matp"
        }
    if(length(instr$dims) == 2) 
        if(identical(instr$dims, c(1L,1L))) {
            ## Some of these not yet written.
            if(identical(instr$index_types, c(1,1)))
                type <- "2_vec_vec"
            if(identical(instr$index_types, c(1,2)))
                if(instr$dims[2] == 1) type <- "2_seq_mat" else type <- "2_seq_matp"
            if(identical(instr$index_types, c(2,1))) 
                if(instr$dims[1] == 1) type <- "2_mat_seq" else type <- "2_matp_seq"
            if(identical(instr$index_types, c(2,2))) {
                if(all(instr$dims == 1)) type <- "2_mat_mat"
                if(all(instr$dims == 2)) type <- "2_matp_matp"
                if(instr$dims[[1]] == 2) type <- "2_matp_mat"
                if(instr$dims[[2]] == 2) type <- "2_mat_matp"
            }
        } else type <- "2_generic"
    if(length(instr$dims) == 3) type <- "3_generic"
    if(is.null(type)) stop("no available specific instruction type")
    ## TODO: determine how much about slots will be pre-baked.
    if(length(instr$dims) && !identical(instr$slots, 1:instr$dim))  # Non-canonical slot ordering
        type <- paste(type, "slots", sep = "_")  
    return(type2itype[[type]])
}

#' @export
makeInstrList <- function(model, varRanges, use_vec = FALSE) {
    if(missing(varRanges))
        varRanges <- model$getVarNames()
    ## This works with a char vector of "nodes" or a list of (or single) varRanges
    if(is(varRanges, 'varRangeClass')) varRanges <- list(varRanges)
    ## First apply calcRule to get overlap between input and the rule.
    ## Then make the calcRange to convert to loop indexing.
    ## Note that `calcRule$apply` handles converting char to varRange and handling full variable extent.
    ranges <- unlist(lapply(varRanges, function(vr)
        lapply(model$modelDef$calcRules[[nimbleModel:::getVarName(vr)]]$rules, function(rule)
            rule$makeCalcRange(rule$apply(vr))
            )))
    instrList <- nList(instr_nClass)$new()
    numRanges <- length(ranges)
    instrList$setLength(numRanges)
    ord <- order(unlist(lapply(ranges, function(x) x$sortID)))
    for(i in 1:numRanges)
        instrList[[i]] <- instr_nClass$new(ranges[[ord[i]]])
    return(instrList)
}

instr_nClass <- nClass(
  classname = "instr_nClass",
  Rpublic = list(
      initialize = function(calcRange) {
          instrList <- range2instr(calcRange)  # This processing could simply be included here in `initialize`.
          self$lens <- instrList$lens
          self$index_types <- instrList$index_types
          self$dim <- instrList$dim
          self$dims <- instrList$dims
          self$slots <- instrList$slots
          self$values <- nList(integerVector)$new()
          self$values$setLength(length(self$dims))
          if(self$dim)
              for(i in 1:length(self$dims))
                  self$values[[i]] <- instrList$values[[i]]
          self$type <- instrList$type    # Use integer for compilation (would char be ok?).
          self$sortID <- instrList$sortID
          self$declID <- instrList$declID
      }),      
  Cpublic = list(
    lens = 'integerVector',
    index_types = 'integerVector',
    dim = 'integerScalar',
    dims =  'integerVector',
    slots =  'integerVector',
    values = 'nList(integerVector)', 
    type = 'integerScalar',
    sortID = 'integerVector',
    declID = 'integerScalar'
  ),
  predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |> file.path("instr_nClass")),
  compileInfo = list(interface = "full",
                        createFromR = FALSE,
                        exportName = "instr_nClass_new",
                        packageNames = c(uncompiled="instr_nClass_R", compiled="instr_nClass")
                        )
)



