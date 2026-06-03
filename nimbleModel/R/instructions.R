type2itype <- list(
    '0' = 0,
    "1_seq" = 1,
    "1_mat" = 2,
    "1_matp" = 3,
    "2_seq_seq" = 4,  # done
    "2_seq_mat" = 5,
    "2_mat_seq" = 6,
    "2_mat_mat" = 7,
    "2_seq_matp" = 8,
    "2_matp_seq" = 9,
    "2_mat_matp" = 10,
    "2_matp_mat" = 11,
    "2_matp_matp" = 12,     
    "3_seq_seq_seq" = 13,
    "3_generic" = 14, 
    "1_matp_ord" = 15,  # done
    ### Probably not needed given reordering when apply graph rules to create instrList. ###
    "2_seq_seq_ord" = 16,    # done
    "2_seq_mat_ord" = 17,
    "2_mat_seq_ord" = 18,
    "2_mat_mat_ord" = 19,
    #########################################################################################
    "2_seq_matp_ord" = 20,
    "2_matp_seq_ord" = 21,
    "2_mat_matp_ord" = 22,
    "2_matp_mat_ord" = 23,
    "2_matp_matp_ord" = 24,
    "3_generic_ord" = 25
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
                   "indexRangeSequenceClass" = x$start,  
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
            if(instr$dims[1] == 1) type <- "1_mat" else {
              if(identical(instr$slots, 1:length(instr$slots))) type <- "1_matp" else type <- "1_matp_ord"
            }              
        }
    if(length(instr$dims) == 2) 
        if(identical(instr$dims, c(1L,1L))) {
            ## Some of these not yet written.
            if(identical(instr$index_types, c(1,1)))
                if(identical(instr$slots, 1:2)) type <- "2_seq_seq" else type <- "2_seq_seq_ord"
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

## TODO: document this since it may be user-facing.
#' @export
makeInstrList <- function(model, input, use_vec = FALSE) {
    ## `model` simply must contain `modelDef`, so it can be a modelClass or modelBase_nClass object.
    ## This works with:
    ## (1) a char vector of "nodes"
    ## (2) a list of (or single) varRanges 
    ## (3) an nList of (or single) instr_nClass objects (assumed to be in sort order)
    ## (4) an R list of instr_nClass objects (not assumed to be in sort order)
    if(is(input, 'nList')) 
        if(!inherits(input[[1]], 'instr_nClass')) {
            stop("nList input to `makeInstrList` should contain `instr_nClass` objects")
        } else return(input)  # Idempotent case.
    if(is(input, 'instr_nClass'))
        input <- list(input)
    if(is.list(input) && all(sapply(input, function(x) inherits(x, 'instr_nClass')))) {
        ## Create sort-ordered nList.
        instrList <- nList(instr_nClass)$new()
        numInstrs <- length(input)
        instrList$setLength(numInstrs)
        ord <- order(unlist(lapply(input, function(x) x$sortID)))
        for(i in 1:numInstrs)
            instrList[[i]] <- input[[ord[i]]]
        return(instrList)
    }
    ## At this point we presumably are working with varRange(s).
    if(is(input, 'varRangeClass')) input <- list(input)
    ## First apply calcRule to get overlap between input and the rule.
    ## Then make the calcRange to convert to loop indexing.
    ## Note that `calcRule$apply` handles converting char to varRange and handling full variable extent.
    ranges <- unlist(lapply(input, function(vr)
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
      initialize = function(calcRange, ...) {
          super$initialize(...)
          if(!missing(calcRange)) {
            instr <- range2instr(calcRange)  # This processing could simply be included here in `initialize`.
            self$lens <- instr$lens %||% integer()
            self$index_types <- instr$index_types %||% integer()
            self$dim <- instr$dim %||% 0L
            self$dims <- instr$dims %||% integer()
            self$slots <- instr$slots %||% integer()
            self$values <- nList(integerVector)$new()
            self$values$setLength(length(self$dims))
            if(self$dim)
                for(i in 1:length(self$dims))
                    self$values[[i]] <- instr$values[[i]]
            self$type <- instr$type  %||% 0L   # Use integer for compilation (would char be ok?).
            self$sortID <- instr$sortID %||% integer()
            self$declID <- instr$declID %||% 0L
          }
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
    declID = 'integerScalar',
    instr_nClass = nFunction(
        function() {
            values <- nList(integerVector)$new()
        },
        compileInfo = list(constructor=TRUE)
    )
  ),
  predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |> file.path("instr_nClass")),
  compileInfo = list(interface = "full",
                     createFromR = TRUE,
                     exportName = "instr_nClass_new",
                     needed_units = list("nList(integerVector)"),
                     packageNames = c(uncompiled="instr_nClass_R", compiled="instr_nClass")
                    )
)



