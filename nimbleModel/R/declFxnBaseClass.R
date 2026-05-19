#' @export
declFxnBase_nClass <- nClass(
  classname = "declFxnBase_nClass",
  Cpublic = list(
    ## model = 'modelBase_nClass',
    ping = nFunction(
      name = "ping",
      function() {return(TRUE); returnType(logical())},
      compileInfo = list(virtual=TRUE)
    ),
    calculate = nFunction(
      name = "calculate",
      fun = function(instr = 'instr_nClass') {
          ## TODO: how embed determination of vec and parallel cases here?
          if(instr$type == 0) return(calc_0(instr))
          if(instr$type == 1) return(calc_1_seq(instr))
          if(instr$type == 2) return(calc_1_mat(instr))
          if(instr$type == 3) return(calc_1_matp(instr))
          return(0)  ## Need to error trap/warn if unhandled type requested
      }, returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE)
    ),    
    ## TODO: for all these type-specific calculates, how do we call the methods of the declFxn_nClass object?
    calc_0 = nFunction(
        name = 'calc_0',
        function(instr = 'instr_nClass') {
            ## Presumably this will have access to derive class' `calc_one`?
            return(calc_one(0))  ## calc_one will always has `idx` as arg?
        }, returnType = 'numericScalar'
    ),
    calc_1_seq = nFunction(
        name = 'calc_1_seq',
        function(instr = 'instr_nClass') {
            logProb = 0
            for(i in 1:instr$lens[1])
                logProb <- logProb + calc_one(instr$values[[1]][1]+i)
            return(logProb)
        }, returnType = 'numericScalar'
    ),
    calc_1_mat = nFunction(
        name = 'calc_1_mat',
        function(instr = 'instr_nClass') {
            logProb = 0
            for(i in 1:instr$lens[1])
                logProb <- logProb + calc_one(instr$values[[1]][i])
            return(logProb)
        }, returnType = 'numericScalar'
    ),
    calc_1_matp = nFunction(
        name = 'calc_1_mat',
        function(instr = 'instr_nClass') {
            logProb = 0
            for(i in 1:instr$lens[1])
                logProb <- logProb + calc_one(instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])  ## Ok to call with a vector?
            return(logProb)
        }, returnType = 'numericScalar'
    )
  ),
  ## We haven't dealt with ensuring a virtual destructor when any method is virtual
  ## For now I did it manually by editing the .h and .cpp
  predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |>
               file.path("declFxnBase_nC")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   exportName = "declFxnBase_nClass_new",
                   needed_units = list("nodeInstr_nClass"),
                   packageNames = c(uncompiled="declFxnBase_nClass_R", compiled="declFxnBase_nClass")
                   )
)
