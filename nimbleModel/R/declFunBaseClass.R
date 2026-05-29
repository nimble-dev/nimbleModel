#' @export
declFunBase_nClass <- nClass(
  classname = "declFunBase_nClass",
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
      #    if(instr$type == 1) return(calc_1_seq(instr))
      #    if(instr$type == 2) return(calc_1_mat(instr))
      #    if(instr$type == 3) return(calc_1_matp(instr))
          return(0)  ## Need to error trap/warn if unhandled type requested
      }, returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE)
    ),    
    calc_0 = nFunction(
        name = 'calc_0',
        function(instr = 'instr_nClass') {
            return(calc_one(0))  ## calc_one will always has `idx` as arg?
        }, 
        returnType = 'numericScalar',
        compileInfo = list(
            C_fun = function(instr = 'instr_nClass') {
                cppLiteral('Rprintf("declFunBase_nClass calc_0 (should not see this)\\n");'); return(0)
            },
            virtual=TRUE
        )
    )#,
    # calc_1_seq = nFunction(
    #     name = 'calc_1_seq',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + calc_one(instr$values[[1]][1]+i)
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    # calc_1_mat = nFunction(
    #     name = 'calc_1_mat',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + calc_one(instr$values[[1]][i])
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    # calc_1_matp = nFunction(
    #     name = 'calc_1_mat',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + calc_one(instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])  ## Ok to call with a vector?
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    
    # calculateDiff = nFunction(
    #   name = "calculateDiff",
    #   fun = function(instr = 'instr_nClass') {
    #       ## TODO: how embed determination of vec and parallel cases here?
    #       if(instr$type == 0) return(calcDiff_0(instr))
    #       if(instr$type == 1) return(calcDiff_1_seq(instr))
    #       if(instr$type == 2) return(calcDiff_1_mat(instr))
    #       if(instr$type == 3) return(calcDiff_1_matp(instr))
    #       return(0)  ## Need to error trap/warn if unhandled type requested
    #   }, returnType = 'numericScalar',
    #   compileInfo = list(virtual=TRUE)
    # ),    
    # calcDiff_0 = nFunction(
    #     name = 'calcDiff_0',
    #     function(instr = 'instr_nClass') {
    #         return(calcDiff_one(0))  ## calcDiff_one will always has `idx` as arg?
    #     }, returnType = 'numericScalar'
    # ),
    # calcDiff_1_seq = nFunction(
    #     name = 'calcDiff_1_seq',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + calcDiff_one(instr$values[[1]][1]+i)
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    # calcDiff_1_mat = nFunction(
    #     name = 'calcDiff_1_mat',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + calcDiff_one(instr$values[[1]][i])
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    # calcDiff_1_matp = nFunction(
    #     name = 'calcDiff_1_mat',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + calcDiff_one(instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])  ## Ok to call with a vector?
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    
    # simulate = nFunction(
    #   name = "simulate",
    #   fun = function(instr = 'instr_nClass') {
    #       ## TODO: how embed determination of vec and parallel cases here?
    #       if(instr$type == 0) sim_0(instr)
    #       if(instr$type == 1) sim_1_seq(instr)
    #       if(instr$type == 2) sim_1_mat(instr)
    #       if(instr$type == 3) sim_1_matp(instr)
    #   },
    #   compileInfo = list(virtual=TRUE)
    # ),    
    # sim_0 = nFunction(
    #     name = 'sim_0',
    #     function(instr = 'instr_nClass') {
    #         sim_one(0) ## sim_one will always has `idx` as arg?
    #     }
    # ),
    # sim_1_seq = nFunction(
    #     name = 'sim_1_seq',
    #     function(instr = 'instr_nClass') {
    #         for(i in 1:instr$lens[1])
    #             sim_one(instr$values[[1]][1]+i)
    #     }
    # ),
    # sim_1_mat = nFunction(
    #     name = 'sim_1_mat',
    #     function(instr = 'instr_nClass') {
    #         for(i in 1:instr$lens[1])
    #             sim_one(instr$values[[1]][i])
    #     }
    # ),
    # sim_1_matp = nFunction(
    #     name = 'sim_1_mat',
    #     function(instr = 'instr_nClass') {
    #         for(i in 1:instr$lens[1])
    #             sim_one(instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])  ## Ok to call with a vector?
    #     }
    # ),

    # getLogProb = nFunction(
    #   name = "getLogProb",
    #   fun = function(instr = 'instr_nClass') {
    #       ## TODO: how embed determination of vec and parallel cases here?
    #       if(instr$type == 0) return(getLogProb_0(instr))
    #       if(instr$type == 1) return(getLogProb_1_seq(instr))
    #       if(instr$type == 2) return(getLogProb_1_mat(instr))
    #       if(instr$type == 3) return(getLogProb_1_matp(instr))
    #       return(0)  ## Need to error trap/warn if unhandled type requested
    #   }, returnType = 'numericScalar',
    #   compileInfo = list(virtual=TRUE)
    # ),    
    # getLogProb_0 = nFunction(
    #     name = 'getLogProb_0',
    #     function(instr = 'instr_nClass') {
    #         return(getLogProb_one(0))  ## getLogProb_one will always has `idx` as arg?
    #     }, returnType = 'numericScalar'
    # ),
    # getLogProb_1_seq = nFunction(
    #     name = 'getLogProb_1_seq',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + getLogProb_one(instr$values[[1]][1]+i)
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    # getLogProb_1_mat = nFunction(
    #     name = 'getLogProb_1_mat',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + getLogProb_one(instr$values[[1]][i])
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # ),
    # getLogProb_1_matp = nFunction(
    #     name = 'getLogProb_1_mat',
    #     function(instr = 'instr_nClass') {
    #         logProb = 0
    #         for(i in 1:instr$lens[1])
    #             logProb <- logProb + getLogProb_one(instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])  ## Ok to call with a vector?
    #         return(logProb)
    #     }, returnType = 'numericScalar'
    # )
    
  ),
  ## We haven't dealt with ensuring a virtual destructor when any method is virtual
  ## For now I did it manually by editing the .h and .cpp
  predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |>
               file.path("declFunBase_nC")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   exportName = "declFunBase_nClass_new",
                   needed_units = list("instr_nClass"),
                   packageNames = c(uncompiled="declFunBase_nClass_R", compiled="declFunBase_nClass")
                   )
)
