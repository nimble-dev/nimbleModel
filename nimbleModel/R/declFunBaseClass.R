#' @export
declFunBase_nClass <- nClass(
  classname = "declFunBase_nClass",
  Rpublic = list(
    calculate = function(instr) {
      calc_op(instr, "calc_one")
    },
    calculateDiff = function(instr) {
      calc_op(instr, "calcDiff_one")
    },
    getLogProb = function(instr) {
      calc_op(instr, "getLogProb_one")
    },
    calc_op = function(instr, fn) {
      if(instr$type == 0) return(calc_0(instr, fn))
      if(instr$type == 1) return(calc_1_seq(instr, fn))
      if(instr$type == 2) return(calc_1_mat(instr, fn))
      if(instr$type == 3) return(calc_1_matp(instr, fn))
      return(0)
    },
    calc_0 = function(instr, fn) {
      return(self[[fn]](0))
    },
    calc_1_seq = 
    function(instr, fn) {
      logProb = 0
      iStart <- instr$values[[1]][1] # Values seem to start offset by -1, a bit confusing
      for(i in 1:instr$lens[1])
        logProb <- logProb + self[[fn]](iStart + i)
      return(logProb)
    },
    calc_1_mat = 
      function(instr, fn) {
        logProb = 0
        for(i in 1:instr$lens[1])
          logProb <- logProb + self[[fn]](instr$values[[1]][i])
        return(logProb)
      },
    calc_1_matp = 
      function(instr, fn) {
        logProb = 0
        for(i in 1:instr$lens[1])
          logProb <- logProb + self[[fn]](instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])  ## Ok to call with a vector?
        return(logProb)
      },
      simulate = function(instr) {
        if(instr$type == 0) return(sim_0(instr))
        if(instr$type == 1) return(sim_1_seq(instr))
        if(instr$type == 2) return(sim_1_mat(instr))
        if(instr$type == 3) return(sim_1_matp(instr))
      },
      sim_0 = function(instr) {
        sim_one(0) ## sim_one will always has `idx` as arg?
      },
      sim_1_seq = function(instr) {
        for(i in 1:instr$lens[1])
          sim_one(instr$values[[1]][1]+i)
      },
      sim_1_mat = function(instr) {
        for(i in 1:instr$lens[1])
          sim_one(instr$values[[1]][i])
      },
      sim_1_matp = function(instr) {
        for(i in 1:instr$lens[1])
          sim_one(instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])  ## Ok to call with a vector?
      }
  ),
  Cpublic = list(
    ## model = 'modelBase_nClass',
    ping = nFunction(
      name = "ping",
      function() {return(TRUE); returnType(logical())},
      compileInfo = list(virtual=TRUE)
    ),
    calculate_cpp = nFunction(
      name = "calculate_cpp",
      function(instr) {
        stop("Uncompiled version of calculate_cpp should not be called.")
      },
      returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base calculate_cpp should never be called (something is wrong)\\n");')
                             return(0)
                         })
    ),
    calculateDiff_cpp = nFunction(
      name = "calculateDiff_cpp",
      function(instr) {
        stop("Uncompiled version of calculateDiff_cpp should not be called.")
      },
      returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base calculateDiff_cpp should never be called (something is wrong)\\n");')
                             return(0)
                         })
    ),
    getLogProb_cpp = nFunction(
      name = "getLogProb_cpp",
      function(instr) {
        stop("Uncompiled version of getLogProb_cpp should not be called.")
      },
      returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base getLogProb_cpp should never be called (something is wrong)\\n");')
                             return(0)
                         })
    ),
    simulate_cpp = nFunction(
      name = "simulate_cpp",
      function(instr) {
        stop("Uncompiled version of simulate_cpp should not be called.")
      },
      returnType = 'void',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base simulate_cpp should never be called (something is wrong)\\n");')
                         })
    )

 
    
 
    
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
