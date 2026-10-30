#' @export
declFunBase_nClass <- nClass(
  classname = "declFunBase_nClass",
  Rpublic = list(
    calculate = function(instr) {
      run_instr(instr, self$calc_one)
    },
    calculateDiff = function(instr) {
      run_instr(instr, self$calcDiff_one)
    },
    getLogProb = function(instr) {
      run_instr(instr, self$getLogProb_one)
    },
    getParamID = function(param) {
      getParamID_one(param)
    },
    # sim_one returns a dummy 0, so simulate shares the calc runners and ignores their sum.
    simulate = function(instr) {
      run_instr(instr, self$sim_one)
      invisible(NULL)
    },
    # One step (node) of an instruction, with no loop. `step` is 1-based within `instr`.
    calculate_singleton = function(instr, step) {
      calc_one(set_instr_idx(instr, step))
    },
    calculateDiff_singleton = function(instr, step) {
      calcDiff_one(set_instr_idx(instr, step))
    },
    getLogProb_singleton = function(instr, step) {
      getLogProb_one(set_instr_idx(instr, step))
    },
    simulate_singleton = function(instr, step) {
      sim_one(set_instr_idx(instr, step))
      invisible(NULL)
    },
    getParam_singleton = function(instr, step, paramID) {
      getParam_one(set_instr_idx(instr, step), paramID)
    },
    getBound_singleton = function(instr, step, boundID) {
      getBound_one(set_instr_idx(instr, step), boundID)
    },
    # getParam: use only the first entry in the instr.
    getParam = function(instr, paramID) {
      fi <- first_idx(instr)
      if (fi$multiple) {
        warning("getParam: instr covers more than one node; using the first node only.")
      }
      getParam_one(fi$idx, paramID)
    },
    # getBound: use only the first entry in the instr.
    getBound = function(instr, boundID) {
      fi <- first_idx(instr)
      if (fi$multiple) {
        warning("getBound: instr covers more than one node; using the first node only.")
      }
      getBound_one(fi$idx, boundID)
    },
    first_idx = function(instr) {
      multiple <- FALSE
      idx <- switch(as.character(instr$instr_type),
        "0" = {
          multiple <- FALSE
          instr$lens
        },
        "1" = ,
        "2" = {
          multiple <- instr$lens[1] > 1
          instr$values[[1]][1]
        },
        "3" = {
          multiple <- instr$lens[1] > 1
          instr$values[[1]][1:instr$dims[1]]
        },
        "18" = {
          multiple <- instr$lens[1] > 1
          out <- rep(0L, instr$dims[1])
          out[instr$slots] <- instr$values[[1]][1:instr$dims[1]]
          out
        },
        "4" = ,
        "5" = ,
        "6" = ,
        "7" = {
          multiple <- instr$lens[1] * instr$lens[2] > 1
          c(instr$values[[1]][1], instr$values[[2]][1])
        },
        "8" = {
          multiple <- instr$lens[1] * instr$lens[2] > 1
          out <- rep(0L, instr$nDim)
          out[instr$slots[1]] <- instr$values[[1]][1]
          out[instr$slots[2:instr$nDim]] <- instr$values[[2]][1:instr$dims[2]]
          out
        },
        "9" = {
          multiple <- instr$lens[1] * instr$lens[2] > 1
          out <- rep(0L, instr$nDim)
          out[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][1:instr$dims[1]]
          out[instr$slots[instr$nDim]] <- instr$values[[2]][1]
          out
        },
        "10" = {
          multiple <- instr$lens[1] * instr$lens[2] > 1
          out <- rep(0L, instr$nDim)
          out[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][1:instr$dims[1]]
          out[instr$slots[(instr$dims[1] + 1):instr$nDim]] <- instr$values[[2]][1:instr$dims[2]]
          out
        },
        "11" = {
          multiple <- instr$lens[1] * instr$lens[2] > 1
          # Hardcoded swap, matching calc_2_x_y_ord: idx[2] from range 1, idx[1] from range 2.
          out <- rep(0L, 2)
          out[2] <- instr$values[[1]][1]
          out[1] <- instr$values[[2]][1]
          out
        },
        "12" = {
          multiple <- instr$lens[1] * instr$lens[2] * instr$lens[3] > 1
          c(instr$values[[1]][1], instr$values[[2]][1], instr$values[[3]][1])
        },
        "13" = {
          multiple <- instr$lens[1] * instr$lens[2] * instr$lens[3] > 1
          first_idx_generic(instr, 3)
        },
        "14" = {
          multiple <- prod(instr$lens[1:4]) > 1
          sapply(instr$values[1:4], `[`, 1)
        },
        "15" = {
          multiple <- prod(instr$lens[1:4]) > 1
          first_idx_generic(instr, 4)
        },
        "16" = {
          multiple <- prod(instr$lens[1:5]) > 1
          sapply(instr$values[1:5], `[`, 1)
        },
        "17" = {
          multiple <- prod(instr$lens[1:5]) > 1
          first_idx_generic(instr, 5)
        },
        stop("getParam: unrecognized instr type ", instr$instr_type)
      )
      list(idx = idx, multiple = multiple)
    },
    # Shared "generic" (slot-placed, mixed seq/matp per range) first-tuple logic
    # for instr types 13, 15, 17, parameterized by the number of ranges `nRanges`.
    first_idx_generic = function(instr, nRanges) {
      idx <- rep(0L, instr$nDim)
      offset <- 0
      for (r in 1:nRanges) {
        if (instr$index_types[r] == 1) {
          idx[instr$slots[offset + 1]] <- instr$values[[r]][1]
        } else {
          d <- instr$dims[r]
          idx[instr$slots[(offset + 1):(offset + d)]] <- instr$values[[r]][1:d]
        }
        offset <- offset + instr$dims[r]
      }
      idx
    }
  ),
  Cpublic = list(
    # model = 'modelBase_nClass',
    ping = nFunction(
      name = "ping",
      function() {
        return(TRUE)
        returnType(logical())
      },
      compileInfo = list(virtual = TRUE)
    ),
    calculate_cpp = nFunction(
      name = "calculate_cpp",
      function(instr) {
        stop("Uncompiled version of calculate_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base calculate_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    calculateDiff_cpp = nFunction(
      name = "calculateDiff_cpp",
      function(instr) {
        stop("Uncompiled version of calculateDiff_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base calculateDiff_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    getLogProb_cpp = nFunction(
      name = "getLogProb_cpp",
      function(instr) {
        stop("Uncompiled version of getLogProb_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base getLogProb_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    simulate_cpp = nFunction(
      name = "simulate_cpp",
      function(instr) {
        stop("Uncompiled version of simulate_cpp should not be called.")
      },
      returnType = "void",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base simulate_cpp should never be called (something is wrong)\\n");')
        }
      )
    ),
    getParamID_cpp = nFunction(
      name = "getParamID_cpp",
      function(param) {
        stop("Uncompiled version of getParamID_cpp should not be called.")
      },
      returnType = "integerScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(param = "characterScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base getParamID_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    getParam_cpp = nFunction(
      name = "getParam_cpp",
      function(instr, paramID) {
        stop("Uncompiled version of getParam_cpp should not be called.")
      },
      returnType = "ETaccessor()",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", paramID = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base getParam_cpp should never be called (something is wrong)\\n");')
          return(ETaccess(0, copy = TRUE))
        }
      )
    ),
    getBound_cpp = nFunction(
      name = "getBound_cpp",
      function(instr, boundID) {
        stop("Uncompiled version of getBound_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", boundID = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base getBound_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    # One step (node) of an instruction, with no loop; see the uncompiled `*_singleton` methods.
    calculate_singleton_cpp = nFunction(
      name = "calculate_singleton_cpp",
      function(instr, step) {
        stop("Uncompiled version of calculate_singleton_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", step = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base calculate_singleton_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    calculateDiff_singleton_cpp = nFunction(
      name = "calculateDiff_singleton_cpp",
      function(instr, step) {
        stop("Uncompiled version of calculateDiff_singleton_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", step = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base calculateDiff_singleton_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    getLogProb_singleton_cpp = nFunction(
      name = "getLogProb_singleton_cpp",
      function(instr, step) {
        stop("Uncompiled version of getLogProb_singleton_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", step = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base getLogProb_singleton_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    ),
    simulate_singleton_cpp = nFunction(
      name = "simulate_singleton_cpp",
      function(instr, step) {
        stop("Uncompiled version of simulate_singleton_cpp should not be called.")
      },
      returnType = "void",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", step = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base simulate_singleton_cpp should never be called (something is wrong)\\n");')
        }
      )
    ),
    getParam_singleton_cpp = nFunction(
      name = "getParam_singleton_cpp",
      function(instr, step, paramID) {
        stop("Uncompiled version of getParam_singleton_cpp should not be called.")
      },
      returnType = "ETaccessor()",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", step = "integerScalar", paramID = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base getParam_singleton_cpp should never be called (something is wrong)\\n");')
          return(ETaccess(0, copy = TRUE))
        }
      )
    ),
    getBound_singleton_cpp = nFunction(
      name = "getBound_singleton_cpp",
      function(instr, step, boundID) {
        stop("Uncompiled version of getBound_singleton_cpp should not be called.")
      },
      returnType = "numericScalar",
      compileInfo = list(
        virtual = TRUE,
        C_fun = function(instr = "instr_nClass", step = "integerScalar", boundID = "integerScalar") {
          cppLiteral('Rprintf("declFunBase_nClass virtual base getBound_singleton_cpp should never be called (something is wrong)\\n");')
          return(0)
        }
      )
    )
  ),
  # We haven't dealt with ensuring a virtual destructor when any method is virtual
  # For now I did it manually by editing the .h and .cpp
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"), package = "nimbleModel") |>
    file.path("declFunBase_nC")),
  compileInfo = list(
    interface = "full",
    createFromR = FALSE,
    exportName = "declFunBase_nClass_new",
    interfaceExclude = c("getParam_cpp", "getParam_singleton_cpp"),
    needed_units = list("instr_nClass"),
    packageNames = c(uncompiled = "declFunBase_nClass_R", compiled = "declFunBase_nClass"),
    depends = "nimbleModel",
    HpostCode = "#include <nimbleModel/predef/declFunClass_/declFunClass_.h>"
  )
)
