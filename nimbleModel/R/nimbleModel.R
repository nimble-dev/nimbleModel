## It's unclear what this should return since in nimble one gets a model object
## but with nCompiler, we need a modelClass to compile and then we can create an instance.
## If we create an instance as the output here, one can't then compile that with an algorithm via `nCompile`.
## Need to think more about the workflow for nimble 2.0.
nimbleModel <- function(code,
                        constants = list(),
                        data = list(),
                        inits = list(),
                        dimensions = list(),
                        compile = FALSE,
                        returnClass = TRUE,
                        where = globalenv(),
                        debug = FALSE,
                        check = getNimbleOption('checkModel'),
                        calculate = TRUE,
                        name = NULL,
                        buildDerivs = getNimbleOption('buildModelDerivs'),
                        userEnv = parent.frame()) {
    ## TODO: arg list taken from `nimble`. Revisit which options are needed.
    ## For the moment this goes through nimbleModel R6 class and then nCompiler class. Clean that up once ideas are in place.
    ## Presumably everything would be in Rpublic initialize for modelBaseClass, so this function will just call modelBase_nClass$new().
    m <- modelClass$new(name = name, code = code, constants = constants, data = data, inits = inits, dimensions = dimensions, userEnv = userEnv)
    modelClassInstance <- make_modelClass_from_nimbleModel(m)
    if(compile) modelClassInstance <- nCompile(modelClassInstance)
    if(returnClass) return(modelClassInstance)  # Standard use for when compiling a model(class) and algo(class) together.
    model <- modelClassInstance$new()  # Otherwise return model object for manipulation from R.
}

make_modelClass_from_nimbleModel <- function(m, compile=FALSE) {
  mDef <- m$modelDef
  allVarInfo <- get_varInfo_from_nimbleModel(m)
  modelVarInfo <- allVarInfo$vars
  declFxnNames <- character()
  declInfoList <- list()
  declFxnList <- list()
  # two vectors for canonical use for calculation instructions
  # to move between names and indices of declFxns:
  for(i in seq_along(mDef$declInfo)) {
    declInfo <- mDef$declInfo[[i]]
    decl_methods <- make_decl_methods_from_declInfo(declInfo)
    declVars <- decl_methods |> lapply(\(x) all.vars(body(x))) |> unlist() |> unique() |> setdiff(c("idx", "LocalNewLogProb_", "LocalAns_", "model")) %||% character()
    declVarInfo <- modelVarInfo[declVars]
    SLN <- declInfo$sourceLineNumber
    decl_classname <- paste0("declClass_", SLN) # name of an nClass generator
    decl_RvarName <- paste0("declFxn_", SLN)    # name of an R variable holding the nClass generator
    decl_membername <- paste0("decl_", SLN)     # name of model member variable holding an instance of the nClass
    # Currently, we can't just make a list of these but need them as named objects in the environment
    declFxnList[[decl_RvarName]] <- make_declFxn_nClass(declVarInfo, decl_methods, decl_classname)
    assign(decl_RvarName,
      declFxnList[[decl_RvarName]]
    )
    declInfoList[[i]] <- make_decl_info_for_model_nClass(decl_membername, decl_RvarName, decl_classname, declVarInfo)
  }
  modelClassInstance <- makeModel_nClass(modelVarInfo, declInfoList, inits = m$origInits, data = m$origData, model = m, classname = "my_model", env = environment())
}


nm_addModelDollarSign <- function(expr, exceptionNames = character(0)) {
    if(is.numeric(expr)) return(expr)
    if(is(expr, 'srcref')) return(expr)
    if(is.name(expr)) {
        if((as.character(expr) %in% exceptionNames) || (as.character(expr) == ''))    return(expr)
        proto <- quote(model$a)
        proto[[3]] <- expr
        return(proto)
    }
    if(is.call(expr)) {
        if(expr[[1]] == '$'){
            expr[2] <- lapply(expr[2], function(listElement) nm_addModelDollarSign(listElement, exceptionNames))
            return(expr)
        }
        if(expr[[1]] == 'returnType')
            return(expr)
        if(length(expr) > 1) {
            expr[2:length(expr)] <- lapply(expr[-1], function(listElement) nm_addModelDollarSign(listElement, exceptionNames))
            return(expr)
        }
    }
    return(expr)
}

# Turn variables and methods into a declFxn nClass
make_declFxn_nClass <- function(varInfo = list(),
                            methods = list(),
                            classname) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  # These are the model member variables to be used by the declFxn.
  # They will be used in a constructor to set up C++ references to model variables.
  CpublicVars <- varInfo |> lapply(\(x) paste0("ref(double(", x$nDim ,", interface=FALSE))"))
  names(CpublicVars) <- varInfo |> lapply(\(x) x$name) |> unlist()


#  varInfo_2_symbol <- \(x) nCompiler:::symbolBasic$new(
#    type="double", nDim=x$nDim, name="", isRef=TRUE, isConst=FALSE, interface=FALSE) # In future maybe isConst=TRUE, but it might not matter much
#  symbolList <- varInfo |> lapply(varInfo_2_symbol)
#  names(symbolList) <- varInfo |> lapply(\(x) x$name) |> unlist()
  numVars <- length(varInfo)

#  CpublicVars <- names(symbolList) |> lapply(\(x) eval(substitute(quote(T(symbolList$NAME)),
#                                                    list(NAME=as.name(x)))))
#  names(CpublicVars) <- names(symbolList)
  # This is a kluge to have a model field in the Cpublic_obj,
  # needed for uncompiled purposes, and for compiled purposes
  # we instead use references to model variables. So
  # the declared type here is arbitrary.
  initFun <- function(){}

  if(numVars > 0) {
    # ctorArgNames <- paste0(names(symbolList), '_')
    ctorArgNames <- paste0(names(CpublicVars), '_')
    # List used when generating C++ constructor code to allow direct initializers, necessary for references.
    # initializersList <- paste0(names(symbolList), '(', ctorArgNames ,')')
    initializersList <- paste0(names(CpublicVars), '(', ctorArgNames ,')')
    formals(initFun) <- structure(as.pairlist(CpublicVars), names = ctorArgNames)
  } else {
    initializersList <- character()
  }
  if(missing(classname))
    classname <- declFxnLabelCreator()

  baseclass <- paste0("declFxnClass_<", classname, ">")

  # Rpublic method to set the model pointer/reference.
  setModel <- function(model) {
    if(!isCompiled()) {
      self$model <- model
      #private$Cpublic_obj$model <- model
    }
    else
       warning("setModel called on compiled object; no action taken")
  }

#  This was a prototype
  declFxn_nClass <- substitute(
    nClass(
      inherit = declFxnBase_nClass,
      classname = CLASSNAME,
      Rpublic = RPUBLIC,
      Cpublic = CPUBLIC,
      compileInfo = list(
        createFromR = FALSE, # Without a default constructor (which we've disabled here), createFromR is impossible
        nClass_inherit = list(base = BASECLASS))  # Ideally this line would be obtained from a base nClass, but we insert it directly for now
    ),
    list(
      CPUBLIC = c(
        list(
          nFunction(
            initFun,
            compileInfo = list(constructor=TRUE, initializers = initializersList)
          )
        ) |> structure(names = classname),
        CpublicVars,
        list(model = "RcppList"),
        methods
      ),
      RPUBLIC = list(#model = NULL,
                     setModel = setModel),
      CLASSNAME = classname,
      BASECLASS = baseclass
    ))
  eval(declFxn_nClass)
}
#test <- nCompiler:::type2symbol('CppVar(baseType = type2cpp("numericVector"), ref=TRUE, const=TRUE)')

# Make all the info needed to include a decl in a model class.
# The declFxn_nClass should be created first.
# Currently it needs to have a name to include in nCompile(). Later we might be able to pass the object itself
# At first drafting this is fairly trivial but could grow in complexity.

make_decl_info_for_model_nClass <- function(membername,
                           declFxnName,
                           classname,
                           varInfo = list()
                           ) {
  ctorArgs <- varInfo |> lapply(\(x) x$name) |> unlist()

  list(declFxnName = declFxnName,
       membername = membername,
       classname = classname,
       ctorArgs = ctorArgs)
}

makeModel_nClass <- function(varInfo,
                             decls = list(),
                             classname,
                             sizes = list(),
                             inits = list(),
                             data = list(),
                             model = NULL,
                             env = parent.frame()
                             ) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  CpublicModelVars <- varInfo |> lapply(\(x) paste0("numericArray(nDim=",x$nDim,")"))
  names(CpublicModelVars) <- varInfo |> lapply(\(x) x$name) |> unlist()
  opDefs <- list(
    base_ping = nCompiler:::getOperatorDef("custom_call"),
    setup_decl_mgmt = nCompiler:::getOperatorDef("custom_call"),
    do_setup_decl_mgmt_from_names = nCompiler:::getOperatorDef("custom_call")
  )
  opDefs$base_ping$returnType <- nCompiler:::type2symbol(quote(void()))   # How can this be passed into nClass?
  opDefs$base_ping$labelAbstractTypes$recurse <- FALSE
  opDefs$setup_decl_mgmt$returnType <- nCompiler:::type2symbol(quote(void()))
  opDefs$setup_decl_mgmt$labelAbstractTypes$recurse <- FALSE
  opDefs$do_setup_decl_mgmt_from_names$returnType <- nCompiler:::type2symbol(quote(void()))
  opDefs$do_setup_decl_mgmt_from_names$labelAbstractTypes$recurse <- FALSE

  if(missing(classname))
    classname <- modelLabelCreator()

  CpublicMethods <- list(
    do_setup_decl_mgmt = nFunction(
      name = "call_setup_decl_mgmt",
      function() {},
      compileInfo=list(
        C_fun = function() {setup_decl_mgmt()})
    ),
    setup_decl_mgmt_from_names = nFunction(
      name = "call_setup_decl_mgmt_from_names",
      function(declNames) {},
      compileInfo=list(
        C_fun = function(declNames="RcppCharacterVector") {do_setup_decl_mgmt_from_names(declNames)})
    ),
    print_decls = nFunction(
      name = "print_decls",
      function() {},
      compileInfo=list(
        C_fun = function() {cppLiteral('modelClass_::c_print_decls();')})
    ),
    set_from_list = nFunction(
      name = "set_from_list",
      function(Rlist) {for(v in names(Rlist))
        if(exists(v, self, inherits=FALSE)) self[[v]] <- Rlist[[v]]},
      compileInfo=list(
        C_fun=function(Rlist = 'RcppList') {cppLiteral('modelClass_::set_from_list(Rlist);')})
    ),
    resize_from_list = nFunction(
      name = "resize_from_list",
      function(Rlist) {for(v in names(Rlist))
        if(exists(v, self, inherits=FALSE)) self[[v]] <- nArray(dim=Rlist[[v]])},
      compileInfo = list(
        C_fun=function(Rlist = 'RcppList') {cppLiteral('modelClass_::resize_from_list(Rlist);')})
    )
  )
  # decls will be a list of membername, declFxnName, (decl) classname, ctorArgs (list)
  decl_pieces <- decls |> lapply(\(x) {
    #nClass_type <- paste0(x$declFxnName, "()")
    init_string <- paste0('nCpp("', x$membername, '( new ', x$classname, '(',
                                    paste0(x$ctorArgs, collapse=","), '))")')
    list(nClass_type = x$declFxnName,
         init_string = init_string,
         membername = x$membername)
  })
  declObjNames <- (decl_pieces |> lapply(\(x) x$membername) |> unlist()) %||% character()
  # declObjNames also serves for canonical lookup of names by index.
  # e.g. declObjNames[i] gives the member name of the index=i decl member.
  declObjName_2_declIndex <- seq_along(declObjNames) |> structure(names=declObjNames)
  # Inversely, declobjName_2_declIndex["decl_3"] gives the index of that decl.
  CpublicDeclFuns <- decl_pieces |> lapply(\(x) x$nClass_type) |> setNames(declObjNames)
  # CpublicDeclFuns <- list(
  #   beta_decl = 'decl_dnorm()'
  # )
  CpublicCtor <- list(
    nFunction(
      function(){},
      compileInfo = list(constructor=TRUE,
                         #initializers = c('nCpp("beta_decl(new decl_dnorm(mu, beta, 1))")'))
                         initializers = decl_pieces |> lapply(\(x) x$init_string) |> unlist())
    )
  ) |> structure(names = classname)
  initialize <- function(sizes = list(), inits = list(), data = list()) {
    # It is not very easy to set debug onto the initialize function, so
    # here is a magic flag.
    if(isTRUE(.GlobalEnv$.debugModelInit)) browser()
    super$initialize()
    if(isCompiled())
      self$setup_decl_mgmt_from_names(self$declObjNames)
    if(!isCompiled()) {
      for(declObj in self$declObjNames) {
        self[[declObj]] <- eval(as.name(self$CpublicDeclFuns[[declObj]]))$new()
        self[[declObj]]$setModel(self)
      }
    }
      
    # First expand any provided or default sizes
    # To-Do possibly merge the argument sizes and defaultSizes by element.
    if(missing(sizes)) sizes <- self$defaultSizes
    if(length(sizes)) resize_from_list(sizes)
  
    # Then any provided inits over-ride any provided sizes
    # To-Do: Ditto
    if(missing(inits)) inits <- self$defaultInits
    if(length(inits)) set_from_list(inits)

    # TODO: do we want to handle data differently?
    # TODO: need to work through not setting as 'data' if values are NA;
    #   check back against how dataRules work in nimbleModel work.
    if(missing(data)) data <- self$defaultData
    if(length(data)) set_from_list(data)
browser()
      
      
  }
  baseclass <- paste0("modelClass_<", classname, ">")
  # CpublicDeclFuns has elements like "decl_1 = quote(declFxn_1())"
  # We provide it in Cpublic to declare C++ member variables with types.
  # We also place the list itself in the class so that we can look up for uncompiled execution
  # the objects that need to be created in initialize.
  # If we someday make type declarations and initializations more automatic, we can avoid this duplication.
  ans <- substitute(
    nClass(
      classname = CLASSNAME,
      inherit = modelBase_nClass,
      compileInfo = list(opDefs = OPDEFS,
                         nClass_inherit = list(base=BASECLASS)#,
#                         needed_units = list("declFxnBase_nClass"), # needed for package=TRUE
#                         Hincludes = '"declFxnBase_nClass_c_.h"' # needed for package=TRUE
                         ),
      Rpublic = RPUBLIC,
      Cpublic = CPUBLIC,
      env = env
    ),
    list(OPDEFS = opDefs,
        # A list of individual elements
        RPUBLIC = list(initialize=initialize,
                      declObjNames = declObjNames,
                      declObjName_2_declIndex = declObjName_2_declIndex,
                      defaultSizes = sizes,
                      defaultInits = inits,
                      defaultData = data,
                      modelDef = model$modelDef,
                      ## TODO: add other fields from `nimbleModel::modelClass` such as dataRules, predictiveRules, etc.
                      CpublicDeclFuns = CpublicDeclFuns),
        # A concatenation of lists
        CPUBLIC = c(CpublicDeclFuns, CpublicModelVars, CpublicCtor, CpublicMethods),
        CLASSNAME = classname,
        BASECLASS = baseclass)
  )
  eval(ans)
}

## Get varInfo from new nimbleModel
get_varInfo_from_nimbleModel <- function(model) {
  mDef <- model$modelDef
  extract <- \(x) x |> lapply(\(x) list(name = x$varName, nDim = x$nDim))
  vars <- mDef$varInfo |> extract()
  logProbVars <- mDef$logProbVarInfo |> extract()
  # The resize_from_list method will error out if a scalar is included.
  # The maxs is empty for scalars, so they are automatically omitted from the sizes result here.
  extract_sizes <- \(x) x|> lapply(\(x) x$maxs)
  sizes <- mDef$varInfo |> extract_sizes()
  logProb_sizes <- mDef$logProbVarInfo |> extract_sizes()
  list(
    vars = c(vars, logProbVars),
    sizes = c(sizes, logProb_sizes)
  )
}

# make_stoch_calculate <- function(LHSrep, RHSrep, logProbExprRep) {
#   lenRHS <- length(RHSrep)
#   if(length(RHS) > 1) {
#     RHSrep[3:(lenRHS+1)] <- RHSrep[2:lenRHS]
#     names(RHSrep)[3:(lenRHS+1)] <- names(RHSrep)[2:lenRHS]
#   }
#   RHSrep[[2]] <- LHSrep
#   names(RHSrep)[2] <- ""
#   RHSrep[[lenRHS+2]] <- 1
#   names(RHSrep)[lenRHS+2] <- "log"
#   # We create separate code for R and C execution.
#   calc1Cfun <- substitute(
#     function(idx) {LHS <- RHS; return(LHS)},
#     list(LHS = logProbExprRep, RHS = RHSrep)
#   ) |> eval()
#   make_calculate_from_Cfun(calc1Cfun)
# }

make_stoch_sim_line <- function(LHSrep, RHSrep) {
  BUGSdistName <- nCompiler:::safeDeparse(RHSrep[[1]])
  distInfo <- getDistributionInfo(BUGSdistName)
  sim_code <- as.name(distInfo$simulateName)
  if(is.null(sim_code)) stop("Could not find simulation ('r') function for ", BUGSdistName)
  RHSrep[[1]] <- sim_code
  # scoot all named arguments right 1 position
  if(length(RHSrep) > 1) {
    for(i in (length(RHSrep)+1):3) {
      RHSrep[i] <- RHSrep[i-1]
      names(RHSrep)[i] <- names(RHSrep)[i-1]
    }
  }
  RHSrep[[2]] <- 1
  names(RHSrep)[2] <- ''
  sim_line <- substitute(
    LHS <- RHS,
    list(LHS = LHSrep, RHS = RHSrep))
  sim_line
}

make_stoch_calc_line <- function(LHSrep, RHSrep, logProbExprRep, diff = FALSE) {
  lenRHS <- length(RHSrep)
  if(length(RHSrep) > 1) {
    RHSrep[3:(lenRHS+1)] <- RHSrep[2:lenRHS]
    names(RHSrep)[3:(lenRHS+1)] <- names(RHSrep)[2:lenRHS]
  }
  RHSrep[[2]] <- LHSrep
  names(RHSrep)[2] <- ""
  RHSrep[[lenRHS+2]] <- 1
  names(RHSrep)[lenRHS+2] <- "log"
  # We create separate code for R and C execution.
  if(!diff) {
    calc_line <- substitute(
      LHS <- RHS,
      list(LHS = logProbExprRep, RHS = RHSrep))
  } else {
    calc_line <- substitute(
        LocalNewLogProb_ <- RHS,
      list(RHS = RHSrep))
  }
  calc_line
}

make_determ_calc_line <- function(LHSrep, RHSrep) {
  calc_line <- substitute(
    LHS <- RHS,
    list(LHS = LHSrep, RHS = RHSrep))
  calc_line
}

make_nFxn_from_Cfun <- function(Cfun) {
  Rfun <- Cfun
  body(calc1Rfun) <- nm_addModelDollarSign(body(Cfun), exceptionNames = c("idx"))
  nFxn <- nFunction(
    name = "calc_one",
    fun = Rfun,
    compileInfo=list(C_fun=Cfun),
    argTypes = list(idx = 'integerVector'),
    returnType = 'numericScalar')
  #declVars <- all.vars(body(calc1Cfun)) |> setdiff("idx")
  nFxn
}

make_decl_method_nFxn <- function(f, name, returnType='numericScalar') {
  Cfun <- f
  Rfun <- f
  body(Rfun) <- nm_addModelDollarSign(body(f), exceptionNames = c("idx", "LocalNewLogProb_", "LocalAns_"))
  if(is.null(returnType)) returnType <- 'void'
  nFxn <- nFunction(
    name = name,
    fun = Rfun,
    argTypes = list(idx = 'integerVector'),
    returnType = returnType,
    compileInfo=list(C_fun=Cfun),
  )
  nFxn
}

make_decl_methods_from_declInfo <- function(declInfo) {
  # pieces are adapted from Chris' code in nimbleModel and/or old nimble.
  #
  # This function creates a calc_one nFunction that calculates single index case.
  # This will then be used by generic iterator over indices.
  # Vectorized cases can be added in this basic framework later.
  modelCode <- declInfo$calculateCode
  LHS <- modelCode[[2]]
  RHS <- modelCode[[3]]
  type <- if(modelCode[[1]]=="~") "stoch" else "determ" # or use declInfo$stoch (logical)
  context <- declInfo$declRule$context
  replacements <- sapply(seq_along(context$singleContexts),
                         function(i) parse(text = paste0('idx[',i,']'))[[1]])
  names(replacements) <- context$indexVarNames
  LHSrep <- eval(substitute(substitute(e, replacements), list(e = LHS)))
  RHSrep <- eval(substitute(substitute(e, replacements), list(e = RHS)))

  if(type == 'determ') {
    methodList <- eval(substitute(
        list(
            sim_one   = (function(idx) {calc_one(idx)}) |>
              make_decl_method_nFxn("sim_one", NULL),
            calc_one  = (function(idx) {DETERMCALC; return(invisible(0))}) |>
              make_decl_method_nFxn("calc_one"),
            calcDiff_one = (function(idx) {calc_one(idx);return(invisible(0))}) |>
              make_decl_method_nFxn("calcDiff_one"),
            getLogProb_one = (function(idx) {return(0)}) |>
              make_decl_method_nFxn("getLogProb_one")
        ),
        list(DETERMCALC = make_determ_calc_line(LHSrep, RHSrep))
        ))
    }
    if(type == 'stoch') {
      logProbExpr <- declInfo$genLogProbExpr()
      logProbExprRep <- eval(substitute(substitute(e, replacements), list(e = logProbExpr)))
      methodList <- eval(substitute(
        list(
            sim_one   = (function(idx) { STOCHSIM }) |>
              make_decl_method_nFxn("sim_one", NULL),
            calc_one  = (function(idx) { STOCHCALC;   return(invisible(LOGPROB)) }) |>
              make_decl_method_nFxn("calc_one"),
            calcDiff_one = (function(idx) {STOCHCALC_DIFF; LocalAns_ <- LocalNewLogProb_ - LOGPROB;
                                           LOGPROB <- LocalNewLogProb_; return(invisible(LocalAns_))}) |>
              make_decl_method_nFxn("calcDiff_one"),
            getLogProb_one = (function(idx) { return(LOGPROB) }) |>
              make_decl_method_nFxn("getLogProb_one")
        ),
        list( LOGPROB   = logProbExprRep,
              STOCHSIM  = make_stoch_sim_line(LHSrep, RHSrep),
              STOCHCALC = make_stoch_calc_line(LHSrep, RHSrep, logProbExprRep),
              STOCHCALC_DIFF = make_stoch_calc_line(LHSrep, RHSrep, logProbExprRep, diff=TRUE))
      ))
  }
  methodList
}


