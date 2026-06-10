## It's unclear what this should return since in nimble one gets a model object
## but with nCompiler, we need a modelClass to compile and then we can create an instance.
## If we create an instance as the output here, one can't then compile that with an algorithm via `nCompile`.
## Need to think more about the workflow for nimble 2.0.
#' @export
nimbleModel <- function(code,
                        constants = list(),
                        data = list(),
                        inits = list(),
                        dimensions = list(),
                        compile = FALSE,
                        returnClass = FALSE,   # Match object-based behavior of nimble::nimbleModel().
                        where = globalenv(),
                        debug = FALSE,
                        check = getNimbleOption('checkModel'),
                        calculate = TRUE,
                        name = NULL,
                        buildDerivs = getNimbleOption('buildModelDerivs'),
                        userEnv = parent.frame()) {
    ## TODO: arg list taken from `nimble`. Revisit which options are needed.
    ## For the moment this goes through (original) nimbleModel R6 class and then nimbleModel nClass. Clean that up once ideas are in place.
    ## Presumably everything would be in Rpublic initialize for modelBaseClass, so this function will just call modelBase_nClass$new().

    if(length(constants) && sum(names(constants) == ""))
        stop("nimbleModel: 'constants' must be a named list")
    if(length(dimensions) && sum(names(dimensions) == ""))
        stop("nimbleModel: 'dimensions' must be a named list")
    if(length(inits) > 0 && is.list(inits[[1]])) {
        messageIfVerbose('  [Note] Detected JAGS-style initial values, provided as a list of lists. Using the first set of initial values')
        inits <- inits[[1]]
    }
    if(length(inits)) {
        unnamed <- which(names(inits) == "")
        if(length(unnamed) || is.null(names(inits))) {
            warning("One or more unnamed elements found in inits.")
            if(length(unnamed))
                inits <- inits[-unnamed] else inits <- list()
        }
    }
    
    if(length(data) && sum(names(data) == ""))
        stop("nimbleModel: 'data' must be a named list")
    if(any(!sapply(data, function(x) {
        is.numeric(x) || is.logical(x) ||
            (is.data.frame(x) && all(sapply(x, 'is.numeric'))) })))
        stop("nimbleModel: elements of 'data' must be numeric")

    ## TODO: determine if we will need these.
    origInits <- inits
    origData <- data

    modelDef <- modelDefClass$new(code, constants = constants,
                                           dimensions = dimensions, inits = inits,
                                  data = data, userEnv = userEnv)
    ## At this point, data will have been removed from constants.
    specificModelClass <- make_modelClass_from_nimbleModel(modelDef, data, inits, name)
    if(compile) specificModelClass <- nCompile(specificModelClass)
    if(returnClass) return(specificModelClass)  
    model <- specificModelClass$new()  
}

make_modelClass_from_nimbleModel <- function(modelDef, data, inits, name=NULL) {
  modelVarInfo <- get_varInfo_from_nimbleModel(modelDef)
  declInfoList <- list()
  declFunClassList <- list()
  declFunNames <- names(modelDef$declFunNameToIndex)
  for(i in seq_along(modelDef$declInfo)) {
    declInfo <- modelDef$declInfo[[i]]
    decl_methods <- make_decl_methods_from_declInfo(declInfo)
    decl_getParam <- make_decl_getParam(declInfo)
    decl_methods <- c(decl_methods, decl_getParam)
    declVars <- decl_methods |> lapply(\(x) all.vars(body(x))) |> unlist() |> unique() |> setdiff(c("idx", "LocalNewLogProb_", "LocalAns_", "caseID_", "model")) %||% character()
    declVarInfo <- modelVarInfo$vars[declVars]
    declID <- as.numeric(declInfo$declRule$ID)  # Formerly `sourceLineNumber`, which may not be unique.
    declFun_membername <- declFunNames[i]
    declFun_classname <- sub("declFun", "declFunClass", declFun_membername) # name of an nClass generator
    declFun_RvarName <- sub("declFun", "declFunClassGen", declFun_membername) # name of R var holding the nClass generator
    # Currently, we can't just make a list of these but need them as named objects in the environment,
    # which is passed into the nClass() call so that `initialize()` can use them via R's scoping.
    assign(declFun_RvarName,
           make_declFun_nClass(declVarInfo, decl_methods, declFun_classname, declID))
    declInfoList[[i]] <- make_decl_info_for_model_nClass(declFun_membername, declFun_RvarName, declFun_classname, declVarInfo)
  }
  ## We have a canonical ordering of decls, but it does arise from a couple of places that should match.
  # so we check here.
  ordered_decl_names <- lapply(declInfoList, function(x) x$membername) |> unlist()
  if(!identical(ordered_decl_names, names(modelDef$declFunNameToIndex)))
   stop("declaration ordering in declInfoList does not matchdeclFunNameToIndex")
  modelClassInstance <- makeModel_nClass(modelVarInfo, declInfoList, inits = inits, data = data,
                                         modelDef = modelDef, classname = name %||% "my_model",
                                         env = environment())
}

## The two "addModelDollarSign" functions are borrowed directly from nimble.
## This should add model$ in front of any names that are not already part of a '$' expression

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

# Turn variables and methods into a declFun nClass
make_declFun_nClass <- function(varInfo = list(),
                            methods = list(),
                            classname,
                            declID) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  # These are the model member variables to be used by the declFxn.
  # They will be used in a constructor to set up C++ references to model variables.
  CpublicVars <- varInfo |> lapply(\(x) paste0("ref(double(", x$nDim ,", interface=FALSE))"))
  names(CpublicVars) <- varInfo |> lapply(\(x) x$name) |> unlist()
  numVars <- length(varInfo)
  initFun <- function(){}

  if(numVars > 0) {
    ctorArgNames <- paste0(names(CpublicVars), '_')
    # List used when generating C++ constructor code to allow direct initializers, necessary for references.
    # initializersList <- paste0(names(symbolList), '(', ctorArgNames ,')')
    initializersList <- paste0(names(CpublicVars), '(', ctorArgNames ,')')
    formals(initFun) <- structure(as.pairlist(CpublicVars), names = ctorArgNames)
  } else {
    initializersList <- character()
  }
  ## TODO: I don't think this labelCreator (or the one for the model) exist (though they shouldn't be used...)
  if(missing(classname))
    classname <- declLabelCreator()

  baseclass <- paste0("declFunClass_<", classname, ">")

  # Rpublic method to set the model pointer/reference.
  setModel <- function(model) {
    if(!isCompiled()) {
      self$model <- model
      #private$Cpublic_obj$model <- model
    }
    else
       warning("setModel called on compiled object; no action taken")
  }

  declFun_nClass <- substitute(
    nClass(
      inherit = declFunBase_nClass,
      classname = CLASSNAME,
      Rpublic = RPUBLIC,
      Cpublic = CPUBLIC,
      compileInfo = list(
        createFromR = FALSE, # Without a default constructor (which we've disabled here), createFromR is impossible
        interfaceExclude = 'getParam_one', # this returns an ETaccessorBase, which has no R counterpart.
        nClass_inherit = list(base = BASECLASS))  # Ideally this line would be obtained from a base nClass, but we insert it directly for now
    ),
    list(
      CPUBLIC = c(
        declID = declID,
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
  eval(declFun_nClass)
}

# Make all the info needed to include a decl in a model class.
# The decl_nClass should be created first.
# Currently it needs to have a name to include in nCompile(). Later we might be able to pass the object itself
# At first drafting this is fairly trivial but could grow in complexity.

make_decl_info_for_model_nClass <- function(membername,
                           declFunName,
                           classname,
                           varInfo = list()
                           ) {
  ctorArgs <- varInfo |> lapply(\(x) x$name) |> unlist()

  list(declFunName = declFunName,
       membername = membername,
       classname = classname,
       ctorArgs = ctorArgs)
}

makeModel_nClass <- function(modelVarInfo,
                             decls = list(),
                             classname,
                             inits = list(),
                             data = list(),
                             modelDef = NULL,
                             env = parent.frame()
                             ) {
  ## varInfo will be a list (names not used) of name, nDim, sizes.
  CpublicModelVars <- modelVarInfo$vars |> lapply(\(x) paste0("numericArray(nDim=",x$nDim,")"))
  names(CpublicModelVars) <- modelVarInfo$vars |> lapply(\(x) x$name) |> unlist()
  opDefs <- list(
    base_ping = nCompiler:::getOperatorDef("custom_call"),
    setup_auto_decl_mgmt = nCompiler:::getOperatorDef("custom_call"),
    do_setup_decl_mgmt_from_names = nCompiler:::getOperatorDef("custom_call")
  )
  opDefs$base_ping$returnType <- nCompiler:::type2symbol(quote(void()))   # How can this be passed into nClass?
  opDefs$base_ping$labelAbstractTypes$recurse <- FALSE
  opDefs$setup_auto_decl_mgmt$returnType <- nCompiler:::type2symbol(quote(void()))
  opDefs$setup_auto_decl_mgmt$labelAbstractTypes$recurse <- FALSE
  opDefs$do_setup_decl_mgmt_from_names$returnType <- nCompiler:::type2symbol(quote(void()))
  opDefs$do_setup_decl_mgmt_from_names$labelAbstractTypes$recurse <- FALSE

  if(missing(classname))
    classname <- modelLabelCreator()

  CpublicMethods <- list(
    do_setup_auto_decl_mgmt = nFunction(
      name = "call_setup_auto_decl_mgmt",
      function() {},
      compileInfo=list(
        C_fun = function() {setup_auto_decl_mgmt()})
    ),
    setup_decl_mgmt_from_names = nFunction(
      name = "call_setup_decl_mgmt_from_names",
      function(declNames) {},
      compileInfo=list(
        C_fun = function(declNames="RcppCharacterVector") {do_setup_decl_mgmt_from_names(declNames)})
    ),
    # print_decls = nFunction(
    #   name = "print_decls",
    #   function() {},
    #   compileInfo=list(
    #     C_fun = function() {cppLiteral('modelClass_::c_print_decls();')})
    # ),
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
        if(exists(v, self, inherits=FALSE)) self[[v]] <- nArray(value = NA, dim=Rlist[[v]])},
      compileInfo = list(
        C_fun=function(Rlist = 'RcppList') {cppLiteral('modelClass_::resize_from_list(Rlist);')})
    )
  )
  # decls will be a list of membername, declName, (decl) classname, ctorArgs (list)
  decl_pieces <- decls |> lapply(\(x) {
    #nClass_type <- paste0(x$declFxnName, "()")
    init_string <- paste0('nCpp("', x$membername, '( new ', x$classname, '(',
                                    paste0(x$ctorArgs, collapse=","), '))")')
    list(nClass_type = x$declFunName,
         init_string = init_string)
  })

  declFunNameToIndex <- modelDef$declFunNameToIndex

  CpublicDeclFuns <- decl_pieces |> lapply(\(x) x$nClass_type) |> setNames(names(declFunNameToIndex))
  # CpublicDeclFuns <- list(
  #   beta_decl = 'decl_dnorm()'
  # )
  CpublicCtor <- list(
    nFunction(
      function(){
        cppLiteral("setup_decl_mgmt();")  # This will be the default but can be overridden by decls that need to do something special. We could also have a version that takes decl names as input and only sets up those.
      },
      compileInfo = list(constructor=TRUE,
                         #initializers = c('nCpp("beta_decl(new decl_dnorm(mu, beta, 1))")'))
                         initializers = decl_pieces |> lapply(\(x) x$init_string) |> unlist())
    )
  ) |> structure(names = classname)

  declFunPtrsSetupLiterals <- paste0("declFunPtrs[(", as.integer(declFunNameToIndex) , ")-1] = ", names(declFunNameToIndex))
  declFunPtrsResizeLiteral <- paste0("declFunPtrs.resize(", length(declFunNameToIndex) , ")")
  setup_decl_mgmt_body <- as.list(c(declFunPtrsResizeLiteral, declFunPtrsSetupLiterals)) |>
    lapply(\(x) substitute(nCpp(X), list(X = x)))
  setup_decl_mgmt_fun <- function() {}
  for(i in seq_along(setup_decl_mgmt_body))
    body(setup_decl_mgmt_fun)[[i+1]] <- setup_decl_mgmt_body[[i]]
  Cpublic_setup_decl_mgmt <- list(setup_decl_mgmt = nFunction(name = "setup_decl_mgmt", fun = setup_decl_mgmt_fun))

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
                         nClass_inherit = list(base=BASECLASS) #,
#                         needed_units = list("declFxnBase_nClass"), # needed for package=TRUE
#                         Hincludes = '"declFxnBase_nClass_c_.h"' # needed for package=TRUE
                         ),
      Rpublic = RPUBLIC,
      Cpublic = CPUBLIC,
      env = env
    ),
    list(OPDEFS = opDefs,
        # A list of individual elements
        RPUBLIC = list(
                      declFunNameToIndex_ = modelDef$declFunNameToIndex,
                      defaultSizes = modelVarInfo$sizes,
                      defaultInits = inits,
                      defaultData = data,
                      modelDef = modelDef,
                      CpublicDeclFuns = CpublicDeclFuns),
        # A concatenation of lists
        CPUBLIC = c(CpublicDeclFuns, Cpublic_setup_decl_mgmt, CpublicModelVars, CpublicCtor, CpublicMethods),
        CLASSNAME = classname,
        BASECLASS = baseclass)
  )
  eval(ans)
}

## Get varInfo from new nimbleModel
get_varInfo_from_nimbleModel <- function(mDef) {
  extract <- \(x) x |> lapply(\(x) list(name = x$varName, nDim = x$nDim))
  vars <- mDef$varInfo |> extract()
  logProbVars <- mDef$logProbVarInfo |> extract()
  # The resize_from_list method will error out if a scalar is included.
  # The maxs is empty for scalars, so they are automatically omitted from the sizes result here.
  # TODO: CJP sees scalars included as numeric(0) in sizes, so not omitted. Will this be a problem for resize_from_list?
  # TODO: If ok, put sizes info into the same list as vars info.
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

make_decl_method_nFxn <- function(f, name,
                                  argTypes = list(idx = 'integerVector'),
                                  returnType='numericScalar',
                                  exceptionNames = c("idx", "LocalNewLogProb_", "LocalAns_", "caseID_")) {
  Cfun <- f
  Rfun <- f
  body(Rfun) <- nm_addModelDollarSign(body(f),
          exceptionNames = exceptionNames)
  if(is.null(returnType)) returnType <- 'void'
  nFxn <- nFunction(
    name = name,
    fun = Rfun,
    argTypes = argTypes,
    returnType = T(returnType),
    compileInfo=list(C_fun=Cfun),
  )
  nFxn
}

make_decl_getParam <- function(declInfo) {
  if(!isTRUE(declInfo$stoch)) {
    nFxn <- nFunction(
      name = "getParam_one",
      function(idx, caseID_) {
        stop("getParam_one should not be called for a deterministic node.")
      },
      argTypes = list(idx = 'integerVector', caseID_ = 'integerScalar'),
      returnType = 'ETaccessor',
      compileInfo = list(
        C_fun = function(idx, caseID_) {
          cppLiteral("Rcpp::stop(\"getParam_one should not be called for a deterministic node.\")")
          cppLiteral("return ETaccessPtr<true, int>(0);")
        }
      )
    )
    return(list(getParam_one = nFxn))
  }
  getParamFun <- function(idx, caseID_) {}
  altParams <- declInfo$altParamExprs
  RHS <- declInfo$calculateCode[[3]]
  LHS <- declInfo$calculateCode[[2]]
  params <- as.list(RHS[-1])
  allParams <- c(list(value = LHS), params, altParams)

  # Same replacement steps as for decl_methods below
  context <- declInfo$declRule$context
  replacements <- sapply(seq_along(context$singleContexts),
                         function(i) parse(text = paste0('idx[',i,']'))[[1]])
  names(replacements) <- context$indexVarNames
  allParamsRep <- lapply(allParams,
                         function(expr) eval(substitute(substitute(e, replacements), list(e = expr))))
  allParamsResExpr <- lapply(allParamsRep,
                             function(expr) {
                             copy <- !is.name(expr)
      substitute(LocalAns_ <- ETaccess(EXPR, copy = COPY), list(EXPR = expr, COPY = copy))
    })
  switchExpr <- as.call(c(list(as.name("nSwitch"), quote(caseID_),
                               substitute(1:N, list(N = length(allParamsResExpr)))),
                          allParamsResExpr))
  body(getParamFun) <- substitute(
  {
    if(caseID_ < 1 | caseID_ > N) {
      cppLiteral("Rcpp::stop(\"Invalid parameter ID in getParam.\");")
      cppLiteral("return ETaccessPtr<true, int>(0);")
    }
      SWITCHEXPR
      return(LocalAns_)
    },
    list(SWITCHEXPR = switchExpr, N = length(allParamsResExpr))
  )
  # We might need a greater split between R_fun and C_fun here.
  nFxn <- make_decl_method_nFxn(getParamFun, name = "getParam_one",
    argTypes = list(idx = 'integerVector', caseID_ = 'integerScalar'),
    returnType = 'ETaccessor')
  list(getParam_one  = nFxn)
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
              make_decl_method_nFxn("sim_one", returnType = NULL),
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
              make_decl_method_nFxn("sim_one", returnType = NULL),
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
