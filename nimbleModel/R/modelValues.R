# Initial rough drafting of modelValues

modelValuesBase_nClass <- nCompiler::nClass(
  classname = "modelValues",
  Rpublic = list(
    initialize = function(...) {
      super$initialize(...)
      CppObj_provided <- "CppObj" %in% ...names()
      if (!CppObj_provided) {
        # Compiled constructor will be called automatically (even without this full interface).
        # Uncompiled matching steps are done by direct call.
        if (!isCompiled()) {
          MVBase_nC()
        }
        # Initialize for anything with this full interface (uncompiled for full-interface compiled).
        self$dot_sizeList <- self$defaultSizes
      }
    }
  ),
  Cpublic = list(
    dot_sizeList = "RcppList",
    dot_current_nRow = "integerScalar",
    MVBase_nC = nCompiler::nFunction(
      function() {
        self$dot_current_nRow <<- 0
        self$dot_sizeList <<- list()
      },
      compileInfo = list(
        constructor = TRUE,
        # By writing it this way, we ensure that even an object
        # created in C++ or without a full interface will have
        # these initializations done.
        # In the full interface case, dot_sizeList is then set to defaultSizes.
        C_fun = function() {
          self$dot_current_nRow <- 0
          cppLiteral("this->dot_sizeList = Rcpp::List();")
        }
      )
    ),
    set_sizeList = nCompiler::nFunction(
      name = "set_sizeList",
      fun = function(sizeList = "RcppList") {
        self$dot_sizeList <<- sizeList
      }
    ),
    get_sizeList = nCompiler::nFunction(
      name = "get_sizeList",
      fun = function() {
        return(self$dot_sizeList)
        returnType("RcppList")
      }
    ),
    resize = nCompiler::nFunction(
      name = "resize",
      fun = function(m) {
        stop("Should not be calling uncompiled modelValuesBase_nClass resize().")
      },
      compileInfo = list(
        C_fun = function(m) {
          cppLiteral('Rcpp::stop("Should not be calling compiled modelValuesBase_nClass resize().");')
        },
        virtual = TRUE
      ),
      argTypes = list(m = "integerScalar")
    ),
    getLength = nCompiler::nFunction(
      name = "getLength",
      function() {
        return(self$dot_current_nRow)
        returnType("integerScalar")
      },
      compileInfo = list(virtual = TRUE)
    ),
    getSize = nCompiler::nFunction( # this is an alias for getLength for backward compatibility.
      name = "getSize",
      function() {
        return(self$getLength())
        returnType("integerScalar")
      },
      compileInfo = list(virtual = TRUE)
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"),
    package = "nimbleModel"
  ) |> file.path("MVBase_nC")),
  compileInfo = list(
    cpp_classname = "MVBase_nC",
    interface = "full",
    createFromR = FALSE,
    exportName = "modelValuesBase_nClass_new",
    packageNames = c(uncompiled = "modeValuesBase_nClass_R", compiled = "modelValuesBase_nClass"),
    depends = "nimbleModel",
    HpostCode = "#include <nimbleModel/predef/modelValuesClass_/modelValuesClass_.h>",
    overloadDefs = list(
      resize = list(
        labelAbstractTypes = list(
          handler = "custom_call",
          recurse = TRUE,
          returnType = "void()"
        ),
        eigenImpl = list(handler = "PtrMethod", methodName = "resize")
      ),
      getsize = list(
        labelAbstractTypes = list(
          handler = "custom_call",
          recurse = TRUE,
          returnType = "integerScalar()"
        ),
        eigenImpl = list(handler = "PtrMethod", methodName = "getLength")
      )
      # In a future it could be nice to support `[` and `[[` by overloaded opDefs.
      # However at the moment we are targeting nimble2 support and will achieve that
      # in its keyword processing system.
    )
  )
)

modelValues_resize <- function(self, m, sizeList, v) {
  #  for (v in names(sizeList)) {
  this_sizeList <- sizeList[[v]]
  # This next check is an uncompiled safety fallback.
  # However, the sizeList entry for a scalar should be length 1, not empty.
  if (length(this_sizeList) == 0) this_sizeList <- 1
  old_m <- length(self[[v]])
  length(self[[v]]) <- m # This invokes nList resizing.
  if (m > old_m) {
    if (length(this_sizeList) == 1) {
      for (i in (old_m + 1):m) self[[v]][[i]] <- numeric(length = this_sizeList)
    } else {
      for (i in (old_m + 1):m) self[[v]][[i]] <- array(0, dim = this_sizeList)
    }
  }
  #  }
}

make_modelValues_nClass <- function(varInfo,
                                    cpp_classname) {
  if (missing(cpp_classname)) {
    hashedID <- make_modelValues_hashID(varInfo)
    cpp_classname <- Rname2CppName(paste0("MV_", hashedID))
  }
  # All cases get the same classname so that they share the same S3 dispatch for `[`, `[<-`, etc.
  classname <- cpp_classname
  # TODO: Sort varInfo$vars (as make_modelValues_hashID does) before generating the class.
  #   The hash is order-robust but Cpublic is built in the given order, so when equivalent
  #   classes are deduped by classID, the member order (interface_names, as.list) comes
  #   from whichever generator was registered first.
  e <- environment()
  CpublicVars <- varInfo$vars |>
    lapply(\(x) {
      nDim <- x$nDim
      nLname <- paste0("nL", nDim, "D")
      if (!exists(nLname, envir = e)) {
        e[[nLname]] <- eval(substitute(nCompiler:::nList(numericArray(nDim = NDIM)), list(NDIM = nDim)))
      }
      paste0(nLname)
    })
  names(CpublicVars) <- varInfo$vars |>
    lapply(\(x) x$name) |>
    unlist()
  ctor_lines <- varInfo$vars |>
    lapply(\(x) {
      nDim <- x$nDim
      nLname <- paste0("nL", nDim, "D")
      substitute(
        V <<- NL$new(),
        list(V = as.name(x$name), NL = as.name(nLname))
      )
    })
  resize_lines <- varInfo$vars |>
    lapply(\(x) {
      nDim <- x$nDim
      # nLname <- paste0("nL", nDim, "D")
      Cline <- gsub("NDIM", nDim, "resize_one<NDIM>(V, m, this->getSizeOrNULL(this->dot_sizeList, NAME))")
      # Cline <- gsub("NDIM", nDim, "resize_one<NDIM>(V, m, as<SEXP>(this->dot_sizeList[NAME]))")
      Cline <- gsub("NAME", paste0("\"", x$name, "\""), Cline)
      Cline <- gsub("V", x$name, Cline)
      substitute(nCpp(CLINE), list(CLINE = Cline))
    })

  c_ctor_fun <- function() {}
  body(c_ctor_fun) <- as.call(c(as.name("{"), ctor_lines))

  c_resize_fun <- function(m) {}
  body(c_resize_fun) <- as.call(c(as.name("{"), resize_lines, list(quote(dot_current_nRow <- m))))

  r_resize_lines <- varInfo$vars |>
    lapply(\(x) {
      substitute(
        modelValues_resize(self, m, dot_sizeList, v = V),
        list(V = x$name)
      )
    })
  r_resize_body <- as.call(c(as.name("{"), r_resize_lines, list(quote(self$dot_current_nRow <- m))))
  r_resize_fun <- function(m) {}
  body(r_resize_fun) <- r_resize_body

  CPUBLIC <- c(
    list(
      CPP_CLASSNAME = nCompiler::nFunction(
        fun = c_ctor_fun,
        compileInfo = list(
          constructor = TRUE
        )
      ),
      resize = nCompiler::nFunction(
        name = "resize",
        fun = r_resize_fun,
        # fun = function(m) {
        #   modelValues_resize(self, m, dot_sizeList)
        #   self$dot_current_nRow <- m
        #   # check preservation issues in resizing
        # },
        compileInfo = list(
          C_fun = c_resize_fun
        ),
        argTypes = list(m = "integerScalar")
      )
    ),
    CpublicVars
  )

  names(CPUBLIC)[1] <- cpp_classname

  defaultSizes <- varInfo$sizes
  if (is.null(defaultSizes)) defaultSizes <- list()
  for (v in names(varInfo$vars)) {
    if (is.null(defaultSizes[[v]]) || length(defaultSizes[[v]]) == 0) {
      nDim <- varInfo$vars[[v]]$nDim
      # Scalars get size 1 so uncompiled rows are numeric(1). C++ ignores sizes for nDim 0.
      defaultSizes[[v]] <- if (nDim == 0) 1 else rep(0, nDim)
    }
  }

  generator_code <- substitute(
    nCompiler::nClass(
      classname = CLASSNAME,
      inherit = modelValuesBase_nClass,
      compileInfo = list(
        cpp_classname = CPP_CLASSNAME,
        nClass_inherit = list(base = "modelValuesClass_")
      ),
      Rpublic = list(
        defaultSizes = defaultSizes,
        varInfo = varInfo,
        initialize = function(...) {
          super$initialize(...)
          if (!isCompiled()) {
            CPP_CLASSNAME_()
          }
        }
      ),
      Cpublic =
        CPUBLIC
    ),
    list(
      CLASSNAME = classname,
      CPP_CLASSNAME_ = as.name(cpp_classname),
      CPP_CLASSNAME = cpp_classname
    )
  )
  generator <- eval(generator_code)
  generator$set("public", "NCgenerator", generator)
  generator
}

make_modelValues_hashID <- function(varInfo) {
  # We go to some lengths to normalize
  # so that we obtain the same hash for equivalent varInfo.
  varsList <- varInfo$vars
  sortedNames <- sort(names(varsList))
  sortedVars <- varsList[sortedNames]
  nested_normalize <- function(x) {
    if (!is.list(x)) {
      stop("Elements of a modelValues varInfo list must be lists with elements 'name' and 'nDim' (or unnamed elements in that order)")
    }
    if (is.null(names(x))) {
      if (length(x) != 2) {
        stop("Elements of a modelValues varInfo list must have 2 elements (optionally named 'name' and 'nDim')")
      }
      if (!is.character(x[[1]]) || !is.numeric(x[[2]])) {
        stop("Elements of a modelValues varInfo list, if not named, must have first element character (for the 'name') and second element numeric (for the 'nDim')")
      }
      x <- list(name = x[[1]], nDim = x[[2]])
    }
    x <- x[sort(names(x))]
    if (!identical(names(x), sort(c("name", "nDim")))) {
      # use sort in the RHS of the identical() to avoid
      # unknown sort ordering in other locales
      stop("Elements of a modelValues varInfo list must be named 'name' and 'nDim'")
    }
    x
  }
  normalizedVars <- lapply(sortedVars, nested_normalize)
  hashedID <- digest::digest(normalizedVars, algo = "crc32")
  hashedID
}

#' @export
modelValues <- function(varInfo, .ID = FALSE, env = parent.frame(), ...) {
  dotsArgs <- list(...)
  # Use legacy mode if legacy_nimble is TRUE or the nimbleModel option is set
  legacy_nimble <- isTRUE(getNimbleModelOption("legacy_nimble")) || isTRUE(dotsArgs$legacy_nimble)
  # But even for legacy mode, when called from modelValueConf, we want to use non-legacy case,
  # so legacy_nimble=FALSE over-rides and gives non-legacy behavior.
  legacy_nimble <- legacy_nimble && !isFALSE(dotsArgs$legacy_nimble)
  if (!legacy_nimble) {
    if (inherits(varInfo, "modelBase_nClass")) {
      varInfo <- get_varInfo_from_nimbleModel(varInfo$modelDef)
    } else if (inherits(varInfo, "modelDefClass")) {
      varInfo <- get_varInfo_from_nimbleModel(varInfo)
    }
    hashedID <- make_modelValues_hashID(varInfo)
    cpp_classname <- Rname2CppName(paste0("MV_", hashedID))
    if (isTRUE(.ID)) {
      return(cpp_classname)
    }
    ans <- make_modelValues_nClass(varInfo, cpp_classname = cpp_classname)
    return(ans)
  }
  # legacy nimble from here down.
  # call could be for prototype modelValues(conf, m), so check for named args in ...
  # or grab ordered args from varInfo and .ID
  if ("conf" %in% names(dotsArgs)) {
    conf <- dotsArgs$conf
    confNamed <- TRUE
  } else {
    conf <- varInfo
    confNamed <- FALSE
  }
  if ("m" %in% names(dotsArgs)) {
    m <- dotsArgs$m
  } else {
    m <- if (confNamed) varInfo else .ID
  }
}
class(modelValues) <- c("function", "nClassBuilder")

#' Create the confs for a custom NIMBLE modelValues object (legacy version)
#'
#' Builds an R-based modelValues conf object
#'
#' @param vars	A vector of character strings naming each variable in the modelValues object
#' @param types	A vector of character strings describing the type of data for the modelValues object.
#' Options include `double' (for real-valued variables) and `int'.
#' @param sizes A list in which the named items of the list match the \code{var} arguments and each item is a numeric vector of the dimensions
#' @param symTab For internal use only
#' @param className For internal use only
#' @param where For internal use only
#' @param modelDef For internal use only
#' @author Clifford Anderson-Bergman
#' @export
#' @details
#' See the \href{https://r-nimble.org/manual/cha-welcome-nimble.html}{User Manual} or \code{help(modelValuesBaseClass)} and \code{help(modelValues)} for information
#'
#' @examples
#' # Custom modelValues object:
#' mvConf <- modelValuesConf(
#'   vars = c("x", "y"),
#'   types = c("double", "int"),
#'   sizes = list(x = 3, y = c(2, 2))
#' )
#' custom_mv <- modelValues(mvConf, m = 2)
#' custom_mv["y", ]
modelValuesConf <- function(symTab, className, vars, types, sizes,
                            modelDef = NA, where = globalenv()) {
  # if(missing(className)) className <- 'modelValuesConf' ## uniqueID will be appended
  if (!missing(className)) stop("modelValuesConf no longer supports the className argument.")
  if (!missing(symTab)) stop("modelValuesConf no longer supports the symTab argument.")
  varsList <- vector("list", length(vars))
  names(varsList) <- vars
  for (i in seq_along(vars)) {
    varsList[[i]] <- list(name = vars[i], nDim = length(sizes[[i]]))
  }
  sizes <- sizes[vars]
  varInfo <- list(vars = varsList, sizes = sizes)
  mvClass <- modelValues(varInfo, .ID = FALSE, legacy_nimble = FALSE)
  ans <- function(nrow = 1L) {
    res <- mvClass$new()
    res$resize(nrow)
    res
  }
  ans
}

## We don't need to overload `[[` or `[[<-` because they
## "just work" by accessing member variables that are nLists.
## However it does return an nList instead of a list.
## To-Do: For nimble backward compatability, we may
##   want to provide an optional way to return a list.
## `[[.modelValues` <- function(x, i) {
##   x[[i]]
## }

#' @exportS3Method
#' @method `[` modelValues
`[.modelValues` <- function(x, var, ind) {
  # nimble attempts to handle missing var (i) by using all vars, but the behavior is unused and broken.
  if (missing(var)) stop("Bracket access of a modelValues object requires a variable in the brackets.")
  # nimble code appears to handle multiple variables but it is untested and broken.
  # It must have been a draft that was not determined be important and not cleaned up.
  if (length(var) != 1) stop("Bracket access of a modelValues object handles only variable at a time.")
  if (missing(ind)) ind <- 1:length(x)
  if (x$isCompiled()) {
    if (length(ind) == 1) {
      nCompiler::value(x, var)[[ind]]
    } else {
      res <- nCompiler::value(x, var)[ind] # nimble returns a list here when nimble2 returns an nList
      if (isTRUE(getNimbleModelOption("legacy_nimble"))) {
        as.list(res)
      } else {
        res
      }
    }
  } else {
    if (length(ind) == 1) {
      x[[var]][[ind]]
    } else {
      res <- x[[var]][ind]
      if (isTRUE(getNimbleModelOption("legacy_nimble"))) {
        as.list(res)
      } else {
        res
      }
    }
  }
}

#' @exportS3Method
#' @method `[<-` modelValues
`[<-.modelValues` <- function(x, var, ind, value) {
  if (missing(var)) stop("Bracket access of a modelValues object requires a variable in the brackets.")
  if (length(var) != 1) stop("Bracket access of a modelValues object handles only variable at a time.")
  if (missing(ind)) ind <- 1:length(x)
  if (x$isCompiled()) {
    if (length(ind) == 1) {
      nCompiler::value(x, var)[[ind]] <- value
    } else {
      nCompiler::value(x, var)[ind] <- value
    } # value could be an nList or a list.
  } else {
    if (length(ind) == 1) {
      x[[var]][[ind]] <- value
    } else {
      x[[var]][ind] <- value
    }
  }
  x
}

modelValues_varNames <- function(x) {
  if (nCompiler::isCompiled(x)) {
    res <- nCompiler::interface_names(x, "members")
    res <- res[!res %in% c("dot_sizeList", "dot_current_nRow")]
    res
  } else {
    x$varInfo$vars |> names()
  }
}

## Early versions of this were cruder.
## Now this code is generic to compiled vs uncompiled
## and also to generic vs. full interface.
#' @exportS3Method
#' @method as.list modelValues
as.list.modelValues <- function(x, varNames, stack = TRUE, flatten = FALSE, iterationAsLastIndex = FALSE, ...) {
  if (missing(varNames)) {
    varNames <- modelValues_varNames(x)
  }
  if (length(varNames)) {
    ans <- vector("list", length(varNames)) |> setNames(varNames)
    for (var in varNames) {
      nl <- nCompiler::value(x, var)
      res <- as.list(nl) # converts nList to list; compiled or uncompiled.
      if (flatten) {
        rawDims <- dimOrLength(res[[1]])
        this_colnames <- nList_2_colnames(nl, rawDims, var = var)
      }
      if (stack) {
        res <- simplify2array(res, higher = !flatten)
        if (is.null(dim(res))) { # scalar var: simplify2array returns a plain vector
          dim(res) <- c(1, length(res))
        }
        if (!iterationAsLastIndex) {
          nDim <- length(dim(res))
          res <- aperm(res, c(nDim, 1:(nDim - 1)))
          if (flatten) {
            colnames(res) <- this_colnames
            names(dimnames(res)) <- c("iteration", "element")
          }
        } else {
          if (flatten) {
            rownames(res) <- this_colnames
            names(dimnames(res)) <- c("element", "iteration")
          }
        }
      } else {
        if (flatten) {
          res <- lapply(res, \(x) {
            dim(x) <- c(1, length(x))
            names(x) <- this_colnames
            x
          })
        }
      }
      ans[[var]] <- res
    }
    return(ans)
  }
  list()
}

# as.list.modelValues <- function(x) {
#   if (x$isCompiled()) {
#     varNames <- nCompiler::interface_names(x, "members")
#     varNames <- varNames[!varNames %in% c("dot_sizeList", "dot_current_nRow")]
#     # Name ordering of nCompiler's interface_names() was updated to guarantee
#     # the order matches the defintion order, so we no longer sort
#     # and the ordering should match a user's expectation.
#     if (!is.null(varNames)) {
#       #varNames <- sort(varNames)
#       return(
#         varNames |> lapply(
#           \(var) as.list(nCompiler::value(x, var))
#         ) |>
#           structure(names = varNames)
#       )
#     }
#   } else {
#     varNames <- x$varInfo$vars |> names()
#     #varNames <- sort(varNames)
#     if (!is.null(varNames)) {
#       return(
#         varNames |> lapply(
#           \(var) as.list(x[[var]])
#         ) |>
#           structure(names = varNames)
#       )
#     }
#   }
#   NULL
# }

#' @exportS3Method
#' @method length modelValues
length.modelValues <- function(x) {
  if (x$isCompiled()) {
    nCompiler::method(x, "getLength")()
  } else {
    x$getLength()
  }
}

#' @exportS3Method
#' @method `length<-` modelValues
`length<-.modelValues` <- function(x, value) {
  if (x$isCompiled()) {
    nCompiler::method(x, "resize")(value)
  } else {
    x$resize(value)
  }
  x
}

dims_2_colnames <- function(dims, var = "x") {
  dim_strings <- dims |>
    lapply(\(x) 1:x) |>
    expand.grid() |>
    apply(1, \(x) paste0(as.character(x), collapse = ", "))
  colnames <- paste0(var, "[", dim_strings, "]")
}

# Column names for the variable held in nList nl.
# A true scalar (nDim 0) is named by the variable name alone, as in nimble,
# whereas a length-1 vector is named with "[1]".
# R values do not distinguish these cases, so we use the nList's element type.
nList_2_colnames <- function(nl, dims, var) {
  if (isTRUE(nl$get_nDim() == 0L)) {
    var
  } else {
    dims_2_colnames(dims, var = var)
  }
}

#' @exportS3Method
#' @method as.matrix modelValues
as.matrix.modelValues <- function(x) {
  varNames <- modelValues_varNames(x)
  # Sorting is not necessary after updating nCompiler to guarantee order matches definition.
  # varNames <- sort(varNames) # could be different from interface_names than definition order
  matrices <- list()
  for (vn in varNames) {
    this_list <- nCompiler::value(x, vn)
    if (length(this_list) == 0) next
    dim_x <- dimOrLength(this_list[[1]])
    tot_len <- prod(dim_x)
    this_colnames <- nList_2_colnames(this_list, dim_x, var = vn)
    # Possibly add check that all elements have the same dim
    this_matrix <- this_list |>
      lapply(\(x) {
        dim(x) <- c(1, tot_len)
        x
      }) |>
      (\(x) do.call("rbind", x))()
    colnames(this_matrix) <- this_colnames
    matrices[[vn]] <- this_matrix
  }
  do.call("cbind", matrices)
}

#' @export
matrix2mv <- function(mat, mv) {
  # inverse of as.matrix.modelValues
  k <- nrow(mat)
  colNames <- colnames(mat)
  colNamesVars <- colNames |> (\(x) sub("\\[.*", "", x))()
  uniqueColNameVars <- colNamesVars |> unique()
  length(mv) <- k
  for (vN in uniqueColNameVars) {
    varInds <- colNamesVars == vN
    numCols <- sum(varInds)
    theseColNames <- colNames[varInds]
    idxPart <- sub("^[^[]*", "", theseColNames) # remove the variable name in case it includes any digits.
    allInds <- lapply(regmatches(idxPart, gregexpr("[0-9]+", idxPart)), as.numeric)
    theseSizes <- do.call("rbind", allInds) |> apply(2, max)

    if (numCols != prod(theseSizes)) {
      stop(paste0("matrix2mv had a problem with the dimensions of ", vN))
    }
    if (length(theseSizes) > 1) {
      res <- apply(mat[, varInds, drop = FALSE], 1, \(x) {
        dim(x) <- theseSizes
        names(x) <- NULL
        x
      },
      simplify = FALSE
      )
    } else {
      res <- apply(mat[, varInds, drop = FALSE], 1, \(x) {
        names(x) <- NULL
        x
      },
      simplify = FALSE
      )
    }
    # We must use value() in case mv is a base class or generic interface.
    if (nCompiler::isCompiled(mv)) {
      nCompiler::value(mv, vN) <- res
    } else {
      mv[[vN]][] <- res
    }
  }
  mv
}

#' Resizes a modelValues object
#'
#' Adds or removes rows to a modelValues object. If rows are added to a modelValues object, the default values are \code{NA}. Works in both R and NIMBLE.
#'
#' @param container		modelValues object
#' @param k				number of rows that modelValues is set to
#' @export
#' @examples
#' mvConf <- modelValuesConf(
#'   vars = c("a", "b"),
#'   types = c("double", "double"),
#'   sizes = list(a = 1, b = c(2, 2))
#' )
#' mv <- modelValues(mvConf)
#' as.matrix(mv)
#' resize(mv, 3)
#' as.matrix(mv)
#'
#' @export
resize <- function(container, k) {
  container$resize(as.integer(k))
}

#' Returns number of rows of modelValues
#'
#' Returns the number of rows of NIMBLE modelValues object. Works in R and NIMBLE.
#'
#' @param container		modelValues object
#'
#' @export
#'
#' @examples
#' mvConf <- modelValuesConf(vars = "a", types = "double", sizes = list(a = 1))
#' mv <- modelValues(mvConf)
#' resize(mv, 10)
#' getsize(mv)
getsize <- function(container) {
  container$getSize()
}
