# Initial rough drafting of modelValues

modelValuesBase_nClass <- nCompiler::nClass(
  classname = "modelValuesBase_nClass",
  Rpublic = list(
    initialize = function(...) {
      super$initialize(...)
      CppObj_provided <- "CppObj" %in% ...names()
      if(!CppObj_provided) {
        if(!isCompiled()) {
          modelValuesBase_nClass()
          self$sizes <- self$defaultSizes
        }
      }
    }
  ),
  Cpublic = list(
    sizes = "RcppList",
    current_nRow_ = "integerScalar",
    modelValuesBase_nClass = nCompiler::nFunction(
      function() {
        current_nRow_ <<- 0
        sizes <<- list()
      },
      compileInfo = list(
        constructor = TRUE,
        C_fun = function() {
          current_nRow_ <- 0
          cppLiteral("this->sizes = Rcpp::List();")
        }
      )
    ),
    set_sizes = nCompiler::nFunction(
      name = "set_sizes",
      fun = function(new_sizes = "RcppList") {
        self$sizes <<- new_sizes
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
        return(current_nRow_)
        returnType("integerScalar")
      },
      compileInfo = list(virtual = TRUE)
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"),
    package = "nimbleModel"
  ) |> file.path("modelValuesBase_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = FALSE,
    exportName = "modelValuesBase_nClass_new",
    packageNames = c(uncompiled = "modeValuesBase_nClass_R", compiled = "modelValuesBase_nClass")
  )
)

modelValues_resize <- function(self, m, sizes) {
  # check preservation issues in resizing
  for (v in names(sizes)) {
    this_sizes <- sizes[[v]]
    length(self[[v]]) <- m
    if (length(this_sizes) == 1) {
      for (i in 1:m) self[[v]][[i]] <- numeric(length = this_sizes)
    } else {
      for (i in 1:m) self[[v]][[i]] <- array(0, dim = this_sizes)
    }
  }
}

make_modelValues_nClass <- function(varInfo,
                                    classname) {
  if(missing(classname)) {
    hashedID <- make_modelValues_hashID(varInfo)
    classname <- Rname2CppName(paste0("MV_", hashedID))
  }
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
      nLname <- paste0("nL", nDim, "D")
      Cline <- gsub("NDIM", nDim, "resize_one<NDIM>(V, m, as<SEXP>(this->sizes[NAME]))")
      Cline <- gsub("NAME", paste0("\"", x$name, "\""), Cline)
      Cline <- gsub("V", x$name, Cline)
      substitute(nCpp(CLINE), list(CLINE = Cline))
    })
  # function() {
  #       mu <<- nL1D$new()
  #       cov <<- nL2D$new()
  #     }
  c_ctor_fun <- function() {}
  body(c_ctor_fun) <- as.call(c(as.name("{"), ctor_lines))
  # function(m = 'integerScalar') {
  #             nCpp("resize_one<1>(mu, m, this->sizes[\"mu\"])")
  #             nCpp("resize_one<2>(cov, m, this->sizes[\"cov\"])")
  #             current_nRow_ <<- m
  #           }
  c_resize_fun <- function(m) {}
  body(c_resize_fun) <- as.call(c(as.name("{"), resize_lines, list(quote(current_nRow_ <- m))))

  CPUBLIC <- c(
    list(
      CLASSNAME = nCompiler::nFunction(
        fun = c_ctor_fun,
        compileInfo = list(
          constructor = TRUE
        )
      ),
      resize = nCompiler::nFunction(
        name = "resize",
        fun = function(m) {
          modelValues_resize(self, m, sizes)
          self$current_nRow_ <- m
          # check preservation issues in resizing
        },
        compileInfo = list(
          C_fun = c_resize_fun
        ),
        argTypes = list(m = "integerScalar")
      )
    ),
    CpublicVars
  )

  names(CPUBLIC)[1] <- classname

  defaultSizes <- varInfo$sizes
  if(is.null(defaultSizes)) defaultSizes <- list()
  for(v in names(varInfo$vars)) {
    if(is.null(defaultSizes[[v]]) || length(defaultSizes[[v]]) == 0) {
      defaultSizes[[v]] <- rep(0, varInfo$vars[[v]]$nDim)
    }
  }

  generator_code <- substitute(
    nCompiler::nClass(
      classname = CLASSNAME,
      inherit = modelValuesBase_nClass,
      compileInfo = list(
        nClass_inherit = list(base = "modelValuesClass_")
      ),
      Rpublic = list(
        defaultSizes = defaultSizes,
        varInfo = varInfo,
        initialize = function(...) {
          super$initialize(...)
          if (!isCompiled()) {
            CLASSNAME_()
          }
        }
      ),
      Cpublic =
        CPUBLIC
    ),
    list(CLASSNAME = classname,
        CLASSNAME_ = as.name(classname))
  )
  generator <- eval(generator_code)
  generator$set("public", "NCgenerator", generator)
  generator
}

## This is automatic native behavior anyway
## However it returns an nList instead of a list.
## to-do: follow-up for backward compatability.
## `[[.modelValues` <- function(x, i) {
##   x[[i]]
## }

#' @export
`[.modelValues` <- function(x, var, ind) {
  x[[var]][[ind]]
}

#' @export
`[<-.modelValues` <- function(x, var, ind, value) {
  x[[var]][[ind]] <- value
  x
}

make_modelValues_hashID <- function(varInfo) {
  # We go to some lengths to normalize
  # so that we obtain the same hash for equivalent varInfo.
  varsList <- varInfo$vars
  sortedNames <- sort(names(varsList))
  sortedVars <- varsList[sortedNames]
  nested_normalize <- function(x) {
    if(!is.list(x)) 
      stop("Elements of a modelValues varInfo list must be lists with elements 'name' and 'nDim' (or unnamed elements in that order)")
    if(is.null(names(x))) {
      if(length(x) != 2) {
        stop("Elements of a modelValues varInfo list must have 2 elements (optionally named 'name' and 'nDim')")
      }
      if(!is.character(x[[1]]) || !is.numeric(x[[2]])) {
        stop("Elements of a modelValues varInfo list, if not named, must have first element character (for the 'name') and second element numeric (for the 'nDim')")
      }
      x <- list(name = x[[1]], nDim = x[[2]])
    }
    x <- x[sort(names(x))]
    if(!identical(names(x), sort(c("name", "nDim")))) {
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
modelValues <- function(varInfo, .ID = FALSE, env = parent.frame()) {
  if(inherits(varInfo, "modelBase_nClass")) {
    varInfo <- get_varInfo_from_nimbleModel(varInfo)
  }
  hashedID <- make_modelValues_hashID(varInfo)
  classname <- Rname2CppName(paste0("MV_", hashedID))
  if(isTRUE(.ID)) {
    return(classname)
  }
  ans <- make_modelValues_nClass(varInfo, classname = classname)
  ans
}
class(modelValues) <- c("function", "nClassBuilder")
