# Initial rough drafting of modelValues
library(nCompiler)

modelValuesBase_nClass = nClass(
  classname = "modelValuesBase_nClass",
  Rpublic = list(
    initialize = function(...) {
      super$initialize(...)
    }
  ),
  Cpublic = list(
    sizes = 'RcppList',
    current_nRow_ = 'integerScalar',
    modelValuesBase_nClass = nFunction(
      function() {
        super$initialize()
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
    set_sizes = nFunction(
      name = "set_sizes",
      fun = function(new_sizes = "RcppList") {
        self$sizes <<- new_sizes
      }
    )
  ),
  predefined = quote(system.file(file.path("include", "nimbleModel", "predef"),
                                 package = "nimbleModel") |> file.path("modelValuesBase_nClass")),
  compileInfo = list(
    interface = "full",
    createFromR = FALSE,
    exportName = "modelValuesBase_nClass_new",
    packageNames = c(uncompiled = "modeValuesBase_nClass_R", compiled = "modelValuesBase_nClass")
  )
)

modelValues_resize <- function(self, m, sizes) {
  # check preservation issues in resizing
  for(v in names(sizes)) {
    this_sizes <- sizes[[v]]
    length(self[[v]]) <<- m
    if(length(this_sizes)==1) {
      for(i in 1:m) self[[v]][[i]] <- numeric(length = this_sizes)
    } else {
      for(i in 1:m) self[[v]][[i]] <- array(0, dim = this_sizes)
    }
  }
}

make_modelValues_nClass <- function(varInfo,
                                    classname) {
  e <- environment()
  CpublicVars <- varInfo$vars |>
    lapply(\(x) {
            nDim <- x$nDim
            nLname <- paste0("nL", nDim, "D")
            if(!exists(nLname, envir = e)) {
              e[[nLname]] <- eval(substitute(nList(numericArray(nDim = NDIM)), list(NDIM=nDim)))
            }
            paste0(nLname)
          }
        )
  names(CpublicVars) <- varInfo$vars |>
    lapply(\(x) x$name) |>
    unlist()
  ctor_lines <- varInfo$vars |>
    lapply(\(x) {
            nDim <- x$nDim
            nLname <- paste0("nL", nDim, "D")
            substitute(V <<- NL$new(),
              list(V = as.name(x$name), NL = as.name(nLname)))
    }
  )
  resize_lines <- varInfo$vars |>
    lapply(\(x) {
            nDim <- x$nDim
            nLname <- paste0("nL", nDim, "D")
            Cline <- gsub("NDIM", nDim, "resize_one<NDIM>(V, m, as<SEXP>(this->sizes[NAME]))")
            Cline <- gsub("NAME", paste0("\"", x$name, "\""), Cline)
            Cline <- gsub("V", x$name, Cline)
           substitute(nCpp(CLINE), list(CLINE = Cline))
    }
  )
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
  body(c_resize_fun) <- as.call(c(as.name("{"), resize_lines))

  classname <- "modelValues_class_make_this_generic"

  CPUBLIC <- c(
    list(
      CLASSNAME = nFunction(
        fun = c_ctor_fun,
        compileInfo = list(
          constructor = TRUE
        )
      ),
      resize = nFunction(
        name = "resize",
        fun = function(m) {
          modelValues_resize(self, m, sizes)
          # check preservation issues in resizing
        },
        compileInfo = list(
          C_fun = c_resize_fun
        ),
        argTypes = list(m = "integerScalar")
      ),
      getLength = nFunction(
        function() {return(current_nRow_); returnType('integerScalar')}
      )
    ),
    CpublicVars
  )

  names(CPUBLIC)[1] <- classname

  generator_code <- substitute(
    nClass(
      classname = CLASSNAME,
      inherit = nimbleModel:::modelValuesBase_nClass,
      compileInfo = list(
        nClass_inherit = list(base = "modelValuesClass_")
      ),
      Rpublic = list(
        initialize = function(...) {
          super$initialize(...)
          if(!isCompiled()) {
            CLASSNAME()
          }
        }
      ),
      Cpublic =
        CPUBLIC
    ),
    list(CLASSNAME = classname)
  )
  generator <- eval(generator_code)
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
