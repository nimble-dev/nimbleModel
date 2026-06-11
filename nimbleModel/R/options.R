.nimbleModelOptions <- as.environment(
  list(
    allowDynamicIndexing = TRUE,
    prioritizeColonLikeBUGS = TRUE, # if FALSE, 1:2 + 1 evaluates to 2:3, consistent with R.  If TRUE, it evalutes to 1:3, consistent with BUGS
    # TODO: if set to FALSE, will invoke seqNoDecrease, which won't be found
    # based on new rigorous scoping behavior because eval code in env't
    # whose parent is baseenv(). Would like parent to be nimble namespace
    # but this has .GlobalEnv as a parent.
    processBackwardsModelIndexRanges = TRUE,
    disallowMultivariateArgumentExpressions = TRUE,
    nodesAsChars = FALSE,
    verbose = TRUE
  )
)

# sets a single option
#' @export
setNimbleModelOption <- function(name, value) {
  assign(name, value, envir = .nimbleModelOptions)
  invisible(value)
}

#' Get NIMBLE Option
#'
#' Allow the user to get the value of a global _option_
#' that affects the way in which NIMBLE operates
#'
#' @param x a character string holding an option name
#' @author Christopher Paciorek
#' @export
#' @return The value of the option.
#' @examples
#' getNimbleModelOption("verifyConjugatePosteriors")
getNimbleModelOption <- function(x) {
  get(x, envir = .nimbleModelOptions)
}

#' NIMBLE Options Settings
#'
#' Allow the user to set and examine a variety of global _options_
#' that affect the way in which NIMBLE operates. Call \code{nimbleModelOptions()}
#' with no arguments to see a list of available opions.
#'
#' @param ... any options to be defined as one or more \code{name = value} pairs
#' or as a single \code{list} of \code{name=value} pairs.
#' @author Christopher Paciorek
#' @export
#'
#' @details \code{nimbleModelOptions} mimics \code{options}. Invoking
#' \code{nimbleModelOptions()} with no arguments returns a list with the
#'   current values of the options.  To access the value of a single option,
#'    one should use \code{getNimbleModelOption()}.
#'
#' @return
#' When invoked with no arguments, returns a list with the current values of all options.
#' When invoked with one or more arguments, returns a list of the the updated options with their updated values.
#'
#' @examples
#' # Set one option:
#' nimbleModelOptions(verifyConjugatePosteriors = FALSE)
#'
#' # Compactly print all options:
#' str(nimbleModelOptions(), max.level = 1)
#'
#' # Save-and-restore options:
#' old <- nimbleModelOptions() # Saves old options.
#' nimbleModelOptions(
#'   showCompilerOutput = TRUE,
#'   verboseErrors = TRUE
#' ) # Sets temporary options.
#' # ...do stuff...
#' nimbleModelOptions(old) # Restores old options.
nimbleModelOptions <- function(...) {
  invisibleReturn <- FALSE
  args <- list(...)
  if (!length(args)) {
    # Get all nimble options.
    return(as.list(.nimbleModelOptions))
  }
  if (length(args) == 1 && is.null(names(args)) && is.list(args[[1]])) {
    # Unpack a single list of many args.
    args <- args[[1]]
  }
  if (is.null(names(args))) {
    # Get some nimble options.
    args <- unlist(args)
  } else {
    # Set some nimble options.
    for (i in seq_along(args)) {
      setNimbleModelOption(names(args)[[i]], args[[i]])
    }
    args <- names(args)
    invisibleReturn <- TRUE
  }
  out <- as.list(.nimbleModelOptions)[args]
  if (length(out) == 1) out <- out[[1]]
  if (invisibleReturn) {
    return(invisible(out))
  } else {
    return(out)
  }
}

#' Temporarily set some NIMBLE options.
#'
#' @param options a list of options suitable for \code{nimbleModelOptions}.
#' @param expr an expression or statement to evaluate.
#' @return expr as evaluated with given options.
#' @export
#'
#' @examples
#' \dontrun{
#' if (!(getNimbleModelOption("showCompilerOutput") == FALSE)) stop()
#' nf <- nimbleFunction(run = function() {
#'   return(0)
#'   returnType(double())
#' })
#' cnf <- withNimbleModelOptions(list(showCompilerOutput = TRUE), {
#'   if (!(getNimbleModelOption("showCompilerOutput") == TRUE)) stop()
#'   compileNimble(nf)
#' })
#' if (!(getNimbleModelOption("showCompilerOutput") == FALSE)) stop()
#' }
withNimbleModelOptions <- function(options, expr) {
  old <- nimbleModelOptions()
  cleanup <- substitute(do.call(nimbleModelOptions, old))
  do.call(on.exit, list(cleanup, add = TRUE))
  nimbleModelOptions(options)
  return(expr)
}
