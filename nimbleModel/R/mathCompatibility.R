# "Logical" (sensu BUGS) functions for compatibility with BUGS

linkInverses <- list(
  "logit" = quote(expit()),
  "log" = quote(exp()),
  "probit" = quote(phi()),
  "cloglog" = quote(icloglog())
)

# The BUGS math functions (e.g., `expit`, `cloglog`, `pow`, `inprod`) are provided by nCompiler.
# `step`, `equals`, `logdet` and `inverse` in model code are changed to nCompiler's `nStep`, `nEquals`,
# `nLogdet` and `nInverse` (see `nmKeywords` and nCompiler's `nKeyWords`).
