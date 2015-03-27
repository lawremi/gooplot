normArg_xlim <- function(xlim) {
  ans <- try(as(xlim, "GRanges"), silent=TRUE)
  if (is(ans, "try-error"))
    stop("'xlim' must be coercible to a GRanges")
  ans
}

normArg_xlab <- function(xlab) {
  if (!isSingleString(xlab))
    stop("'xlab' must be a single, non-NA string")
  xlab
}

normArg_ylab <- function(ylab) {
  if (!isSingleString(ylab))
    stop("'ylab' must be a single, non-NA string")
  ylab
}

normArg_main <- function(main) {
  if (!isSingleString(main))
    stop("'main' must be a single, non-NA string")
  main
}

normArg_query <- function(query) {
  if (!isSingleString(query))
    stop("'query' must be a single, non-NA string")
  
}
