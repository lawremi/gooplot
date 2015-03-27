setGeneric("autoquery",
           function(object, ...) standardGeneric("autoquery"))

## Design of query objects: Instead of having a special class for
## every query type and data source, we could have a generic (eval) that
## dispatches on a "query type" object and the data source.

## The query is implemented by a function, usually a generic that
## dispatches on the object type. This makes it super easy to write
## new/optimized queries. We could store the parameters by storing a
## prototypical call to the function. The query is represented by that
## call. When plot parameters are changed (like the xlim), we override
## the parameters in the call.

## What sort of API should be enforced for the callback, i.e., which
## parameters will the user be able to adjust? Under the realization
## that the query is abstractly equivalent to a stat, there could be a
## mapping between aesthetics and query parameters. Take the need to
## restrict by 'xlim' for example. We are drawing genes. At the bottom
## is a rect geom, with its left/right aesthetics mapped to
## start/end. At the top is a gene/alignment geom, the data are a
## GRangesList, and there is an abstract mapping of x=>range. So
## perhaps the 'xlim' could then be passed as the 'range' argument to
## the query function? Same for 'ylim', but there would be no 'y'
## aesthetic for the genes. We will need to modularize this logic,
## because sometimes we do not want to map the plot xlim to a query
## restriction.

## But dispatching to choose the right geom (and facets) will not work
## with a plain function, so we need a special class.

## TO TENGFEI: feel free to email me and tell me I'm crazy

setClass("QueryFunction", contains = "standardGeneric")

setClass("Query", contains = "call")

setClass("QueryAggregate", contains = "Query")

QueryConstructor <- function(FUN, ...) {
  body <- substitute({
    mc <- match.call()
    mc[[1]] <- FUN
    new(CLASS, mc)
  }, list(CLASS=deparse(substitute(FUN)), FUN=FUN))
  as.function(c(formals(FUN), body))
}

query_aggregate <- QueryConstructor(aggregate)

setMethod("autoquery", "BigWigFile", function(object) query_aggregate(object))

setMethod("aggregate", "BigWigFile",
          function(x, binwidth = 1L, xlim = seqinfo(x)) {
            xlim <- normArg_xlim_GRanges(xlim)
            if (binwidth == 1L) {
              import(object, which=xlim, ...)
            } else {
              size <- round(width(xlim) / binwidth)
              summary(object, size=size, which=xlim, ...)
            }
          })

## call("foo")[-1] yields NULL, so there is no way to have a no-op call
setClassUnion("QueryORNULL", c("Query", "NULL"))

