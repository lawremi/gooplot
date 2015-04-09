## Compilation algorithm:
## - Execute plot-level query
## - fortify() the query result to df, and set to plot$data
## - For each layer:
##   - if it is a QueryLayer:
##     - if there is a query: execute the query
##     -    else: take result from plot
##     - apply Position to query result
##     - request molds from Geom, named by ggplot2 geom
##     - execute molds from positioned query result
##     - construct ggplot2 layers from mold results
##   - add layer(s) to plot$layers

setGeneric("compile", function(x, target, ...) standardGeneric("compile"))

setMethod("compile", c("Bioplot", "missing"), function(x, target, ...) {
  compile(x, x@ggplot, ...)
})

setMethod("compile", c("Bioplot", "ggplot"), function(x, target) {
  d <- x@query()
})

## IDEA: dispatch compilation on the high-level AND low-level plot objects,
##       so that we can target implementations other than ggplot2.
##       currently, the API is obviously dependent on ggplot2,
##       but that might eventually change.
##       in other words, multiple dispatch on source and target
