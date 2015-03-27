
setGeneric("autoscales", function(query, mapping, ...)
  standardGeneric("autoscales"),
           signature = "query")
setMethod("autoscales", "ANY", function(query, mapping) list())

## It makes sense for the type of guide to depend on the scale. The
## user can adjust the guide separately from the scale via the
## guides() function.


