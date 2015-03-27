setGeneric("autostat", function(query, geom, ...)
  standardGeneric("autostat"))

setMethod("autostat", c("ANY", "ANY"),
          function(query, geom) autostat_for_geom(geom))
setMethod("autostat", c("QueryAggregate", "GeomBar"),
          function(query, geom) "identity")
