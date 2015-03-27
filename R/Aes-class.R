autoaes_for_geom <- function(geom) {
  ggplot2_geom(geom)$default_aes()
}

setGeneric("autoaes", function(query, geom, ...)
  standardGeneric("autoaes"))
setMethod("autoaes", c("ANY", "ANY"),
          function(query, geom) autoaes_for_geom(geom))

## High-level aesthetic class, at least for pretty printing
setOldClass(c("Aes", "uneval"))
