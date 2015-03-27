## High-level position class, just a marker for now
setOldClass(c("Position", "proto", "environment"))

autoposition_for_geom <- function(geom) {
  ggplot2_geom(geom)$default_position()
}

setGeneric("autoposition", function(query, geom, ...)
  standardGeneric("autoposition"))
setMethod("autoposition", c("ANY", "ANY"),
          function(query, geom) autoposition_for_geom(geom))

