layer <- function(geom, ...) {
  fun <- match.fun(paste0("geom_", names(geom)))
  fun(...)
}

ggplot2_geom <- function(geom) {
  layer(geom)$geom
}

autostat_for_geom <- function(geom) {
  ggplot2_geom(geom)$default_stat()
}

setOldClass(c("proto", "environment"))

setClassUnion("ggplot2Layer", c("proto", "Layer"))

setClass("ggplot2Plot",
         representation(ggplot="ggplot"),
         contains="Plot")

setMethod("+", c("ggplot2Plot", "ANY"), function(e1, e2) {
  e1@ggplot <- e1@ggplot + e2
  e1
})
