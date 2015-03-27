## Choosing a geom based on a query.

## The challenge: how to represent a geom? The ggplot2 API hides the
## notion of a geom being an object. Instead, each geom has a geom_[x]
## function that constructs a *layer* containing that geom. The hidden
## geom object is a factory of the layer. The user typically
## communicates the geom by its name (like in qplot). The name is
## looked up against all Geom* symbols, going up from the ggplot2
## namespace to the global environment. To avoid dealing with ggplot2
## internals, perhaps the autogeom generic should return the geom
## name. This is how the user would provide it anyway.

## It is then not clear:
## (a) how to obtain default stat/position/aes for geom AND query
##     - autostat, etc are generics dispatching on query,
##       methods take geom (name) as argument
##     - introduce our own class hierarchy of geoms, with auto*
##       dispatching on both the geom and request
## (b) how to look up the geom by name without using internals
##     - Reimplement search for GeomX and call $New?
##     - Find geom_X function and call it to make the layer?
##     - Search for our own GeomX class?

setClass("Geom")
setClass("GeomBar", contains="Geom")
setClass("GeomPoint", contains="Geom")

camelToUnderscore <- function(x) {
  sub("^_", "", tolower(gsub("([A-Z])", "_\\1", x)))
}

setMethod("names", "Geom", function(x) {
  camelToUnderscore(sub("^Geom", "", class(x)))
})

setGeneric("autogeom", function(x, ...) standardGeneric("autogeom"))
setMethod("autogeom", "QueryAggregate", function(x) new("GeomBar"))
