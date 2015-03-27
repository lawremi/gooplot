## Putting the pieces together: Layer objects

setClass("Layer",
         representation(query="QueryORNULL",
                        geom="Geom",
                        mappings="Aes",
                        position="Position"))

setClass("LayerList", prototype=prototype(elementType="Layer"),
         contains="SimpleList")

## So that people can easily add just a layer...
Layer <- function(object,
                  query=autoquery(object),
                  geom=autogeom(query),
                  mapping=NULL,
                  position=autoposition(geom))
{
  new("Layer", query=query, geom=geom, mappings=mappings, position=position)  
}
