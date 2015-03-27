setClass("Plot",
         representation(query="Query",
                        layers="LayerList"))

setMethod("+", c("Plot", "Layer"), function(e1, e2) {
  e1@layers <- c(e1@layers, e2)
  e1
})

##' A modular approach to constructing a plot. The idea is to rely on
##' S4 generics for determining the appropriate plot components from
##' the type of object.
##'
##' A grammatical (e.g., ggplot2) plot consists of a series of
##' layers. The layer holds the data, as well as the geom, stat,
##' position and all related parameters. Other things, like the
##' scales, labels, faceting and coordinate system transcend layers.
##'
##' We introduce to the grammar the notion of a data query. The query
##' retrieves data from a data source when rendering the plot. The
##' object returned by the query may be of any type, although
##' implementations may depend on coercion to a particular type, like
##' a data frame. The query relies on the aesthetic mapping to
##' determine, for example, which attribute of the data corresponds to
##' the xlim and ylim of the plot when restricting itself (in some
##' cases, this is undesirable, so the query mappings will need to be
##' configurable).
##'
##' In a call to Plot(), there is conceptually a *single* layer. This
##' means a single query. The geoms may be complex and consist of
##' other, more atomic geoms. The atomic (leaf) geoms should be
##' compilable to an implementation geom, and the implementation layer
##' will qqbe passed the result of querying and coercing the data.
##' There is a *single* position that is applied to the compound geom
##' 
##' Consider the complex example of drawing transcript structures: The
##' geom is compound, at least consisting of a different geom for the
##' exons and introns. The position is custom: dodge (stepping). The
##' query yield a GRanges, with a
##' grouping variable by transcript. The transcript geom does no
##' molding. The position is a scalar, so it is applied only at the
##' top-level. The GRanges with Y adjustment is passed down to the
##' atomic phase. The stat/position are identity, and there are two
##' geoms, say rect and chevron. The chevron is a custom geom, there
##' is a mold() applied so that the segment geom draws chevrons.
##' 
##' Geoms could also be dynamic, in that they would generate a
##' different set of moldings and low-level geoms depending on the
##' amount of data to display. But we will defer that until
##' later. Probably would be based on xlim/ylim, rather than the coord
##' limits, which would be more like a physical zoom.
##'  
##' Once the layers are compiled, they are combined with the result of
##' compiling the scales, labels, guides, facetting, coqordinate
##' system and theme into a final plot. All of these are automatically
##' chosen based on the type of object.
##'                     
##' Overall flow of pipeline:
##' Data =query=> =position=>
##' `-> =mold1=> df1 [=scales=> =stat1=> =coord=> =geom1=\        ]
##' `-> =mold2=> df2 [=scales=> =stat2=> =coord=> =geom2=/`-> plot]
##' Stuff inside [ ] is implemented by ggplot2.
##' 
##' @title Construct a plot
##' @param object The object to display.
##' @param query A Query object that accesses a data source and
##' reduces the data to yield one or more data.frames. This can
##' involve a significant amount of processing and is a large
##' determinant of the plot itself. Each data.frame is drawn by a
##' separate layer (geom).
##' @param geom Draws the data based on some geometry.  This is the
##' primary way to indicate the plot type. Default chosen based on the
##' query. Note that multiple geoms can be passed, resulting in one
##' layer per geom.
##' @param mapping User-specified aesthetic mappings that connect
##' geometric parameters with variables in the data.  These override
##' the defaults and are plot-wide, even when there are multiple layers.
##' @param position Position adjustment for overlapping
##' geometry. Default indicated by the geom.
##' @param facets Faceting into small multiple plots. Default chosen
##' based on the query. Typically need to facet by sequence.
##' @param scales One or more scales, each one of which scales the
##' data to one particular aesthetic.
##' @param xlim x limits, typically a GRanges
##' @param ylim y limits
##' @param main title
##' @param xlab x axis label
##' @param ylab y axis label
##' @param ... Parameters that apply to the mapping, geom, stat, and
##' query, in order.
##' @param aes the result of merging the user mappings with the
##' default aesthetics; other argument defaults depend on this.
##' @return some sort of plot object
##' @author Michael Lawrence
Plot <- function(object,
                 query=autoquery(object),
                 geom=autogeom(query),
                 mapping=NULL,
                 position=autoposition(geom),
                 facets=autofacets(query),
                 xlim=c(NA_real_, NA_real_), ylim=c(NA_real_, NA_real_),
                 main=NULL,
                 xlab=xlab(aes), ylab=ylab(aes),
                 ...,
                 aes=merge(mapping, autoaes(query)))
{
  query <- normArg_query(query)
  geom <- normArg_geom(geom)
  mapping <- normArg_mapping(mapping)
  facets <- normArg_facets(facets)

  layers <- QueryLayer(query, geom=geom, aes=aes, position=position)
  
  scales <- autoscales(query, aes)
  
  p <- ggplot() + layers + facets + scales
  
  if (!is.null(main)) {
    p <- p + ggtitle(main)
  }
  if (!missing(xlab)) {
    p <- p + xlab(xlab)
  }
  if (!missing(ylab)) {
    p <- p + ylab(ylab)
  }
  if (!missing(xlim)) {
    p <- p + xlim(xlim)
  }
  if (!missing(ylim)) {
    p <- p + ylim(ylim)
  }
  
  p
}

print.Plot <- function(x, ...) {
  p <- compile(x)
  print(p, ...)
}

setMethod("show", "Plot", function(object) {
  print(object)
})
