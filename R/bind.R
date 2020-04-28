#' Combining \pkg{sp} class objects
#'
#' Utilities to combine instances of \pkg{sp} classes.
#' Perhaps these would have a better home in \pkg{sp} itself,
#' but until then, here we are.
#'
#' @param x,... Multiple \linkS4class{SpatialPoints} objects of the same dimensionality.
#'
#' Alternatively, multiple \linkS4class{SpatialPolygons} objects.
#' @param ignore.mcols,recursive Further arguments passed to \code{\link{c,Vector-method}}.
#'
#' @return An object of the same class as \code{x} containing coordinates from all objects in \code{...}.
#'
#' @author Aaron Lun
#'
#' @examples
#' # SpatialPoints:
#' coords <- matrix(runif(30), nrow=10)
#' X <- sp::SpatialPoints(coords)
#'
#' coords2 <- matrix(runif(15), nrow=5)
#' Y <- sp::SpatialPoints(coords2)
#'
#' c(X, Y, X, Y) 
#'
#' # SpatialPolygons:
#' spm <- makeSpatialPolygons(
#'     rbind(c(0,0), c(0.5, 0.2), c(0.2, 0.6), c(0.9, 0.8)),
#'     rbind(c(1,1), c(0.3, 0.7), c(0.1, 0.2), c(0.8, 0.3))
#' )
#'
#' spm2 <- makeSpatialPolygons(
#'     matrix(runif(10), ncol=2),
#'     matrix(rnorm(8), ncol=2)
#' )
#'
#' c(spm, spm2)
#'
#' @name spatula-bind
#'
#' @export
#' @importMethodsFrom S4Vectors c
setMethod("c", "SpatialPoints", function(x, ..., ignore.mcols = FALSE, recursive = FALSE) {
    stuff <- lapply(list(...), .PointsVector)
    out <- do.call(c, c(list(.PointsVector(x)), stuff, list(ignore.mcols=ignore.mcols, recursive=recursive)))
    out@points
})

#' @export
#' @rdname spatula-bind
#' @importMethodsFrom S4Vectors c
setMethod("c", "SpatialPolygons", function(x, ..., ignore.mcols = FALSE, recursive = FALSE) {
    stuff <- lapply(list(...), .PolygonsVector)
    out <- do.call(c, c(list(.PolygonsVector(x)), stuff, list(ignore.mcols=ignore.mcols, recursive=recursive)))
    out@polygons
})
