#' Comparisons for \pkg{sp} classes
#'
#' Implements comparison methods for \pkg{sp} classes including ordering and matching.
#'
#' @section Sorting:
#' \code{order(..., na.last=TRUE, decreasing=FALSE, method=c("auto", "shell", "radix"))} 
#' returns an integer permutation vector that arranges the first argument of \code{...} in ascending or descending order,
#' see \code{?"base::order"} for more details.
#' \code{...} should contain one or more \linkS4class{SpatialPoints} objects of the same dimensionality;
#' or one or more \linkS4class{SpatialPolygons} objects.
#'
#' \code{sort(x, decreasing=FALSE, ...)} returns \code{x} in sorted order, see \code{?"base::sort"} for more details.
#' \code{x} may be a \linkS4class{SpatialPoints} or \linkS4class{SpatialPolygons} object.
#'
#' Ordering of points inside a \linkS4class{SpatialPoints} object is determined by the value of the first dimension;
#' and then the second dimension, if the first is tied; and so on for all available dimensions.
#' Ordering of polygons inside a \linkS4class{SpatialPolygons} object is determined by the coordinates of the polygon centroid,
#' with comparisons between successive dimensions as described above.
#'
#' @section Duplicates:
#' \code{duplicated(x, incomparables=FALSE, ...)} returns a logical vector indicating whether each entry of \code{x}
#' is a duplicate of a previous elemen, see \code{?"base::duplicated"} for more details.
#' \code{x} may be a \linkS4class{SpatialPoints} or \linkS4class{SpatialPolygons} object.
#'
#' \code{unique(x, incomparables=FALSE, ...)} returns \code{x}
#' after removing all of the duplicated points, see \code{?"base::unique"} for more details.
#' \code{x} may be a \linkS4class{SpatialPoints} or \linkS4class{SpatialPolygons} object.
#'
#' In a \linkS4class{SpatialPoints} object, a point is considered as duplicate of another point if they have exactly the same coordinates.
#' No consideration is given towards numerical imprecision.
#' In a \linkS4class{SpatialPolygons} object, polygons are considered to be duplicated if they have the same coordinates
#' and other attributes (e.g., ring direction, whether or not it is a hole).
#'
#' @section Comparisons:
#' Boolean operations like \code{x == y} can be performed if both \code{x} and \code{y} are:
#' \itemize{
#' \item two \linkS4class{SpatialPoints} objects of the same dimensionality.
#' \item two \linkS4class{SpatialPolygons} objects.
#' }
#' This returns a logical vector indicating whether each point in \code{x} is equal to, less than or greater than 
#' (depending on the operator) the corresponding point in \code{y}.
#' If \code{x} and \code{y} are not of the same length, the shorter object is recycled.
#'
#' \code{match(x, table, nomatch=NA_integer, incomparables=FALSE, ...)} 
#' returns an integer vector of length equal to that of \code{x}.
#' Each element of this vector specifies an entry in \code{table} that is equal to the corresponding entry in \code{x}.
#'
#' In \linkS4class{SpatialPoints} objects, points are compared to each other by considering the vectors of their coordinates.
#' If all coordinates are exactly equal, so are the points;
#' the first unequal coordinate is used to determine which point is \dQuote{greater than} or \dQuote{less than} the other.
#' In \linkS4class{SpatialPolygons} objects, polygons are compared to each other by considering the vectors of their centroid coordinates.
#'
#' @author Aaron Lun
#'
#' @examples
#' # For SpatialPoints:
#' coords <- matrix(runif(30), nrow=10)
#' X <- sp::SpatialPoints(coords)
#'
#' order(X)
#' sort(X)
#' match(X, X[2:5])
#' unique(X[c(1,1,1,2,2,3)])
#'
#' X==X
#' coords2 <- matrix(runif(15), nrow=5)
#' Y <- sp::SpatialPoints(coords2)
#' X < Y
#' X > Y
#'
#' # For SpatialPolygons:
#' spm <- makeSpatialPolygons(
#'     rbind(c(0,0), c(0.5, 0.2), c(0.2, 0.6), c(0.9, 0.8)),
#'     rbind(c(1,1), c(0.3, 0.7), c(0.1, 0.2), c(0.8, 0.3))
#' )
#'
#' order(spm)
#' sort(spm)
#' match(spm, spm[2:1])
#' unique(c(spm, spm))
#'
#' spm==spm
#' spm2 <- makeSpatialPolygons(
#'     rbind(c(0,0), c(0.4, 0.2), c(0.2, 0.6), c(0.9, 0.8)),
#'     rbind(c(1,1.1), c(0.3, 0.7), c(0.1, 0.2), c(0.8, 0.3))
#' )
#' spm < spm2
#' spm > spm2
#'
#' @docType methods
#' @name spatula-comparisons
#' @aliases
#' order,SpatialPoints-method
#' sort,SpatialPoints-method
#' unique,SpatialPoints-method
#' duplicated,SpatialPoints-method
#' Ops,SpatialPoints,SpatialPoints-method
#' match,SpatialPoints,SpatialPoints-method
#' %in%,SpatialPoints,SpatialPoints-method
#'
#' order,SpatialPolygons-method
#' sort,SpatialPolygons-method
#' unique,SpatialPolygons-method
#' duplicated,SpatialPolygons-method
#' Ops,SpatialPolygons,SpatialPolygons-method
#' match,SpatialPolygons,SpatialPolygons-method
#' %in%,SpatialPolygons,SpatialPolygons-method
NULL

#####################
### SpatialPoints ###
#####################

#' @export
setMethod("order", "SpatialPoints", function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
    vectorized <- lapply(list(...), .PointsVector)
    do.call(order, c(vectorized, list(na.last=na.last, decreasing=decreasing, method=match.arg(method))))
})

#' @export
setMethod("sort", "SpatialPoints", function(x, decreasing = FALSE, ...) {
    out <- sort(.PointsVector(x), decreasing=decreasing, ...)
    out@spatial
})

#' @export
setMethod("unique", "SpatialPoints", function(x, incomparables=FALSE, ...) {
    out <- unique(.PointsVector(x), incomparables=incomparables, ...)
    out@spatial
})

#' @export
setMethod("duplicated", "SpatialPoints", function(x, incomparables=FALSE, ...) {
    duplicated(.PointsVector(x), incomparables=incomparables, ...)
})

#' @export
setMethod("Ops", c("SpatialPoints", "SpatialPoints"), function(e1, e2) {
    callGeneric(.PointsVector(e1), .PointsVector(e2))
})

#' @export
#' @importFrom BiocGenerics match
setMethod("match", c("SpatialPoints", "SpatialPoints"), function(x, table, 
    nomatch = NA_integer_, incomparables = NULL, ...)
{
    match(.PointsVector(x), .PointsVector(table), nomatch=nomatch, incomparables=incomparables, ...)
})

#' @export
#' @importFrom BiocGenerics %in%
setMethod("%in%", c("SpatialPoints", "SpatialPoints"), function(x, table) {
    .PointsVector(x) %in% .PointsVector(table)
})

#######################
### SpatialPolygons ###
#######################

#' @export
setMethod("order", "SpatialPolygons", function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
    vectorized <- lapply(list(...), .PolygonsVector)
    do.call(order, c(vectorized, list(na.last=na.last, decreasing=decreasing, method=match.arg(method))))
})

#' @export
setMethod("sort", "SpatialPolygons", function(x, decreasing = FALSE, ...) {
    out <- sort(.PolygonsVector(x), decreasing=decreasing, ...)
    out@spatial
})

#' @export
setMethod("unique", "SpatialPolygons", function(x, incomparables=FALSE, ...) {
    out <- unique(.PolygonsVector(x), incomparables=incomparables, ...)
    out@spatial
})

#' @export
setMethod("duplicated", "SpatialPolygons", function(x, incomparables=FALSE, ...) {
    duplicated(.PolygonsVector(x), incomparables=incomparables, ...)
})

#' @export
setMethod("Ops", c("SpatialPolygons", "SpatialPolygons"), function(e1, e2) {
    callGeneric(.PolygonsVector(e1), .PolygonsVector(e2))
})

#' @export
#' @importFrom BiocGenerics match
setMethod("match", c("SpatialPolygons", "SpatialPolygons"), function(x, table, 
    nomatch = NA_integer_, incomparables = NULL, ...)
{
    match(.PolygonsVector(x), .PolygonsVector(table), nomatch=nomatch, incomparables=incomparables, ...)
})

#' @export
#' @importFrom BiocGenerics %in%
setMethod("%in%", c("SpatialPolygons", "SpatialPolygons"), function(x, table) {
    .PolygonsVector(x) %in% .PolygonsVector(table)
})
