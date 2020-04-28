#' Comparisons for \pkg{sp} classes
#'
#' Implements comparison methods for \pkg{sp} classes including ordering and matching.
#'
#' @section Sorting:
#' \code{order(..., na.last=TRUE, decreasing=FALSE, method=c("auto", "shell", "radix"))} 
#' returns an integer permutation vector that arranges the first argument of \code{...} in ascending or descending order,
#' see \code{?"base::order"} for more details.
#' \code{...} should contain \linkS4class{SpatialPoints} objects of the same dimensionality.
#'
#' \code{sort(x, decreasing=FALSE, ...)} returns the \linkS4class{SpatialPoints} object \code{x} in sorted order,
#' see \code{?"base::sort"} for more details.
#'
#' Ordering of points inside a \linkS4class{SpatialPoint} object is determined by the value of the first dimension;
#' and then the second dimension, if the first is tied; and so on for all available dimensions.
#'
#' @section Duplicates:
#' \code{duplicated(x, incomparables=FALSE, ...)} returns a logical vector indicating whether a 
#' of a \linkS4class{SpatialPoints} object \code{x} is a duplicate of a previous element.
#' See \code{?"base::duplicated"} for more details.
#'
#' \code{unique(x, incomparables=FALSE, ...)} returns the \linkS4class{SpatialPoints} object \code{x}
#' after removing all of the duplicated points, see \code{?"base::unique"} for more details.
#'
#' A point is considered as duplicate of another point if they have exactly the same coordinates.
#' No consideration is given towards numerical imprecision.
#'
#' @section Comparisons:
#' Boolean operations like \code{x == y} can be performed if both \code{x} and \code{y}
#' are \linkS4class{SpatialPoints} objects of the same dimensionality.
#' This returns a logical vector indicating whether each point in \code{x} is equal to, less than or greater than 
#' (depending on the operator) the corresponding point in \code{y}.
#' If \code{x} and \code{y} are not of the same length, the shorter object is recycled.
#'
#' \code{match(x, table, nomatch=NA_integer, incomparables=FALSE, ...)} 
#' returns an integer vector of length equal to the number of points in the \linkS4class{SpatialPoints} object \code{x}.
#' Each element of this vector speciffies a point in the \linkS4class{SpatialPoints} object \code{table} 
#' that is equal to the corresponding point in \code{x}.
#'
#' Points are compared to each other by considering the vectors of their coordinates.
#' If all coordinates are exactly equal, so are the points;
#' the first unequal coordinate is used to determine which point is \dQuote{greater than} or \dQuote{less than} the other.
#'
#' @author Aaron Lun
#'
#' @examples
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
#' @docType methods
#' @name comparisons
#' @aliases
#' spatula-comparisons
#' order,SpatialPoints-method
#' sort,SpatialPoints-method
#' unique,SpatialPoints-method
#' duplicated,SpatialPoints-method
#' Ops,SpatialPoints,SpatialPoints-method
#' match,SpatialPoints,SpatialPoints-method
#' %in%,SpatialPoints,SpatialPoints-method
NULL

#' @export
setMethod("order", "SpatialPoints", function(..., na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix")) {
    vectorized <- lapply(list(...), .PointsVector)
    do.call(order, c(vectorized, list(na.last=na.last, decreasing=decreasing, method=match.arg(method))))
})

#' @export
setMethod("sort", "SpatialPoints", function(x, decreasing = FALSE, ...) {
    out <- sort(.PointsVector(x), decreasing=decreasing, ...)
    out@points
})

#' @export
setMethod("unique", "SpatialPoints", function(x, incomparables=FALSE, ...) {
    out <- unique(.PointsVector(x), incomparables=incomparables, ...)
    out@points
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
