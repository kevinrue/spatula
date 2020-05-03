#' Parallel set operations on polygons
#'
#' Perform set operations such as intersections and unions on two parallel \linkS4class{SpatialPolygons} objects.
#' Requires installation of the \pkg{rgeos} package.
#'
#' @param x,y \linkS4class{SpatialPolygons} objects to be processed in parallel.
#' The shorter object is recycled to the same length as the larger object.
#' 
#' @return A \linkS4class{SpatialPolygons} object representing the intersection, union or difference of the polygons.
#'
#' @details
#' These functions iterate through \code{x} and \code{y} simultaneously to take the intersection, union, etc. 
#' of the corresponding polygons in the two objects.
#'
#' If the intersection is empty, a polygon is returned with zero area and all vertices at the origin.
#' This is necessary because, unfortunately, the \linkS4class{Polygons} constructor does not support empty lists.
#'
#' Lines or points formed by intersections are currently ignored.
#' 
#' @examples
#' poly <- makeSpatialPolygons(
#'     rbind(c(0,0), c(0.5, 0), c(0.5, 0.5), c(0, 0.5)),
#'     rbind(c(1,1), c(0.5, 1), c(0.5, 0.5), c(1, 0.5)),
#'     rbind(c(0.5,0), c(1, 0), c(1, 0.5), c(0.5, 0.5)),
#'     rbind(c(0,1), c(0.5, 1), c(0.5, 0.5), c(0, 0.5))
#' )
#' plot(poly)
#'
#' other.poly <- makeSpatialPolygons(
#'     rbind(c(0.1,0.1), c(0.7, 0.1), c(0.7, 0.7), c(0.1, 0.7))
#' )
#' 
#' intersected <- pintersect(poly, other.poly)
#' plot(intersected[1], xlim=c(0, 1), ylim=c(0, 1))
#'
#' unioned <- punion(poly, other.poly)
#' plot(unioned[1], xlim=c(0, 1), ylim=c(0, 1))
#'
#' diffed <- psetdiff(poly, other.poly)
#' plot(diffed[1], xlim=c(0, 1), ylim=c(0, 1))
#'
#' @author Aaron Lun
#' @name pintersect
NULL

#' @importFrom S4Vectors bindROWS
.ppolys_ops <- function(x, y, FUN) {
    N <- max(length(x), length(y))
    left <- rep(seq_along(x), length.out=N)
    right <- rep(seq_along(y), length.out=N)

    all.polys <- vector("list", N)
    for (i in seq_len(N)) {
        cur.polys <- FUN(x[left[i]], y[right[i]])

        if (is.null(cur.polys)) {
            cur.polys <- makeSpatialPolygons(matrix(0, 4, 2))
        } else if (is(cur.polys, "SpatialCollections")) {
            # NOOO! Can't avoid raw slot accesses!
            cur.polys <- cur.polys@polyobj
        }

        all.polys[[i]] <- cur.polys
    }

    bindROWS(all.polys[[1]], all.polys[-1])
}

#' @export
#' @rdname pintersect
#' @importFrom IRanges pintersect
setMethod("pintersect", c("SpatialPolygons", "SpatialPolygons"), function(x, y) {
    .ppolys_ops(x, y, FUN=rgeos::gIntersection)
})

#' @export
#' @rdname pintersect
#' @importFrom IRanges punion
setMethod("punion", c("SpatialPolygons", "SpatialPolygons"), function(x, y) {
    .ppolys_ops(x, y, FUN=rgeos::gUnion)
})

#' @export
#' @rdname pintersect
#' @importFrom IRanges psetdiff
setMethod("psetdiff", c("SpatialPolygons", "SpatialPolygons"), function(x, y) {
    .ppolys_ops(x, y, FUN=rgeos::gDifference)
})
