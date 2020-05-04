#' Expand points to near-circles
#'
#' Expand points to circumscribed polygons to approximate circles of a given radius.
#'
#' @param x A two-dimensional \linkS4class{SpatialPoints} object, where each point is used as the center of a circle.
#' @param radius A numeric vector of length equal to \code{x}, specifying the radius for each circle.
#' Shorter vectors are recycled as necessary.
#' @param resolution Integer scalar indicating the number of vertices of the circumscribed polygon.
#'
#' @return A \linkS4class{SpatialPolygons} object containing the circumscribed polygons.
#'
#' @details
#' On occasion, it is helpful to convert points into circles around those points,
#' e.g., to check for overlaps of a minimum distance or overlap.
#' This function uses circumscribed polygons to approximate circles to the desired resolution,
#' given that \pkg{sp} does not (seem to?) have a natural way of representing pure circles.
#' 
#' @author Aaron Lun
#' @examples
#' coords <- matrix(runif(30), nrow=10)
#' X <- sp::SpatialPoints(coords)
#' Y <- expandPoints(X, runif(10))
#' plot(Y)
#' 
#' @seealso
#' \code{\link{makeSpatialPolygons}}, for more general construction of \linkS4class{SpatialPolygons} objects.
#'
#' @export
#' @importFrom utils head
#' @importFrom sp coordinates
expandPoints <- function(x, radius, resolution=20) {
    radius <- rep(radius, length.out=length(x))
    angles <- seq(0, 2*pi, length.out=(resolution+1))
    angles <- head(angles, -1)
    X <- sin(angles) 
    Y <- cos(angles)

    coords <- coordinates(x)
    all.circles <- vector("list", length(x))
    for (i in seq_along(x)) {
        all.circles[[i]] <- cbind(X * radius[i] + coords[i,1], Y * radius[i] + coords[i,2])
    }

    do.call(makeSpatialPolygons, all.circles)
}
