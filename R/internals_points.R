.PointsVector <- function(x) new(".PointsVector", points=x)

#' @importFrom S4Vectors vertical_slot_names
setMethod("vertical_slot_names", ".PointsVector", function(x) {
    c("points", callNextMethod())
})

#' @importFrom sp coordinates
setMethod("coordinates", ".PointsVector", function(obj) {
    coordinates(obj@points)
})

#' @importFrom sp coordinates SpatialPoints
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", ".PointsVector", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    obj.coords <- lapply(objects, coordinates)
    all.coords <- do.call(rbind, c(list(coordinates(x)), obj.coords))
    initialize(x, points=SpatialPoints(all.coords))
})

.mat2list <- function(x) {
    lapply(seq_len(ncol(x)), function(i) x[,i])
}

#' @importFrom S4Vectors sameAsPreviousROW
setMethod("sameAsPreviousROW", ".PointsVector", function(x) {
    out <- .mat2list(coordinates(x))
    collected <- lapply(out, sameAsPreviousROW)
    Reduce("&", collected)
})

#' @importFrom sp coordinates
#' @importFrom S4Vectors order
setMethod("order", ".PointsVector", function(..., na.last = TRUE, 
    decreasing = FALSE, method = c("auto", "shell", "radix")) 
{
    out <- lapply(list(...), FUN=function(x) {
        coords <- coordinates(x)
        .mat2list(coords)
    })
    out <- unlist(out, recursive=FALSE)
    do.call(order, c(out, list(na.last=na.last, decreasing=decreasing, method=match.arg(method))))
})
