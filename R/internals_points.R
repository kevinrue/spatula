.PointsVector <- function(x) new(".PointsVector", spatial=x)

#' @importFrom S4Vectors vertical_slot_names
setMethod("vertical_slot_names", ".PointsVector", function(x) {
    c("spatial", callNextMethod())
})

#' @importFrom S4Vectors bindROWS
#' @importFrom sp coordinates SpatialPoints SpatialPointsDataFrame
setMethod("bindROWS", ".PointsVector", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    ref <- .as_spatial(x)
    others <- lapply(objects, .as_spatial)
    obj.coords <- lapply(others, coordinates)
    all.coords <- do.call(rbind, c(list(coordinates(ref)), obj.coords))

    if (is(ref, "SpatialPointsDataFrame")) {
        df <- .combine_df(ref, others, nrow(all.coords), ignore.mcols=ignore.mcols)
        pointers <- SpatialPointsDataFrame(all.coords, df, proj4string=.get_proj4string(ref))
    } else {
        pointers <- SpatialPoints(all.coords, proj4string=.get_proj4string(ref))
    }

    initialize(x, spatial=pointers)
})

#' @importFrom S4Vectors sameAsPreviousROW
setMethod("sameAsPreviousROW", ".PointsVector", function(x) {
    out <- .mat2list(coordinates(.as_spatial(x)))
    collected <- lapply(out, sameAsPreviousROW)
    Reduce("&", collected)
})

#' @importFrom sp coordinates
#' @importFrom S4Vectors order
setMethod("order", ".PointsVector", function(..., na.last = TRUE, 
    decreasing = FALSE, method = c("auto", "shell", "radix")) 
{
    out <- lapply(list(...), FUN=function(x) {
        .mat2list(coordinates(.as_spatial(x)))
    })
    out <- unlist(out, recursive=FALSE)
    do.call(order, c(out, list(na.last=na.last, decreasing=decreasing, method=match.arg(method))))
})
