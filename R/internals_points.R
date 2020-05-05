.PointsVector <- function(x) new(".PointsVector", spatial=x)

#' @importFrom S4Vectors vertical_slot_names
setMethod("vertical_slot_names", ".PointsVector", function(x) {
    c("spatial", callNextMethod())
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
