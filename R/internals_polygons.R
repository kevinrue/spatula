.PolygonsVector <- function(x) new(".PolygonsVector", polygons=x)

#' @importFrom S4Vectors vertical_slot_names
setMethod("vertical_slot_names", ".PolygonsVector", function(x) {
    c("polygons", callNextMethod())
})

# UGH! Need raw access to SpatialPolygons' `polygon` field.
# The obvious getter `polygons()` doesn't do what you might expect!
.raw_polygons <- function(x) x@polygons@polygons

#' @importFrom sp coordinates
setMethod("coordinates", ".PolygonsVector", function(obj) {
    coordinates(obj@polygons)
})

.uniquify_names <- function(poly.list) {
    ids <- lapply(poly.list, slot, name="ID")
    if (anyDuplicated(ids)) {
        ids <- make.names(ids, unique=TRUE)
        for (i in seq_along(poly.list)) {
            poly.list[[i]]@ID <- ids[i]
        }
    }
    poly.list
}

#' @importFrom sp polygons 
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", ".PolygonsVector", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    obj.poly <- lapply(objects, .raw_polygons)
    obj.poly <- unlist(obj.poly, recursive=FALSE)
    ref.poly <- .raw_polygons(x)
    all.poly <- c(ref.poly, obj.poly)

    # Checking names.
    all.poly <- .uniquify_names(all.poly)

    # It must be said that the sp getter/setters leave much to be desired.
    initialize(x, polygons=SpatialPolygons(all.poly, proj4string=x@polygons@proj4string)) 
})

.wipe_id <- function(x) {
    x@ID <- ""
    x
}

#' @importFrom S4Vectors sameAsPreviousROW
setMethod("sameAsPreviousROW", ".PolygonsVector", function(x) {
    out <- .mat2list(coordinates(x))
    collected <- lapply(out, sameAsPreviousROW)
    tmp <- Reduce("&", collected)

    # A more rigorous check based on exact coordinates 
    # and other attributes (except the name).
    if (any(tmp)) {
        raw <- .raw_polygons(x)
        for (r in which(tmp)) {
            left <- .wipe_id(raw[[r]])
            right <- .wipe_id(raw[[r-1]])
            tmp[r] <- identical(left, right)
        }
    }

    unname(tmp)
})

#' @importFrom sp coordinates
#' @importFrom S4Vectors order
setMethod("order", ".PolygonsVector", function(..., na.last = TRUE, 
    decreasing = FALSE, method = c("auto", "shell", "radix")) 
{
    everything <- list(...)
    coords.out <- lapply(everything, FUN=function(x) {
        coords <- coordinates(x)
        .mat2list(coords)
    })

    coords.out2 <- unlist(coords.out, recursive=FALSE)
    args <- list(na.last=na.last, decreasing=decreasing, method=match.arg(method))
    o <- do.call(order, c(coords.out2, args))

    # Tie breaking.
    has.ties <- lapply(coords.out[[1]], function(x) sameAsPreviousROW(x[o]))
    has.ties <- Reduce("&", has.ties)

    if (any(has.ties)) {
        deparsed <- everything
        for (i in seq_along(everything)) {
            current <- .raw_polygons(everything[[i]])
            deparsed[[i]] <- vapply(current, FUN=function(x) {
                x <- .wipe_id(x)
                paste(deparse(x), collapse="")
            }, FUN.VALUE="")
        }

        o <- do.call(order, c(coords.out2, deparsed, args))
    }

    o
})
