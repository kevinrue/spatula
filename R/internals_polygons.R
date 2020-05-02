.PolygonsVector <- function(x) {
    X <- new(".PolygonsVector", spatial=x)

    # What on earth is going on here? SpatialPolygonsDF
    # gets coerced to a SpatialPolygons by new()!
    X@spatial <- x

    X
}

#' @importFrom S4Vectors vertical_slot_names
setMethod("vertical_slot_names", ".PolygonsVector", function(x) {
    c("spatial", callNextMethod())
})

# UGH! Need raw access to SpatialPolygons' `polygon` and `ID` fields.
# The obvious getter `polygons()` doesn't do what you might expect!
.get_polygons <- function(sp) sp@polygons

.get_areas <- function(sp) {
    vapply(.get_polygons(sp), slot, name="area", 0)
}

.get_ids <- function(sp) {
    vapply(.get_polygons(sp), slot, name="ID", "")
}

.replace_ids <- function(sp, ids) {
    for (i in seq_along(.get_polygons(sp))) {
        sp@polygons[[i]]@ID <- ids[i]
    }
    sp
}

#' @importFrom S4Vectors bindROWS
#' @importFrom sp rbind.SpatialPolygons rbind.SpatialPolygonsDataFrame
setMethod("bindROWS", ".PolygonsVector", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    ref <- .as_spatial(x)
    obj <- lapply(objects, .as_spatial)

    # Hacking our way around the IDs.
    ref.ids <- .get_ids(ref)
    obj.ids <- lapply(obj, .get_ids)
    all.ids <- list(ref.ids, obj.ids)
    new.ids <- relist(make.unique(unlist(all.ids)), all.ids)

    ref <- .replace_ids(ref, new.ids[[1]])
    for (i in seq_along(obj)) {
        obj[[i]] <- .replace_ids(obj[[i]], new.ids[[2]][[i]])
    }

    out <- do.call(rbind, c(list(ref), obj))
    initialize(x, spatial=out)
})

.wipe_id <- function(x) {
    x@ID <- ""
    x
}

#' @importFrom S4Vectors sameAsPreviousROW
setMethod("sameAsPreviousROW", ".PolygonsVector", function(x) {
    ref<- .as_spatial(x)
    out <- .mat2list(coordinates(ref))
    collected <- lapply(out, sameAsPreviousROW)
    tmp <- Reduce("&", collected)

    # A more rigorous check based on exact coordinates 
    # and other attributes (except the name).
    if (any(tmp)) {
        raw <- .get_polygons(ref)
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
    everything <- lapply(list(...), .as_spatial)
    coords.out <- lapply(everything, FUN=function(y) {
        .mat2list(coordinates(y))
    })

    coords.out2 <- unlist(coords.out, recursive=FALSE)
    args <- list(na.last=na.last, decreasing=decreasing, method=match.arg(method))
    o <- do.call(order, c(coords.out2, args))

    # Tie breaking. This could probably be done in a better way
    # but I simply could not be bothered at this point. Very 
    # unlikely that we'll get two polygons with the same centroid.
    has.ties <- lapply(coords.out[[1]], function(x) sameAsPreviousROW(x[o]))
    has.ties <- Reduce("&", has.ties)

    if (any(has.ties)) {
        deparsed <- everything
        for (i in seq_along(everything)) {
            current <- .get_polygons(everything[[i]])
            deparsed[[i]] <- vapply(current, FUN=function(x) {
                x <- .wipe_id(x)
                paste(deparse(x), collapse="")
            }, FUN.VALUE="")
        }

        o <- do.call(order, c(coords.out2, deparsed, args))
    }

    o
})
