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
    pl <- .get_polygons(sp)
    for (i in seq_along(pl)) {
        pl[[i]]@ID <- ids[i]
    }
    sp@polygons <- pl
    sp
}

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
#' @importFrom S4Vectors selfmatch
setMethod("xtfrm", ".PolygonsVector", function(x) {
    ref <- .as_spatial(x)
    coords <- .mat2list(coordinates(ref))
    metrics <- c(coords, list(.get_areas(ref)))

    o <- do.call(order, metrics)
    output <- seq_along(o)
    output[o] <- output

    has.ties <- lapply(metrics, function(x) sameAsPreviousROW(x[o]))
    has.ties <- Reduce("&", has.ties)

    if (any(has.ties)) {
        all.polys <- .get_polygons(ref)

        # Iterating through the runs of ties to rearrange them. This could
        # probably be done in a better way with some kind of base64 encoding,
        # but I didn't want to introduce another dependency, so WHATEVER.
        tie.start <- which(!has.ties)
        tie.end <- c(tie.start[-1]-1L, length(has.ties))
        keep <- tie.start!=tie.end

        for (i in which(keep)) {
            curi <- tie.start[i]:tie.end[i]
            curo <- o[curi]
            curp <- all.polys[curo]

            # AW GROSS!
            deparsed <- vapply(curp, FUN=function(x) {
                x <- .wipe_id(x)
                paste(deparse(x), collapse="")
            }, FUN.VALUE="")

            dep.ranks <- rank(deparsed, ties.method="min")
            rank.dex <- selfmatch(dep.ranks)
            output[curo] <- output[curo][rank.dex]
        }
    }

    output
})
