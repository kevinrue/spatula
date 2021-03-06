.mat2list <- function(x) {
    lapply(seq_len(ncol(x)), function(i) x[,i])
}

.as_spatial <- function(pv) pv@spatial

.get_data <- function(sdf) sdf@data

.get_proj4string <- function(s) s@proj4string
