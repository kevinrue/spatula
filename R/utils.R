.mat2list <- function(x) {
    lapply(seq_len(ncol(x)), function(i) x[,i])
}

.as_spatial <- function(pv) pv@spatial

.get_data <- function(sdf) sdf@data

.get_proj4string <- function(s) s@proj4string

.combine_df <- function(ref, others, N) {
    if (!ignore.mcols) {
        data.frame(matrix(0L, N, 0L))
    } else {
        obj.df <- lapply(others, .get_data)
        df <- c(list(.get_data(ref)), obj.df)
        do.call(rbind, df)
    }
}
