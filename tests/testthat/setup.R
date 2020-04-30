library(sp)

# SpatialPoints:
coords1 <- matrix(runif(30), nrow=10)
points1 <- SpatialPoints(coords1)
                                                          
coords2 <- matrix(runif(15), nrow=5)
points2 <- SpatialPoints(coords2)

# SpatialPointsDataFrame:
pointsdf1 <- SpatialPointsDataFrame(coords1, data.frame(X=head(letters, nrow(coords1))))
pointsdf2 <- SpatialPointsDataFrame(coords2, data.frame(X=tail(LETTERS, nrow(coords2))))

# SpatialPolygons:
polys1 <- makeSpatialPolygons(
    matrix(runif(10), ncol=2),
    matrix(rnorm(8), ncol=2),
    matrix(rnorm(20), ncol=2),
    matrix(rnorm(16), ncol=2)
)

polys2 <- makeSpatialPolygons(
    rbind(c(0,0), c(0.5, 0.2), c(0.2, 0.6), c(0.9, 0.8)),
    rbind(c(1,1), c(0.3, 0.7), c(0.1, 0.2), c(0.8, 0.3))
)

# SpatialPolygonsDataFrame:
polysdf1 <- SpatialPolygonsDataFrame(polys1, data.frame(X=head(letters, length(polys1))))
polysdf2 <- SpatialPolygonsDataFrame(polys2, data.frame(X=tail(LETTERS, length(polys2))))
