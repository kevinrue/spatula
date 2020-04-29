# STRICTLY AN INTERNAL CLASS. Purely designed to allow us to plug
# into S4Vectors' automatic methods without having to rewrite them.
setClass(".PointsVector", contains="Vector", slots=c(spatial="SpatialPoints"))

setClass(".PolygonsVector", contains="Vector", slots=c(spatial="SpatialPolygons"))

