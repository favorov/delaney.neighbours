.onAttach <- function(libname, pkgname) {
  version <- packageDescription("delaunay.neighbours", field="Version")
  packageStartupMessage(paste("Welcome to delaunay.neighbours version", version,"and know the neigbours."))
}