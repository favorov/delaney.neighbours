#' @import deldir
#' @import dplyr
#import description end 
0


#' delaunay.neighbours
#' 
#' Delaunay triangulation-based pairs of neighbours
#' @param points a data structure that has fields \code{points$x} and \code{points$y} with pairs 
#' of coordinates of the points; if one or both are absent, or the parameter in \code{NULL} (default),
#' the coordinates as to be passed as \code{x} and \code{y} coordinate vectors
#' @param x,y coordinate vectors
#' @return \code{data.frame}, each row is a pair of indices of neighbouring points
#' @export
delaunay.neighbours<-function(points=NULL,x=NULL,y=NULL){
  ##get the sides of Delauneu triangles
  #tess<-cwse %>% select(x,y) %>% deldir
  if(is.null(points$x)){
    if(is.null(x)) {
      stop("delaney.neighbours: empty x.\n")
    }
  } else {x<-points$x}
  if(is.null(points$y)){
    if(is.null(y)) {
      stop("delaney.neighbours: empty y.\n")
    }
  } else {y<-points$y}
  if(length(x)!=length(y)) {stop("delaney.neighbours: x and y of different lengths.\n")}
  
  tesselation<-deldir(x,y)
  neighb_pairs<-tesselation$delsgs[,5:6]
  #add the complement pairs
  neighb_pairs_compl<-neighb_pairs %>% select(ind1=ind2,ind2=ind1)
  neighb_pairs <- neighb_pairs %>% rbind(neighb_pairs_compl)
  neighb_pairs
}