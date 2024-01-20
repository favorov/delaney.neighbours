#' @import deldir
#' @import dplyr
#import description end 
0


#' delaunay.neighbours
#' 
#' Delaunay triangulation-based pairs of neighbours
#' 
#' @title delaunay.neighbours: create a list of neighbouring point pairs form the point coordinates
#' @param x could be a data structure that can be subsetted by \code{$x} and \code{$y} 
#' (the names can be altered by parameters #' \code{x.name} and \code{y.name}) or a numeric coordinate vector. In this case, y parameter is required.
#' @return \code{data.frame}, each row is a pair of indices of neighbouring points
#' @examples
#' x <- runif(50)
#' y <- runif(50)
#' df<-data.frame(x=x,y=y)
#' neighb.xy<-delaunay.neighbours(x,y)
#' neighb.df<-delaunay.neighbours(df)
#' dfc<-data.frame(x.coord=x,y.coord=y)
#' neighb.dfc<-delaunay.neighbours(dfc,x.name="x.coord",y.name="y.coord")
#' @export
delaunay.neighbours<-function(x, ...){
  ##get the sides of Delauneu triangles
  #tess<-cwse %>% select(x,y) %>% deldir
  UseMethod("delaunay.neighbours")
}

#' @rdname delaunay.neighbours
#' @param y second coordinate vector
#' @export
delaunay.neighbours.numeric <- function(x,y,...){
  if (class(y) != "numeric") {stop("delaney.neighbours: x is numeric, y is not.\n")}
  if(length(x)!=length(y)) {stop("delaney.neighbours: x and y of different lengths.\n")}
  tesselation<-deldir(x,y)
  neighb_pairs<-tesselation$delsgs[,5:6]
  #add the complement pairs
  neighb_pairs_compl<-neighb_pairs %>% select(ind1=ind2,ind2=ind1)
  neighb_pairs <- neighb_pairs %>% rbind(neighb_pairs_compl)
  neighb_pairs
}

#' @rdname delaunay.neighbours
#' @param x.name the field of column name to subset \code{x$x.name}, the default is "x"
#' @param y.name the field of column name to subset \code{x$y.name}, the default is "y"
#' @export
delaunay.neighbours.default <- function(x,x.name="x",y.name="y",...){
  if(is.atomic(x)) {stop("delaney.neighbours: x is atomic and it is not numeric.\n")}
  if(is.null(x[[x.name]])){ #it is $
    stop(paste0("delaney.neighbours: x$",x_name," is empty.\n"))
  } 
  if(is.null(x[[y.name]])){
    stop(paste0("delaney.neighbours: x$",y_name," is empty.\n"))
  } 
  tesselation<-deldir(x[[x.name]],x[[y.name]])
  neighb_pairs<-tesselation$delsgs[,5:6]
  #add the complement pairs
  neighb_pairs_compl<-neighb_pairs %>% select(ind1=ind2,ind2=ind1)
  neighb_pairs <- neighb_pairs %>% rbind(neighb_pairs_compl)
  neighb_pairs
}

