####################################################################################
#  R Programming Assignment 2: Lexical Scoping
#  Megan Jones
#  May 15, 2020 
####################################################################################

##########################################################Install Packages###########################################
# Not Applicable for this assignment if you would like to build on this and utilize packages an example of how to use package manager packman is below
# install.packages("pacman")
# pacman::p_load(pacman,janitor,readr,gmodels,dplyr,tidyr,plyr,ggplot2,hrbrthemes,ggcorrplot,magrittr,pdftools,corrgram)

################################################################ Part 1: makeCacheMatrix ############################
#Function based on example provided @ https://github.com/rdpeng/ProgrammingAssignment2
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

################################################################ Part 2: cacheSolve #################################
#Function based on example provided @ https://github.com/rdpeng/ProgrammingAssignment2
cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

