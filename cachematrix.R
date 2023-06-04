## Put comments here that give an overall description of what your
## functions do

## Get values of inverse matrix

makeCacheMatrix <- function(x=matrix()){
  inv=NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
}

cacheInverse <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## Return matrix that is the inverse
data <- makeCacheMatrix(matrix(1:4,nrow =2,ncol = 2)){
data$get()
data$getInverse()
cacheInverse(data)
}
