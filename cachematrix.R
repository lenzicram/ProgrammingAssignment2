## This code contains the functions makeCacheMatrix and 
## cacheSolve. This is an exercise on lexical looping and 
## invertible matrices


## makeCacheMatrix is a function that creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
         m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix <- function() m
  list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)

}


## cacheSolve is a function that computes the inverse of the 
## special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x=matrix(), ...) {
         m <- x$getmatrix()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
