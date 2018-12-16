## Put comments here that give an overall description of what your
## functions do
###the purpose of this writing code assignment is to make two functions that can cache inverse matrix namely 
###makeCacheMatrix and cacheSolve.

## Write a short comment describing this function
#makeCacheMatrix is function that creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.m<-NULL
  set<-function(y){
    x<<-y
    inv.m<<-NULL
  }
  get<-function() x
  set.inv<-function(inverse) inv.<<-mean
  get.inv<-function() inv.m
  list(set=set,get=get,
       set.inv=set.inv,
       get.inv=get.inv)
}


## Write a short comment describing this function
#chacheSolve calculates the inversion of the special matrix. It checks to see if the inverse matrix has been already computed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv.m <- x$get.inv()
  if(!is.null(inv.m)) {
    message("getting cached data")
    return(inv.m)
  }
  data <- x$get()
  inv.m <- solve(data, ...)
  x$set.inv(inv.m)
  inv.m
}

inv.m<-matrix(rnorm(100),5,5)
a<-makeCacheMatrix(inv.m)
cacheSolve(makeCacheMatrix(matrix(rnorm(100),5,5)))

           