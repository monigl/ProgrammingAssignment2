rm(list=ls())


################## Caching the Inverse of a Matrix ##################

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {
  m <- NULL
  set <- function(B) {
    A <<- B
    m <<- NULL
  }
  get <- function() A
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(A, ...) {
  m <- A$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- A$get()
  m <- solve(data, ...)
  A$setinv(m)
  m
}


####### TEST ######

a<-c(4,5,4)       
b<-c(3,4,4)      
d<-c(8,7,7) 

A<-rbind(a,b,d)

myMatrix_object <- makeCacheMatrix(A)
cacheSolve(myMatrix_object)
