## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function caches the matrix with null as its inverse
## the inverse is stores in m
## it also defines the get, set, getinverse and setinverse functions

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <-function() x
  setinverse <- function(invmat) m <<- invmat
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve checks the cache if inverse of x is available
## if available it pulls data from cache and saves in m
## if not, it calculates inverse, saves it in m and saves it into cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
