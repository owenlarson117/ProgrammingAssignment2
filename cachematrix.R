## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse. cacheSolve computes the
## inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should return the inverse
## from the cache
makeCacheMatrix<- function(x = matrix()) {
      ## creates matrix object
      inv <- NULL
      set <- function(y) {
            x<<- y
            inv<<- NULL
      }
      get<-function() x
      setInv<- function(inv) inv<<- inv
      getInv <- function() inv
      list(set = set, get = get, setInv = setInv, getInv = getInv)
} ## sets list to retrieve options for cacheSolve

cacheSolve <- function(x, ...) {
      ## retrieves from makeCacheMatrix
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      } ## if cacheSolve has been already used before; 
        ## retrieve data from the cache
      data<- x$get()
      inv<- solve(data, ...)
      x$setInv(inv)
      inv
} ## if otherwise opperate as normal.
