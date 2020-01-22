makeCacheMatrix<- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x<<- y
            inv<<- NULL
      }
      get<-function() x
      setInv<- function(inv) inv<<- inv
      getInv <- function() inv
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("getting cahced data")
            return(inv)
      }
      data<- x$get()
      inv<- solve(data, ...)
      x$setInv(inv)
      inv
}
