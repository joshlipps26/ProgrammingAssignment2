## allows for the caching of matrix inverse values to speed up computation of 
## repeated function calls. inverse of matrix is calculated using the
## solve() function

makeCacheMatrix <- function(x = matrix()) {
  
  # default value for cached inverse
  inv <- NULL
  
  # declare setters / getters for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # declare setters / getters for inverse matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  # create list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## if inverse hasnt been calculated before, calculate
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        ## if inverse has been calculated before, retrieve cached value
        inv <- solve(x$get())
        x$setinv(inv)
        inv
}
