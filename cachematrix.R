## R programming assignment 2 (week 3)
## two functions: the first to create a special matrix object, and the second to get the inverse (trying cache first)

## Create a special matrix object with internal functions to get/set the matrix values and get/set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return the inverse of x, either from cache (if available) or by calculation with solve()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
