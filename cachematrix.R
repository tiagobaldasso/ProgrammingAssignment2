
## The function below creates a special vector in the form of a list with
## getters and setters for both the matrix supplied (get and set) and a possible 
## cached inverted matrix (getinv and setinv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function below takes as input the list returned in the
## previous function and checks to see if there`s already a cached inverted matrix.
## If there`s already one, it returns the cached matrix. Otherwise, it calculates
## an inverted matrix from scratch and caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverted matrix")
    return(inv)
  }
  mtx <- x$get()
  inv <- solve(mtx)
  x$setinv(inv)
  inv
}
