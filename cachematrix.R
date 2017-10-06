## This function creates a special "matrix" 
## object that can cache its inverse. It caches the information
## in a list to be recalled later.  

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinv <- function() inv_matrix <<- solve(x)
  getinv <- function() inv_matrix
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by th makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  inv_matrix <- x$getinv()
  if (!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  mat <- x$get()
  inv_matrix <- solve(mat, ...)
  x$setinv(inv_matrix)
  inv_matrix
}
