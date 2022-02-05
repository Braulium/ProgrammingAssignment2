## Two functions to:
## an object containing a matrix and cacheing its inverse
## receiving as an input the output of the first object, and calculating the inverse of the matrix if that is not alreadz in cache, or taking it from the cache if it's already computed

## makeCacheMatrix: given a matrix as an input, will create a list of functions to set and get a matrix, and set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolved <- function(solve) m <<- solve
  getsolved <- function() m
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## cacheSolve: receives the output of makeCacheMatrix, retrieving inverse of the matrix if it has been alreadz calculated, or calculates it if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolved()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolved(m)
  m
}
