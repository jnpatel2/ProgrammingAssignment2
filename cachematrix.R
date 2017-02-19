###########################################################################
# Function : makeCacheMatrix, cacheSolve
#
# These function create special object of Matrix to store matrix and 
# its inverse value, allowing re-use calculated invese value again and
# again, as long as matrix value do not change
###########################################################################


###########################################################################
# Function    : makeCacheMatrix
# Args        : x (matrix)
# Description : Create special matrix, which is list containing a function
#               to set matrix, get matrix, set inverse, get inverse
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvrt <- function(solve) m <<- solve
  getinvrt <- function() m
  list(set = set, get = get,
       setinvrt = setinvrt,
       getinvrt = getinvrt)
}


###########################################################################
# Function    : cacheSolve
# Args        : x (matrix)
# Description : Calculate the inverse of special matrix if it's not already
#               calculated and cached. Also cache this calculated value. 
#               If inverse is already caculated and cached, then get it and 
#               re-use instead of re-calculating it.
###########################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvrt()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvrt(m)
  m
}
