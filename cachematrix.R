## The following functions are used to first cache a matrix and
## then compute the inverse of the matrix. 
## To reduce computing time, the code will check if the inverse
## has already been computed and if not, it will perfom the inversion

## This function makes a "special" matrix where it sets the 
## values of the matrix, gets the matrix values, sets the
## inverse of the matrix, and gets the inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
  
  ## Sets the value of the matrix and creates an empty matrix 
  ## to set the inverse matrix to
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Returns the matrix x and then sets the inverse matrix m
  ## and then returns the inverse matrix m. This then stores
  ## all this information in a list
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function checks to see if the inverse has already
## been computed and if not it will compute the inverse
## before returning the inverse matrix of x

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  ## Checks to see if the inverse has already been computed
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Computes the inverse of the matrix x and returns
  ## the inverse matrix
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
