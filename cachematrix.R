# This function caches the inverse of a matrix to store the result and save computation resources.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         # resets any previously stored value of "inv"

  set <- function(y) {
     inv <<- NULL     # resets any cached value within environment of this function.
     x <<- y          # saves matrix within environment of the function.
  }

  get <- function() x  # returns matrix

  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse # caches the inverse

  # returns list of functions used to inverse the matrix
  list( set = set, get = get,
   setinverse = setinverse,
   getinverse = getinverse) 
}
 
# This function computes the inverse of a matrix.
# If the inverse has been calculated, retrieves from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # checks for existing inverse, and returns a message if it already exists.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if inverse has not been calculated, this function computes and stores the inverse of the matrix.
  data <- x$get()            # function acquires data in specified matrix.
  inv <- solve(data,...) # solves for the inverse of the matrix.
  x$setinverse(inv)      # caches the inverse.
  inv                    # returns the inverse. 
}
