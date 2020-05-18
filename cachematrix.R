# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a special “matrix” object that can cache 
# its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
  	set <- function(y) {
 		x <<- y
		i <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) i <<- inverse
  	getinverse <- function() i
  	list(set = set,
      	get = get,
      	setinverse = setinverse,
		getinverse = getinverse)
	}
}

# cacheSolve(): computes the inverse of the “matrix” returned by 
# makeCacheMatrix(). If the inverse has already been calculated 
# and the matrix has not changed, it’ll retrieves the inverse 
# from the cache directly.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}