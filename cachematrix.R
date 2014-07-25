## Hi there!
## The following 2 functions, "makeCacheMatrix" and "cacheSolve"
## are meant to calculate and cache the inverse of an invertible
## matrix, and to give an already cached result if such exists.

## The following function is used to create a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) s <<- inverse
	getinverse <- function() s
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function computes and prints the inverse of the
## special "matrix" created with the above function. If the inverse
## has already been calculated, the function prints the cached result.

cacheSolve <- function(x, ...) {
	s <- x$getinverse()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setinverse(s)
	s
}
