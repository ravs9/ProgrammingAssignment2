## Calculate matrix inversion, with efficiency through by caching the inverse
## if it has already been computed (and the matrix has not changed since then).
##
## The program is composed of two functions - makeCacheMatrix which holds manages the cache,
## and cacheSolve which performs the inversion.

## makeCacheMatrix takes a matrix as input and provides methods
## to set and get the matrix as well as its inverse.
makeCacheMatrix <- function(the_matrix = matrix()) {
	# The cached inverse of the input matrix
	matrix_inverse <- NULL

	# Set (modify) the input matrix
	set <- function(mtrx) {
		matrix_inverse <<- NULL
		the_matrix <<- mtrx
	}

	# Get the input matrix
	get <- function() the_matrix

	# Set the inverse
	setInverse <- function(inv) matrix_inverse <<- inv

	# Get the inverse
	getInverse <- function() matrix_inverse

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve tries to retrieve the cached inverse of the matrix. If it is not already cached,
## cacheSolve will solve for the inverse and cache it.
##
## Returns the inverse of the original matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("Getting cached data.")
		return(inv)
	}

	mtrx <- x$get()
	inv <- solve(mtrx)
	x$setInverse(inv)
	inv
}
