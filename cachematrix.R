## Function "makeCacheMatrix" takes a matrix argument and creates a list
## of functions that cache information and support the "cacheSolve" function.
## The list from "makeCacheMatrix" is the argument input into "cacheSolve."

makeCacheMatrix <- function(x = matrix()) {

	# inv holds the cached inverse
	# there is no initial inverse, hence NULL
	inv <- NULL

	# function "set" takes a new matrix, caches it,
	# and removes the cached inverse for the old matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	# function "get" returns the cached matrix
	get <- function() x

	# function "setinv" takes a calculated inverse and caches it
	setinv <- function(inverse) inv <<- inverse

	# function "getinv" returns the cached inverse
	getinv <- function() inv

	# so that "makeCacheMatrix" returns all functions, create a list
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function "cacheSolve" takes the list of functions from "makeCacheMatrix"
## and returns the cached inverse matrix (if available). If the matrix
## inverse is not cached, the inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {

	# read in the cached inverse
	# if the cached inverse exists, return it
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	# otherwise, read in the matrix and invert it
	data <- x$get()
	inv <- solve(data, ...)

	# cache and return the calculated inverse
	x$setinv(inv)
	inv
}