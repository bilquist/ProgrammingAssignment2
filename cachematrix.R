## Put comments here that give an overall description of what your
## functions do:
##
## makeCacheMatrix is a list of functions that make a special "matrix" which
## can cache its state and inverse
##
## cacheSolve calculates the inverse of the matrix provided from makeCacheMatrix
## unless the matrix has been solved for, in which event the cached value
## will be returned
##
## Note: these functions do not check to see if the inverse of the provided
## matrix exists


## Write a short comment describing this function
## The functions created within makeCacheMatrix do the following:
##	1. Set the value of the matrix
##	2. Get the value of the matrix
##	3. Set the value of the inverse of the matrix
##	4. Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(y) {
		x <<- y
		inv <<- NULL
	}
	get = function() x
	setInverse = function(inverse) inv <<- inverse
	getInverse = function() inv
	list(set = set, get = get
	    ,setInverse = setInverse, getInverse = getInverse
	    )
}


## Write a short comment describing this function
## If the inverse of the provided makeCacheMatrix has been solved previously,
## this function provides the cached inverse. If it has not, this function
## solves for the inverse and sets the cache value so it does not need
## to be resolved in the future.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv = x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data = x$get()
	inv = solve(data, ...)
	x$setInverse(inv)
	inv
}
