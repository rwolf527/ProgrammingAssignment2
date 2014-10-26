## Author: Richard Wolf
## Date:   October 26, 2014

## makeCacheMatrix creates a special "matrix", which can cache its inverse.  The implementation of this matrix
## is really a list containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	cachedSolve <- NULL
	set <- function(y) {
		x <<- y
		cachedSolve <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) cachedSolve <<- solve
	getSolve <- function() cachedSolve
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solveResult <- x$getSolve()
        if(!is.null(solveResult)) {
        	message("getting cached data . . .")
        	return(solveResult)
        }
        data <- x$get()
        solveResult = solve(data, ...)
        x$setSolve(solveResult)
        solveResult
}
