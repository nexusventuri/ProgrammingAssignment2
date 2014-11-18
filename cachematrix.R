## Caching the inverse of a Matrix
## Matrix inversion is usually an expensive calculation, we want to cache the result to make calculation faster

## There are two functions provided: makeCacheMatrix and cacheSolve. 
## - makeCacheMatrix will create the cache matrix 
## - cacheSolve will compute the inverse of the matrix only the first time is requested


## makeCacheMatrix will return a special matrix which is really a list
## containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - get the value of the inverse
## - set the cached value of the inverse

makeCacheMatrix <- function(data = matrix()) {
	inverse = NULL

	set = function(value) {
		data <<- value
		inverse <<- NULL
	}
	get = function() data 

	setinverse = function(inv) inverse = inv
	getinverse = function() inverse

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse for a special cache matrix
## If the cache matrix contains the value will return it
## Otherwise it computes it on the fly

cacheSolve <- function(cmatrix, ...) {
	inverse = cmatrix$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	data = cmatrix$get()
	inverse = solve(data, ...)

	cmatrix$setinverse(inverse)
	inverse
}
