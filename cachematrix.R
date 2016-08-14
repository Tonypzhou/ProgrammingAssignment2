## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) m <<- inverse
	get_inverse <- function() m
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
	inv <- x$get_inverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	mat <- x$get()
	m <- solve(mat, ...)
	x$set_inverse(m)
	m
}
