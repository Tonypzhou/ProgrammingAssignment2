## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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
