## This file contains two functions that manage matrix data types by caching the matrix inverses. Computing the inverse of a matrix can be
## very compute-heavy, and if a matrix inverse is required repeatedly for a particular matrix, we can cache its inverse after its computed. 
## makeCacheMatrix() function creates a list of helper functions to manage the matrix data, and cacheSolve() function returns the inverse matrix
## from the cache or computes it (if inverse is not cached).


## makeCacheMatrix()
##	creates a special vector of functions that allow setting new matrix data (set), retrieving an already set matrix (get),
##	cache the inverse of the stored matrix (setinverse), and retrieve a cached value of the matrix inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## checks if matrix inverse is stored in the cache. if a value is cached, then the cached value is returned,
## else, the inverse of the matrix is computed, stored in the cache and returned as an inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)){
        	message("getting cached inverse data...")
        	return(i)
        }

        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}