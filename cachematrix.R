## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix creates a special list containing a function to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverted matrix
##   get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of
## the data with the R solve function and sets the value of the 
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
