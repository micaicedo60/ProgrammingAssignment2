## This is my work for the assignment in week 3
##
## I did a simple adaptation of the codes given in the examples
## Just changed x 0 numeric for x = matrix, and the function call mean 
## for solve. Also changed the names of setmean and getmean for setinv (inv for inverse) 
## and getinv
##
##	Mario I. Caicedo

# MakeCacheMatrix creates a list of "places" in which to look
# for inverse of the matrix to invert (letś say x)

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        	set <- function(y) {
                	x <<- y
               	 m <<- NULL
        	}
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}	

## This function reads  the list (output from MakeCacheMatrix)
## and returns the inverse (x^{-1}) of x 

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...) {
        	m <- x$getinv()
        	if(!is.null(m)) {
                	message("getting cached data")
                	return(m)
        	}
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

