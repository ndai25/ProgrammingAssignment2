
##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly

## makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse. which is really a list containing a function to
##1.  set the value of the vector
##2.  get the value of the vector
##3.  set the value of the mean
##4.  get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
	        solution <- NULL
            set <- function(y) {
                    x <<- y
                    solution <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) solution <<- inverse
            getinverse <- function() solution
            list(set = set, get = get,
                 setinverse = setinverse,
     			 getinverse = getinverse)
}


##`cacheSolve`: This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {


            solution <- x$getinverse()
            if(!is.null(solution)) {
                    message("getting cached matrix data")
                    return(solution)
            }
            data <- x$get()
            solution <- solve(data, ...)
            x$setinverse(solution)
            solution
}
