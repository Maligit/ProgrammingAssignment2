## Write a short comment describing this function

cacheS <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)){
        	message("getting cached inverse")
        	return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
