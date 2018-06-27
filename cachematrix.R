## Write two functions designed to first create an inverted matrix
## and then save the inverted matrix to cache

## the function is designed to save computing time 

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setinvM <- function(inverse) invM <<- inverse
        getinvM <- function() invM
        list(set = set, get = get, setinvM = setinvM, getinvM = getinvM)
}



## The function casheSolve computes the inverse Matrix and retrieves from cache
## if and only if it has not already been calculated or created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getinvM()
        if(!is.null(invM)) {
                message("getting cached result")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setinvM(invM)
        invM
}

#####..........Program testing and check.......

m <- matrix(rnorm(25), 5, 5)
m2 <- makeCacheMatrix(m)
cacheSolve(m2)

