## we are going to write a pair of functions "makeCacheMatrix" and "cacheSolve" which compute the inverse of
## a invertible matrix. So below is the explanation how thw functions are formed

## The function "makeCacheMatrix" returns a special "matrix" object that can 
## obtain its inverse for the input which is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## calculates the mean of the data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
