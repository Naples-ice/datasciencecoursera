## This pair of functions cache the inverse of a matrix
## and return the inverse

## This function create a special 'Matrix' containing its
## calculated inverse, saved in cache

makeCacheMatrix <- function(x = matrix()) {
        res <- NULL
        set <- function(y){
          x <<- y
          res <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) res <<- solve
        getinverse <- function() res
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the saved inverse in the special 'Matrix',
## if the result hasn't be saved, it calculates the inverse and stores
## the result to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        res <- x$getinverse()
        if(!is.null(res)){
          message("getting chached data!")
         return(res)
        }
        data <- x$get()
        res <- solve(data,...)
        x$setinverse(res)
        res
}
